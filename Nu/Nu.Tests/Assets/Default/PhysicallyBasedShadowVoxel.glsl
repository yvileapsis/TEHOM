#shader vertex
#version 460 core

uniform mat4 view;
uniform mat4 projection;
uniform mat4 viewProjection;
uniform vec2 viewPort;
uniform samplerBuffer paletteTexture;

layout(location = 0) in uint splatKey;
layout(location = 3) in mat4 model;
layout(location = 7) in vec4 voxelSize;
layout(location = 12) in vec4 clearCoatPlus;

flat out vec3 centerOut;
flat out vec3 axisXOut;
flat out vec3 axisYOut;
flat out vec3 axisZOut;
flat out vec4 colorOut;

void main()
{
    uint x = splatKey & 63u;
    uint y = (splatKey >> 6) & 63u;
    uint z = (splatKey >> 12) & 63u;
    uint paletteIndex = splatKey >> 18;
    vec3 voxelOrigin = vec3(voxelSize.w, clearCoatPlus.z, clearCoatPlus.w);
    vec3 position = voxelOrigin + vec3(float(x), float(y), float(z)) * voxelSize.xyz;
    vec4 centerWorld = model * vec4(position, 1.0);
    vec3 axisX = model[0].xyz * voxelSize.x * 0.5;
    vec3 axisY = model[1].xyz * voxelSize.y * 0.5;
    vec3 axisZ = model[2].xyz * voxelSize.z * 0.5;
    float radius = length(vec3(length(axisX), length(axisY), length(axisZ)));
    vec4 centerView = view * centerWorld;
    float pointSizePerspective = projection[1][1] * radius * viewPort.y / max(abs(centerView.z), 0.001) * 2.0;
    float pointSizeOrthographic = projection[1][1] * radius * viewPort.y;

    centerOut = centerWorld.xyz;
    axisXOut = axisX;
    axisYOut = axisY;
    axisZOut = axisZ;
    colorOut = texelFetch(paletteTexture, int(paletteIndex));

    gl_PointSize = max(1.0, projection[3][3] == 0.0 ? pointSizePerspective : pointSizeOrthographic);
    gl_Position = viewProjection * centerWorld;
}

#shader fragment
#version 460 core

uniform mat4 view;
uniform mat4 projection;
uniform mat4 viewInverse;
uniform mat4 projectionInverse;
uniform vec2 viewPort;
uniform float lightShadowExponent;

flat in vec3 centerOut;
flat in vec3 axisXOut;
flat in vec3 axisYOut;
flat in vec3 axisZOut;
flat in vec4 colorOut;

layout(location = 0) out vec2 depths;

vec3 worldPoint(float ndcZ)
{
    vec2 ndc = (gl_FragCoord.xy / viewPort) * 2.0 - 1.0;
    vec4 pointView = projectionInverse * vec4(ndc, ndcZ, 1.0);
    pointView /= pointView.w;
    return (viewInverse * pointView).xyz;
}

vec2 intersectBox(vec3 rayOrigin, vec3 rayDirection, vec3 halfSize)
{
    vec3 safeDirection = vec3(
        abs(rayDirection.x) < 0.000001 ? (rayDirection.x < 0.0 ? -0.000001 : 0.000001) : rayDirection.x,
        abs(rayDirection.y) < 0.000001 ? (rayDirection.y < 0.0 ? -0.000001 : 0.000001) : rayDirection.y,
        abs(rayDirection.z) < 0.000001 ? (rayDirection.z < 0.0 ? -0.000001 : 0.000001) : rayDirection.z);
    vec3 t0 = (-halfSize - rayOrigin) / safeDirection;
    vec3 t1 = (halfSize - rayOrigin) / safeDirection;
    vec3 tMin = min(t0, t1);
    vec3 tMax = max(t0, t1);
    return vec2(max(max(tMin.x, tMin.y), tMin.z), min(min(tMax.x, tMax.y), tMax.z));
}

void main()
{
    if (colorOut.a <= 0.0)
        discard;

    vec3 rayOriginWorld = worldPoint(-1.0);
    vec3 rayEndWorld = worldPoint(1.0);
    vec3 rayDirectionWorld = normalize(rayEndWorld - rayOriginWorld);
    vec3 xDir = normalize(axisXOut);
    vec3 yDir = normalize(axisYOut);
    vec3 zDir = normalize(axisZOut);
    vec3 halfSize = max(vec3(length(axisXOut), length(axisYOut), length(axisZOut)), vec3(0.000001));
    vec3 centerToRay = rayOriginWorld - centerOut;
    vec3 rayOriginLocal = vec3(dot(centerToRay, xDir), dot(centerToRay, yDir), dot(centerToRay, zDir));
    vec3 rayDirectionLocal = vec3(dot(rayDirectionWorld, xDir), dot(rayDirectionWorld, yDir), dot(rayDirectionWorld, zDir));
    vec2 hit = intersectBox(rayOriginLocal, rayDirectionLocal, halfSize);
    float hitDistance = max(hit.x, 0.0);
    if (hit.y < hitDistance)
        discard;

    vec3 hitWorld = rayOriginWorld + rayDirectionWorld * hitDistance;
    vec4 hitClip = projection * view * vec4(hitWorld, 1.0);
    float hitDepth = hitClip.z / hitClip.w * 0.5 + 0.5;
    if (hitDepth < 0.0 || hitDepth > 1.0)
        discard;

    gl_FragDepth = hitDepth;
    depths.x = hitDepth;
    depths.y = exp(lightShadowExponent * hitDepth);
}
