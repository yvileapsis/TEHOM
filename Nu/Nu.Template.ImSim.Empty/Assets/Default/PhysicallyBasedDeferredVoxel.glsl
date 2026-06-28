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
layout(location = 8) in vec4 albedo;
layout(location = 9) in vec4 material;
layout(location = 10) in vec4 heightPlus;
layout(location = 11) in vec4 subsurfacePlus;
layout(location = 12) in vec4 clearCoatPlus;

flat out vec3 centerOut;
flat out vec3 axisXOut;
flat out vec3 axisYOut;
flat out vec3 axisZOut;
flat out vec4 colorOut;
flat out vec4 albedoOut;
flat out vec4 materialOut;
flat out vec4 heightPlusOut;
flat out vec4 subsurfacePlusOut;
flat out vec4 clearCoatPlusOut;

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
    albedoOut = albedo;
    materialOut = material;
    heightPlusOut = heightPlus;
    subsurfacePlusOut = subsurfacePlus;
    clearCoatPlusOut = clearCoatPlus;

    gl_PointSize = max(1.0, projection[3][3] == 0.0 ? pointSizePerspective : pointSizeOrthographic);
    gl_Position = viewProjection * centerWorld;
}

#shader fragment
#version 460 core

const float GAMMA = 2.2;

uniform mat4 view;
uniform mat4 projection;
uniform mat4 viewInverse;
uniform mat4 projectionInverse;
uniform vec2 viewPort;
uniform vec4 clipPlane;

flat in vec3 centerOut;
flat in vec3 axisXOut;
flat in vec3 axisYOut;
flat in vec3 axisZOut;
flat in vec4 colorOut;
flat in vec4 albedoOut;
flat in vec4 materialOut;
flat in vec4 heightPlusOut;
flat in vec4 subsurfacePlusOut;
flat in vec4 clearCoatPlusOut;

layout(location = 0) out float depth;
layout(location = 1) out vec3 albedo;
layout(location = 2) out vec4 material;
layout(location = 3) out vec4 normalPlus;
layout(location = 4) out vec4 subdermalPlus;
layout(location = 5) out vec4 scatterPlus;
layout(location = 6) out vec4 clearCoatPlus;

vec3 saturate(vec3 rgb, float adjustment)
{
    const vec3 w = vec3(0.2125, 0.7154, 0.0721);
    vec3 intensity = vec3(dot(rgb, w));
    return mix(intensity, rgb, adjustment);
}

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
    float alpha = colorOut.a * albedoOut.a;
    if (alpha <= 0.0)
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

    vec3 hitLocal = rayOriginLocal + rayDirectionLocal * hitDistance;
    vec3 faceDistance = abs(abs(hitLocal) - halfSize);
    vec3 localNormal =
        faceDistance.x <= faceDistance.y && faceDistance.x <= faceDistance.z ? vec3(hitLocal.x < 0.0 ? -1.0 : 1.0, 0.0, 0.0) :
        faceDistance.y <= faceDistance.z ? vec3(0.0, hitLocal.y < 0.0 ? -1.0 : 1.0, 0.0) :
        vec3(0.0, 0.0, hitLocal.z < 0.0 ? -1.0 : 1.0);
    vec3 faceNormal = normalize(localNormal.x * xDir + localNormal.y * yDir + localNormal.z * zDir);

    vec3 hitWorld = rayOriginWorld + rayDirectionWorld * hitDistance;
    if (dot(clipPlane.xyz, clipPlane.xyz) > 0.0 && dot(vec4(hitWorld, 1.0), clipPlane) < 0.0)
        discard;
    vec4 hitClip = projection * view * vec4(hitWorld, 1.0);
    float hitDepth = hitClip.z / hitClip.w * 0.5 + 0.5;
    if (hitDepth < 0.0 || hitDepth > 1.0)
        discard;

    gl_FragDepth = hitDepth;
    depth = hitDepth;
    albedo = pow(clamp(colorOut.rgb * albedoOut.rgb, vec3(0.0), vec3(1.0)), vec3(GAMMA));
    material = materialOut;
    normalPlus.xyz = faceNormal;
    normalPlus.w = heightPlusOut.y;

    float scatterType = subsurfacePlusOut.g;
    if (scatterType != 0.0)
    {
        subdermalPlus.rgb = saturate(albedo, 1.5);
        subdermalPlus.a = clamp(subsurfacePlusOut.r, 0.0, 1.5);
        scatterPlus.rgb =
            scatterType > 0.09 && scatterType < 0.11 ?
            vec3(1.0, 0.25, 0.04) :
            vec3(0.6, 1.0, 0.06);
        scatterPlus.a = scatterType;
    }
    else
    {
        subdermalPlus = vec4(0.0);
        scatterPlus = vec4(0.0);
    }

    clearCoatPlus.r = clearCoatPlusOut.r;
    clearCoatPlus.g = clearCoatPlusOut.g;
    clearCoatPlus.ba = vec2(0.0);
}
