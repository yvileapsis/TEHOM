#version 450 core

const float GAMMA = 2.2;

struct EyeStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

layout(set = 0, binding = 0) uniform EyeUniform { EyeStruct eye; };

flat layout(location = 0) in vec3 center;
flat layout(location = 1) in vec3 axisX;
flat layout(location = 2) in vec3 axisY;
flat layout(location = 3) in vec3 axisZ;
flat layout(location = 4) in vec4 color;
flat layout(location = 5) in vec4 albedo;
flat layout(location = 6) in vec4 material;
flat layout(location = 7) in vec4 heightPlus;
flat layout(location = 8) in vec4 subsurfacePlus;
flat layout(location = 9) in vec4 clearCoatPlus;
flat layout(location = 10) in vec4 viewport;
flat layout(location = 11) in vec4 clipPlane;

layout(location = 0) out float depthOut;
layout(location = 1) out vec3 albedoOut;
layout(location = 2) out vec4 materialOut;
layout(location = 3) out vec4 normalPlusOut;
layout(location = 4) out vec4 subdermalPlusOut;
layout(location = 5) out vec4 scatterPlusOut;
layout(location = 6) out vec4 clearCoatPlusOut;

vec3 saturate(vec3 rgb, float adjustment)
{
    const vec3 weights = vec3(0.2125, 0.7154, 0.0721);
    return mix(vec3(dot(rgb, weights)), rgb, adjustment);
}

float signNotZero(float value)
{
    return value >= 0.0 ? 1.0 : -1.0;
}

vec2 signNotZero(vec2 value)
{
    return vec2(signNotZero(value.x), signNotZero(value.y));
}

vec2 encodeOctahedral(vec3 value)
{
    vec2 result = value.xy / (abs(value.x) + abs(value.y) + abs(value.z));
    if (value.z < 0.0)
        result = (1.0 - abs(result.yx)) * signNotZero(result.xy);
    return result;
}

vec3 worldPoint(float ndcZ)
{
    vec2 pixel = (gl_FragCoord.xy - viewport.xy) / viewport.zw;
    vec2 ndc = pixel * 2.0 - 1.0;
    vec4 pointView = eye.projectionInverse * vec4(ndc, ndcZ, 1.0);
    pointView /= pointView.w;
    return (eye.viewInverse * pointView).xyz;
}

vec2 intersectBox(vec3 rayOrigin, vec3 rayDirection, vec3 halfSize)
{
    vec3 safeDirection = vec3
    (
        abs(rayDirection.x) < 0.000001 ? (rayDirection.x < 0.0 ? -0.000001 : 0.000001) : rayDirection.x,
        abs(rayDirection.y) < 0.000001 ? (rayDirection.y < 0.0 ? -0.000001 : 0.000001) : rayDirection.y,
        abs(rayDirection.z) < 0.000001 ? (rayDirection.z < 0.0 ? -0.000001 : 0.000001) : rayDirection.z
    );
    vec3 first = (-halfSize - rayOrigin) / safeDirection;
    vec3 second = (halfSize - rayOrigin) / safeDirection;
    vec3 nearHit = min(first, second);
    vec3 farHit = max(first, second);
    return vec2(max(max(nearHit.x, nearHit.y), nearHit.z), min(min(farHit.x, farHit.y), farHit.z));
}

void main()
{
    float alpha = color.a * albedo.a;
    if (alpha <= 0.0)
        discard;

    vec3 nearPoint = worldPoint(0.0);
    vec3 farPoint = worldPoint(1.0);
    bool perspective = abs(eye.projection[3][3]) < 0.5;
    vec3 rayOriginWorld = perspective ? eye.center : nearPoint;
    vec3 rayDirectionWorld = normalize(farPoint - rayOriginWorld);
    vec3 xDirection = normalize(axisX);
    vec3 yDirection = normalize(axisY);
    vec3 zDirection = normalize(axisZ);
    vec3 halfSize = max(vec3(length(axisX), length(axisY), length(axisZ)), vec3(0.000001));
    vec3 centerToRay = rayOriginWorld - center;
    vec3 rayOriginLocal = vec3(dot(centerToRay, xDirection), dot(centerToRay, yDirection), dot(centerToRay, zDirection));
    vec3 rayDirectionLocal = vec3(dot(rayDirectionWorld, xDirection), dot(rayDirectionWorld, yDirection), dot(rayDirectionWorld, zDirection));
    vec2 hit = intersectBox(rayOriginLocal, rayDirectionLocal, halfSize);
    float hitDistance = max(hit.x, 0.0);
    if (hit.y < hitDistance)
        discard;

    vec3 hitLocal = rayOriginLocal + rayDirectionLocal * hitDistance;
    vec3 faceDistance = abs(abs(hitLocal) - halfSize);
    vec3 localNormal =
        faceDistance.x <= faceDistance.y && faceDistance.x <= faceDistance.z ? vec3(signNotZero(hitLocal.x), 0.0, 0.0) :
        faceDistance.y <= faceDistance.z ? vec3(0.0, signNotZero(hitLocal.y), 0.0) :
        vec3(0.0, 0.0, signNotZero(hitLocal.z));
    vec3 faceNormal = normalize(localNormal.x * xDirection + localNormal.y * yDirection + localNormal.z * zDirection);
    vec3 hitWorld = rayOriginWorld + rayDirectionWorld * hitDistance;
    if (dot(clipPlane.xyz, clipPlane.xyz) > 0.0 && dot(vec4(hitWorld, 1.0), clipPlane) < 0.0)
        discard;

    vec4 hitClip = eye.viewProjection * vec4(hitWorld, 1.0);
    float hitDepth = hitClip.z / hitClip.w;
    if (hitDepth < 0.0 || hitDepth > 1.0)
        discard;

    gl_FragDepth = hitDepth;
    depthOut = hitDepth;
    albedoOut = pow(clamp(color.rgb, vec3(0.0), vec3(1.0)), vec3(GAMMA)) * albedo.rgb;
    materialOut = material;
    normalPlusOut = vec4(faceNormal, heightPlus.y);

    float scatterType = subsurfacePlus.g;
    if (scatterType != 0.0)
    {
        subdermalPlusOut = vec4(saturate(albedoOut, 1.5), clamp(subsurfacePlus.r, 0.0, 1.5));
        scatterPlusOut = vec4
        (
            scatterType > 0.09 && scatterType < 0.11 ? vec3(1.0, 0.25, 0.04) : vec3(0.6, 1.0, 0.06),
            scatterType
        );
    }
    else
    {
        subdermalPlusOut = vec4(0.0);
        scatterPlusOut = vec4(0.0);
    }

    if (clearCoatPlus.r > 0.0)
        clearCoatPlusOut = vec4(clearCoatPlus.r, clamp(clearCoatPlus.g, 0.0, 1.0), encodeOctahedral(faceNormal));
    else clearCoatPlusOut = vec4(0.0);
}
