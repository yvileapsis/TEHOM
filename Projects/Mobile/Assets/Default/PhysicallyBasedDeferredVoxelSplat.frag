#version 450 core

struct VoxelSplatInstanceStruct
{
    mat4 modelViewProjection;
    vec4 voxelOrigin;
    vec4 voxelSize;
    vec4 albedo;
    vec4 material;
    vec4 heightPlus;
    vec4 subsurfacePlus;
    vec4 clearCoatPlus;
    vec4 normalX;
    vec4 normalY;
    vec4 normalZ;
    vec4 clipPlaneLocal;
    vec4 rayOriginBase;
    vec4 rayOriginU;
    vec4 rayOriginV;
    vec4 rayDirectionBase;
    vec4 rayDirectionU;
    vec4 rayDirectionV;
    vec4 cameraLocal;
    vec4 proxyParams;
};

flat layout(location = 0) in vec4 color;
flat layout(location = 1) in vec4 albedo;
flat layout(location = 2) in vec4 material;
flat layout(location = 3) in vec4 heightPlus;
flat layout(location = 4) in vec4 subsurfacePlus;
flat layout(location = 5) in vec4 clearCoatPlus;
flat layout(location = 6) in vec3 voxelCenter;
flat layout(location = 7) in uint faces;
noperspective layout(location = 8) in vec3 rayOrigin;
noperspective layout(location = 9) in vec3 rayDirection;

layout(set = 2, binding = 0) uniform InstanceUniform { VoxelSplatInstanceStruct instance; };

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

float safeInverse(float value)
{
    return value == 0.0 ? 1.0e12 : 1.0 / value;
}

vec3 safeInverse(vec3 value)
{
    return vec3(safeInverse(value.x), safeInverse(value.y), safeInverse(value.z));
}

uint faceBit(int axis, float normalSign)
{
    if (axis == 0) return normalSign > 0.0 ? 1u : 2u;
    if (axis == 1) return normalSign > 0.0 ? 4u : 8u;
    return normalSign < 0.0 ? 16u : 32u;
}

bool intersectVoxelBox(out float distance, out vec3 normal)
{
    vec3 boxRadius = instance.voxelSize.xyz * 0.5;
    vec3 origin = rayOrigin - voxelCenter;
    float winding = all(lessThan(abs(origin), boxRadius)) ? -1.0 : 1.0;
    vec3 directionSign = -sign(rayDirection);
    vec3 outwardSign = directionSign * winding;
    vec3 distanceToPlane =
        (boxRadius * winding * directionSign - origin) * safeInverse(rayDirection);

    bool testX =
        distanceToPlane.x >= 0.0 &&
        all(lessThanEqual(abs(origin.yz + rayDirection.yz * distanceToPlane.x), boxRadius.yz)) &&
        (faces & faceBit(0, outwardSign.x)) != 0u;
    bool testY =
        distanceToPlane.y >= 0.0 &&
        all(lessThanEqual(abs(origin.zx + rayDirection.zx * distanceToPlane.y), boxRadius.zx)) &&
        (faces & faceBit(1, outwardSign.y)) != 0u;
    bool testZ =
        distanceToPlane.z >= 0.0 &&
        all(lessThanEqual(abs(origin.xy + rayDirection.xy * distanceToPlane.z), boxRadius.xy)) &&
        (faces & faceBit(2, outwardSign.z)) != 0u;

    normal =
        testX ? vec3(outwardSign.x, 0.0, 0.0) :
        testY ? vec3(0.0, outwardSign.y, 0.0) :
        vec3(0.0, 0.0, testZ ? outwardSign.z : 0.0);
    distance = testX ? distanceToPlane.x : testY ? distanceToPlane.y : distanceToPlane.z;
    return testX || testY || testZ;
}

void main()
{
    float hitDistance;
    vec3 localNormal;
    if (!intersectVoxelBox(hitDistance, localNormal))
        discard;

    vec3 hitLocal = rayOrigin + rayDirection * hitDistance;
    if (dot(vec4(hitLocal, 1.0), instance.clipPlaneLocal) < 0.0)
        discard;

    vec4 hitClip = instance.modelViewProjection * vec4(hitLocal, 1.0);
    if (hitClip.w <= 0.0)
        discard;
    float hitDepth = hitClip.z / hitClip.w;
    if (hitDepth < 0.0 || hitDepth > 1.0)
        discard;

    float depthCutoff = heightPlus.z;
    if (depthCutoff >= 0.0)
    {
        if (hitClip.z > depthCutoff) discard;
    }
    else if (hitClip.z <= -depthCutoff) discard;

    float alpha = color.a * albedo.a;
    if (alpha <= 0.0)
        discard;

    vec3 faceNormal =
        localNormal.x != 0.0 ? instance.normalX.xyz * localNormal.x :
        localNormal.y != 0.0 ? instance.normalY.xyz * localNormal.y :
        instance.normalZ.xyz * localNormal.z;

    gl_FragDepth = hitDepth;
    depthOut = hitDepth;
    albedoOut = color.rgb * albedo.rgb;
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
