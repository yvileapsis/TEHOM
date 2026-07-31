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

layout(std430, set = 1, binding = 0) readonly buffer SplatBuffer { uint splats[]; };
layout(std430, set = 1, binding = 2) readonly buffer PaletteBuffer { vec4 palette[]; };
layout(set = 2, binding = 0) uniform InstanceUniform { VoxelSplatInstanceStruct instance; };

flat layout(location = 0) out vec4 colorOut;
flat layout(location = 1) out vec4 albedoOut;
flat layout(location = 2) out vec4 materialOut;
flat layout(location = 3) out vec4 heightPlusOut;
flat layout(location = 4) out vec4 subsurfacePlusOut;
flat layout(location = 5) out vec4 clearCoatPlusOut;
flat layout(location = 6) out vec3 voxelCenterOut;
flat layout(location = 7) out uint facesOut;
noperspective layout(location = 8) out vec3 rayOriginOut;
noperspective layout(location = 9) out vec3 rayDirectionOut;

const vec2 QUAD_CORNERS[4] = vec2[4]
(
    vec2(-1.0, -1.0),
    vec2( 1.0, -1.0),
    vec2( 1.0,  1.0),
    vec2(-1.0,  1.0)
);

const vec3 CUBE_CORNERS[8] = vec3[8]
(
    vec3(-1.0, -1.0, -1.0),
    vec3( 1.0, -1.0, -1.0),
    vec3( 1.0,  1.0, -1.0),
    vec3(-1.0,  1.0, -1.0),
    vec3(-1.0, -1.0,  1.0),
    vec3( 1.0, -1.0,  1.0),
    vec3( 1.0,  1.0,  1.0),
    vec3(-1.0,  1.0,  1.0)
);

const uvec2 CUBE_EDGES[12] = uvec2[12]
(
    uvec2(0, 1), uvec2(1, 2), uvec2(2, 3), uvec2(3, 0),
    uvec2(4, 5), uvec2(5, 6), uvec2(6, 7), uvec2(7, 4),
    uvec2(0, 4), uvec2(1, 5), uvec2(2, 6), uvec2(3, 7)
);

uint stableHash(uint value)
{
    value ^= value >> 16;
    value *= 0x7feb352du;
    value ^= value >> 15;
    value *= 0x846ca68bu;
    return value ^ (value >> 16);
}

void includeProjected(vec4 clipPosition, inout vec2 boundsMin, inout vec2 boundsMax)
{
    vec2 projected = clipPosition.xy / clipPosition.w;
    boundsMin = min(boundsMin, projected);
    boundsMax = max(boundsMax, projected);
}

bool projectClippedBoxBounds(vec4 clipCorners[8], out vec2 boundsMin, out vec2 boundsMax)
{
    boundsMin = vec2(1.0e30);
    boundsMax = vec2(-1.0e30);
    bool found = false;
    for (int index = 0; index < 8; ++index)
    {
        vec4 corner = clipCorners[index];
        if (corner.z >= 0.0 && corner.w > 0.0)
        {
            includeProjected(corner, boundsMin, boundsMax);
            found = true;
        }
    }
    for (int edgeIndex = 0; edgeIndex < 12; ++edgeIndex)
    {
        vec4 first = clipCorners[CUBE_EDGES[edgeIndex].x];
        vec4 second = clipCorners[CUBE_EDGES[edgeIndex].y];
        bool firstInside = first.z >= 0.0;
        bool secondInside = second.z >= 0.0;
        if (firstInside != secondInside)
        {
            float amount = first.z / (first.z - second.z);
            vec4 clipped = mix(first, second, amount);
            if (clipped.w > 0.0)
            {
                includeProjected(clipped, boundsMin, boundsMax);
                found = true;
            }
        }
    }
    boundsMin = clamp(boundsMin, vec2(-1.0), vec2(1.0));
    boundsMax = clamp(boundsMax, vec2(-1.0), vec2(1.0));
    return found && all(greaterThan(boundsMax, boundsMin));
}

bool projectSphereBounds(vec3 centerLocal, float sphereRadius, out vec2 boundsMin, out vec2 boundsMax)
{
    const vec4 quadricDiagonal = vec4(1.0, 1.0, 1.0, -1.0);
    vec4 sphereCenter = vec4(centerLocal, 1.0);
    mat4 modelViewProjection = transpose(instance.modelViewProjection);
    mat3x4 transformed = mat3x4
    (
        mat3
        (
            modelViewProjection[0].xyz,
            modelViewProjection[1].xyz,
            modelViewProjection[3].xyz
        ) * sphereRadius
    );
    transformed[0].w = dot(sphereCenter, modelViewProjection[0]);
    transformed[1].w = dot(sphereCenter, modelViewProjection[1]);
    transformed[2].w = dot(sphereCenter, modelViewProjection[3]);
    mat3x4 dual = mat3x4
    (
        transformed[0] * quadricDiagonal,
        transformed[1] * quadricDiagonal,
        transformed[2] * quadricDiagonal
    );
    float denominator = dot(dual[2], transformed[2]);
    vec4 coefficients = vec4
    (
        dot(dual[0], transformed[2]),
        dot(dual[1], transformed[2]),
        dot(dual[0], transformed[0]),
        dot(dual[1], transformed[1])
    ) / denominator;
    vec2 radicand = coefficients.xy * coefficients.xy - coefficients.zw;
    if (abs(denominator) < 0.0000001 || any(lessThan(radicand, vec2(0.0))) || any(isnan(coefficients)) || any(isinf(coefficients)))
        return false;
    vec2 radius = sqrt(radicand);
    boundsMin = coefficients.xy - radius;
    boundsMax = coefficients.xy + radius;
    return all(greaterThan(boundsMax, boundsMin));
}

void main()
{
    uint packedPosition;
    uint x;
    uint y;
    uint z;
    uint faces;
    uint paletteIndex;
    if (instance.proxyParams.w > 0.0)
    {
        packedPosition = splats[gl_InstanceIndex];
        x = packedPosition & 63u;
        y = (packedPosition >> 6) & 63u;
        z = (packedPosition >> 12) & 63u;
        faces = (packedPosition >> 18) & 63u;
        paletteIndex = packedPosition >> 24;
    }
    else
    {
        packedPosition = splats[gl_InstanceIndex * 2];
        uint packedAttributes = splats[gl_InstanceIndex * 2 + 1];
        x = packedPosition & 1023u;
        y = (packedPosition >> 10) & 1023u;
        z = (packedPosition >> 20) & 1023u;
        faces = packedAttributes & 63u;
        paletteIndex = packedAttributes >> 6;
    }

    vec3 centerLocal = instance.voxelOrigin.xyz + vec3(float(x), float(y), float(z)) * instance.voxelSize.xyz;
    vec3 halfVoxel = instance.voxelSize.xyz * 0.5;
    vec3 cameraOffset = instance.cameraLocal.xyz - centerLocal;
    bool cameraInside = all(lessThanEqual(abs(cameraOffset), halfVoxel));
    uint cameraFacingFaces =
        (cameraOffset.x >= 0.0 ? 1u : 2u) |
        (cameraOffset.y >= 0.0 ? 4u : 8u) |
        (cameraOffset.z <= 0.0 ? 16u : 32u);

    vec2 quadCorner = QUAD_CORNERS[gl_VertexIndex];
    vec2 vertexNdc = quadCorner;
    bool culled = !cameraInside && (faces & cameraFacingFaces) == 0u;
    if (!culled)
    {
        vec4 centerClip = instance.modelViewProjection * vec4(centerLocal, 1.0);
        vec4 extentX = instance.modelViewProjection * vec4(halfVoxel.x, 0.0, 0.0, 0.0);
        vec4 extentY = instance.modelViewProjection * vec4(0.0, halfVoxel.y, 0.0, 0.0);
        vec4 extentZ = instance.modelViewProjection * vec4(0.0, 0.0, halfVoxel.z, 0.0);
        vec4 clipCorners[8];
        float minimumW = 1.0e30;
        float maximumW = -1.0e30;
        float minimumZ = 1.0e30;
        for (int cornerIndex = 0; cornerIndex < 8; ++cornerIndex)
        {
            vec3 corner = CUBE_CORNERS[cornerIndex];
            vec4 clipCorner = centerClip + corner.x * extentX + corner.y * extentY + corner.z * extentZ;
            clipCorners[cornerIndex] = clipCorner;
            minimumW = min(minimumW, clipCorner.w);
            maximumW = max(maximumW, clipCorner.w);
            minimumZ = min(minimumZ, clipCorner.z);
        }

        if (maximumW <= 0.00001)
        {
            culled = true;
        }
        else if (minimumW <= 0.00001)
        {
            gl_Position = vec4(quadCorner, 0.0, 1.0);
        }
        else
        {
            vec2 boundsMin;
            vec2 boundsMax;
            bool boundsValid = projectSphereBounds(centerLocal, length(halfVoxel), boundsMin, boundsMax);
            vec2 resolution = 1.0 / instance.proxyParams.xy;
            vec2 projectedPixelSize = boundsValid ? (boundsMax - boundsMin) * resolution * 0.5 : vec2(1.0e30);
            bool useClippedBoxBounds =
                minimumZ <= 0.0 ||
                !boundsValid ||
                max(projectedPixelSize.x, projectedPixelSize.y) > 20.0;
            if (useClippedBoxBounds)
                boundsValid = projectClippedBoxBounds(clipCorners, boundsMin, boundsMax);
            else
            {
                boundsMin = clamp(boundsMin, vec2(-1.0), vec2(1.0));
                boundsMax = clamp(boundsMax, vec2(-1.0), vec2(1.0));
                boundsValid = all(greaterThan(boundsMax, boundsMin));
            }

            if (!boundsValid)
            {
                culled = true;
            }
            else
            {
                projectedPixelSize = (boundsMax - boundsMin) * resolution * 0.5;
                float stochasticCoverage = projectedPixelSize.x * projectedPixelSize.y;
                float stochasticScale = 1.0;
                if (stochasticCoverage < 0.8)
                {
                    uvec3 centerBits = floatBitsToUint(centerLocal);
                    uint randomValue =
                        stableHash
                        (
                            packedPosition ^
                            centerBits.x ^
                            centerBits.y * 0x9e3779b9u ^
                            centerBits.z * 0x85ebca6bu
                        ) & 0xffffu;
                    culled = float(randomValue) > stochasticCoverage * (65535.0 / 0.8);
                    stochasticScale = sqrt(0.8 / max(stochasticCoverage, 0.000001));
                }
                if (!culled)
                {
                    vec2 boundsCenter = (boundsMin + boundsMax) * 0.5;
                    vec2 boundsExtent =
                        (boundsMax - boundsMin) * (0.5 * stochasticScale) +
                        abs(instance.proxyParams.zw);
                    boundsMin = max(boundsCenter - boundsExtent, vec2(-1.0));
                    boundsMax = min(boundsCenter + boundsExtent, vec2(1.0));
                    vec2 cornerSelector = quadCorner * 0.5 + 0.5;
                    vertexNdc = mix(boundsMin, boundsMax, cornerSelector);
                    gl_Position = vec4(vertexNdc * centerClip.w, 0.0, centerClip.w);
                }
            }
        }
    }
    if (culled)
        gl_Position = vec4(2.0, 2.0, 0.0, 1.0);

    vec2 rayUv = vertexNdc * 0.5 + 0.5;
    rayOriginOut =
        instance.rayOriginBase.xyz +
        rayUv.x * instance.rayOriginU.xyz +
        rayUv.y * instance.rayOriginV.xyz;
    rayDirectionOut =
        instance.rayDirectionBase.xyz +
        rayUv.x * instance.rayDirectionU.xyz +
        rayUv.y * instance.rayDirectionV.xyz;
    colorOut = palette[paletteIndex];
    albedoOut = instance.albedo;
    materialOut = instance.material;
    heightPlusOut = instance.heightPlus;
    subsurfacePlusOut = instance.subsurfacePlus;
    clearCoatPlusOut = instance.clearCoatPlus;
    voxelCenterOut = centerLocal;
    facesOut = faces;
}
