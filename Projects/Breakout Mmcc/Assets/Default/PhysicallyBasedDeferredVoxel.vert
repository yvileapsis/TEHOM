#version 450 core

struct VoxelInstanceStruct
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
};

layout(std430, set = 0, binding = 0) readonly buffer FaceBuffer { uint faces[]; };
layout(std430, set = 0, binding = 1) readonly buffer PaletteBuffer { vec4 palette[]; };
layout(set = 1, binding = 0) uniform InstanceUniform { VoxelInstanceStruct instance; };

flat layout(location = 0) out vec4 colorOut;
flat layout(location = 1) out vec4 albedoOut;
flat layout(location = 2) out vec4 materialOut;
flat layout(location = 3) out vec4 heightPlusOut;
flat layout(location = 4) out vec4 subsurfacePlusOut;
flat layout(location = 5) out vec4 clearCoatPlusOut;
flat layout(location = 6) out vec3 normalOut;
layout(location = 7) out float clipDistanceOut;

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

const uvec4 FACE_CORNERS[6] = uvec4[6]
(
    uvec4(1, 2, 6, 5),
    uvec4(0, 4, 7, 3),
    uvec4(3, 7, 6, 2),
    uvec4(0, 1, 5, 4),
    uvec4(0, 3, 2, 1),
    uvec4(4, 5, 6, 7)
);

void main()
{
    uint x;
    uint y;
    uint z;
    uint faceIndex;
    uint paletteIndex;
    if (instance.voxelSize.w > 0.0)
    {
        uint face = faces[gl_InstanceIndex];
        x = face & 63u;
        y = (face >> 6) & 63u;
        z = (face >> 12) & 63u;
        faceIndex = (face >> 18) & 7u;
        paletteIndex = face >> 21;
    }
    else
    {
        uint packedPosition = faces[gl_InstanceIndex * 2];
        uint packedAttributes = faces[gl_InstanceIndex * 2 + 1];
        x = packedPosition & 1023u;
        y = (packedPosition >> 10) & 1023u;
        z = (packedPosition >> 20) & 1023u;
        faceIndex = packedAttributes & 7u;
        paletteIndex = packedAttributes >> 3;
    }

    vec3 centerLocal =
        instance.voxelOrigin.xyz +
        vec3(float(x), float(y), float(z)) * instance.voxelSize.xyz;
    vec3 corner = CUBE_CORNERS[FACE_CORNERS[faceIndex][gl_VertexIndex]];
    vec3 positionLocal = centerLocal + corner * instance.voxelSize.xyz * 0.5;

    colorOut = palette[paletteIndex];
    albedoOut = instance.albedo;
    materialOut = instance.material;
    heightPlusOut = instance.heightPlus;
    subsurfacePlusOut = instance.subsurfacePlus;
    clearCoatPlusOut = instance.clearCoatPlus;
    normalOut =
        faceIndex == 0u ? instance.normalX.xyz :
        faceIndex == 1u ? -instance.normalX.xyz :
        faceIndex == 2u ? instance.normalY.xyz :
        faceIndex == 3u ? -instance.normalY.xyz :
        faceIndex == 4u ? instance.normalZ.xyz :
                         -instance.normalZ.xyz;
    clipDistanceOut = dot(vec4(positionLocal, 1.0), instance.clipPlaneLocal);
    gl_Position = instance.modelViewProjection * vec4(positionLocal, 1.0);
}
