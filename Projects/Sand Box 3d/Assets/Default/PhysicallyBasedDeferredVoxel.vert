#version 450 core

struct EyeStruct
{
    vec3 center;
    mat4 view;
    mat4 viewInverse;
    mat4 projection;
    mat4 projectionInverse;
    mat4 viewProjection;
};

struct VoxelInstanceStruct
{
    mat4 model;
    vec4 voxelOrigin;
    vec4 voxelSize;
    vec4 albedo;
    vec4 material;
    vec4 heightPlus;
    vec4 subsurfacePlus;
    vec4 clearCoatPlus;
    vec4 clipPlane;
};

layout(set = 0, binding = 0) uniform EyeUniform { EyeStruct eye; };
layout(std430, set = 1, binding = 0) readonly buffer FaceBuffer { uvec2 faces[]; };
layout(std430, set = 1, binding = 1) readonly buffer PaletteBuffer { vec4 palette[]; };
layout(set = 2, binding = 0) uniform InstanceUniform { VoxelInstanceStruct instance; };

layout(location = 0) out vec3 positionOut;
flat layout(location = 1) out vec4 colorOut;
flat layout(location = 2) out vec4 albedoOut;
flat layout(location = 3) out vec4 materialOut;
flat layout(location = 4) out vec4 heightPlusOut;
flat layout(location = 5) out vec4 subsurfacePlusOut;
flat layout(location = 6) out vec4 clearCoatPlusOut;
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
    uvec2 face = faces[gl_InstanceIndex];
    uint packedPosition = face.x;
    uint x = packedPosition & 1023u;
    uint y = (packedPosition >> 10) & 1023u;
    uint z = (packedPosition >> 20) & 1023u;
    uint faceIndex = face.y & 7u;
    uint paletteIndex = face.y >> 3;
    vec3 centerLocal = instance.voxelOrigin.xyz + vec3(float(x), float(y), float(z)) * instance.voxelSize.xyz;
    vec3 centerWorld = (instance.model * vec4(centerLocal, 1.0)).xyz;
    vec3 axisX = instance.model[0].xyz * instance.voxelSize.x * 0.5;
    vec3 axisY = instance.model[1].xyz * instance.voxelSize.y * 0.5;
    vec3 axisZ = instance.model[2].xyz * instance.voxelSize.z * 0.5;
    vec3 corner = CUBE_CORNERS[FACE_CORNERS[faceIndex][gl_VertexIndex]];
    vec3 positionWorld = centerWorld + corner.x * axisX + corner.y * axisY + corner.z * axisZ;

    positionOut = positionWorld;
    colorOut = palette[paletteIndex];
    albedoOut = instance.albedo;
    materialOut = instance.material;
    heightPlusOut = instance.heightPlus;
    subsurfacePlusOut = instance.subsurfacePlus;
    clearCoatPlusOut = instance.clearCoatPlus;
    clipDistanceOut = dot(vec4(positionWorld, 1.0), instance.clipPlane);
    gl_Position = eye.viewProjection * vec4(positionWorld, 1.0);
}
