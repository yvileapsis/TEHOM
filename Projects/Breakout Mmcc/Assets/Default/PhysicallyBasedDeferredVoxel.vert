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
    vec4 viewport;
    vec4 clipPlane;
};

layout(set = 0, binding = 0) uniform EyeUniform { EyeStruct eye; };
layout(std430, set = 1, binding = 0) readonly buffer SplatBuffer { uint splats[]; };
layout(std430, set = 1, binding = 1) readonly buffer PaletteBuffer { vec4 palette[]; };
layout(set = 2, binding = 0) uniform InstanceUniform { VoxelInstanceStruct instance; };

flat layout(location = 0) out vec3 centerOut;
flat layout(location = 1) out vec3 axisXOut;
flat layout(location = 2) out vec3 axisYOut;
flat layout(location = 3) out vec3 axisZOut;
flat layout(location = 4) out vec4 colorOut;
flat layout(location = 5) out vec4 albedoOut;
flat layout(location = 6) out vec4 materialOut;
flat layout(location = 7) out vec4 heightPlusOut;
flat layout(location = 8) out vec4 subsurfacePlusOut;
flat layout(location = 9) out vec4 clearCoatPlusOut;
flat layout(location = 10) out vec4 viewportOut;
flat layout(location = 11) out vec4 clipPlaneOut;

const vec2 QUAD_CORNERS[6] = vec2[6]
(
    vec2(-1.0, -1.0),
    vec2( 1.0, -1.0),
    vec2( 1.0,  1.0),
    vec2( 1.0,  1.0),
    vec2(-1.0,  1.0),
    vec2(-1.0, -1.0)
);

void main()
{
    uint splat = splats[gl_InstanceIndex];
    uint x = splat & 63u;
    uint y = (splat >> 6) & 63u;
    uint z = (splat >> 12) & 63u;
    uint paletteIndex = splat >> 18;
    vec3 position = instance.voxelOrigin.xyz + vec3(float(x), float(y), float(z)) * instance.voxelSize.xyz;
    vec4 centerWorld = instance.model * vec4(position, 1.0);
    vec3 axisX = instance.model[0].xyz * instance.voxelSize.x * 0.5;
    vec3 axisY = instance.model[1].xyz * instance.voxelSize.y * 0.5;
    vec3 axisZ = instance.model[2].xyz * instance.voxelSize.z * 0.5;
    float radius = length(vec3(length(axisX), length(axisY), length(axisZ)));
    vec4 centerView = eye.view * centerWorld;
    vec4 centerClip = eye.viewProjection * centerWorld;
    bool perspective = abs(eye.projection[3][3]) < 0.5;
    float distanceScale = perspective ? max(abs(centerView.z), 0.001) : 1.0;
    vec2 ndcRadius = abs(vec2(eye.projection[0][0], eye.projection[1][1])) * radius / distanceScale;
    vec2 corner = QUAD_CORNERS[gl_VertexIndex];

    centerOut = centerWorld.xyz;
    axisXOut = axisX;
    axisYOut = axisY;
    axisZOut = axisZ;
    colorOut = palette[paletteIndex];
    albedoOut = instance.albedo;
    materialOut = instance.material;
    heightPlusOut = instance.heightPlus;
    subsurfacePlusOut = instance.subsurfacePlus;
    clearCoatPlusOut = instance.clearCoatPlus;
    viewportOut = instance.viewport;
    clipPlaneOut = instance.clipPlane;

    gl_Position = centerClip;
    gl_Position.xy += corner * ndcRadius * centerClip.w;
}
