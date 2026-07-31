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
    vec4 ambientLight;
    vec4 keyLightDirection;
    vec4 keyLightColor;
};

layout(set = 1, binding = 0) uniform InstanceUniform { VoxelInstanceStruct instance; };

flat layout(location = 0) in vec4 color;
flat layout(location = 1) in vec4 albedo;
flat layout(location = 2) in vec4 material;
flat layout(location = 6) in vec3 faceNormal;
layout(location = 7) in float clipDistance;

layout(location = 0) out vec4 colorOut;

void main()
{
    float alpha = color.a * albedo.a;
    if (alpha <= 0.0 || clipDistance < 0.0)
        discard;

    vec3 normal = normalize(faceNormal);
    vec3 lightDirection = normalize(instance.keyLightDirection.xyz);
    float diffuse = max(dot(normal, lightDirection), 0.0);
    vec3 ambient = instance.ambientLight.rgb * 0.55;
    vec3 direct = instance.keyLightColor.rgb * diffuse * 0.22;
    vec3 lighting = ambient + direct + vec3(material.w);
    colorOut = vec4(color.rgb * albedo.rgb * lighting, alpha);
}
