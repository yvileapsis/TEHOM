#version 450 core

flat layout(location = 0) in vec4 color;
flat layout(location = 1) in vec4 albedo;
flat layout(location = 2) in vec4 material;
flat layout(location = 3) in vec4 heightPlus;
flat layout(location = 4) in vec4 subsurfacePlus;
flat layout(location = 5) in vec4 clearCoatPlus;
flat layout(location = 6) in vec3 faceNormal;
layout(location = 7) in float clipDistance;

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

void main()
{
    float depthCutoff = heightPlus.z;
    float depth = gl_FragCoord.z / gl_FragCoord.w;
    if (depthCutoff >= 0.0) { if (depth > depthCutoff) discard; }
    else if (depth <= -depthCutoff) discard;

    float alpha = color.a * albedo.a;
    if (alpha <= 0.0)
        discard;
    if (clipDistance < 0.0)
        discard;

    depthOut = gl_FragCoord.z;
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
