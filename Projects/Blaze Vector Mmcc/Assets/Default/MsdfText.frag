#version 450 core

layout(binding = 2) uniform texture2D tex;
layout(set = 1, binding = 0) uniform sampler samp;

layout(location = 0) in vec2 texCoords;
layout(location = 1) in vec4 color;
layout(location = 2) in vec4 outlineColor;
layout(location = 3) in vec4 shader;
layout(location = 4) in vec4 shader2;

layout(location = 0) out vec4 frag;

float median(float r, float g, float b)
{
    return max(min(r, g), min(max(r, g), b));
}

float screenPxRange(vec2 uv, float pxRange)
{
    vec2 texSize = vec2(textureSize(sampler2D(tex, samp), 0));
    vec2 unitRange = vec2(pxRange) / texSize;
    vec2 screenTexSize = vec2(1.0) / fwidth(uv);
    return max(0.5 * dot(unitRange, screenTexSize), 1.0);
}

float coverage(float distancePx, float softness)
{
    return clamp(distancePx / max(softness, 0.001) + 0.5, 0.0, 1.0);
}

void main()
{
    vec4 sampleColor = texture(sampler2D(tex, samp), texCoords);
    float distanceRange = shader.x;
    float edgeOffset = shader.y;
    float softness = max(shader.z, 0.001);
    float outlineThickness = max(shader.w, 0.0);
    float outlineSoftness = max(shader2.x, 0.001);
    float screenRange = screenPxRange(texCoords, distanceRange);
    float multiChannelDistance = median(sampleColor.r, sampleColor.g, sampleColor.b) - 0.5;
    float trueDistance = sampleColor.a - 0.5;
    float fillOpacity = coverage(screenRange * multiChannelDistance + edgeOffset, softness);
    float outlineOpacity = max(coverage(screenRange * trueDistance + edgeOffset + outlineThickness, outlineSoftness) - fillOpacity, 0.0);
    float fillAlpha = color.a * fillOpacity;
    float outlineAlpha = outlineColor.a * outlineOpacity * (1.0 - fillAlpha);
    float alpha = fillAlpha + outlineAlpha;
    vec3 rgb = alpha > 0.0 ? (color.rgb * fillAlpha + outlineColor.rgb * outlineAlpha) / alpha : color.rgb;
    frag = vec4(rgb, alpha);
}
