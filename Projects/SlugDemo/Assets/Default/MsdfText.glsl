#shader vertex
#version 460 core

const int VERTS = 6;
const int MSDF_TEXT_BATCH_SIZE = 192;

const vec4 FILTERS[VERTS] =
    vec4[VERTS](
        vec4(1.0, 1.0, 0.0, 0.0),
        vec4(1.0, 1.0, 1.0, 0.0),
        vec4(1.0, 1.0, 1.0, 1.0),
        vec4(1.0, 1.0, 1.0, 1.0),
        vec4(1.0, 1.0, 0.0, 1.0),
        vec4(1.0, 1.0, 0.0, 0.0));

uniform vec4 perimeters[MSDF_TEXT_BATCH_SIZE];
uniform vec4 texCoordses[MSDF_TEXT_BATCH_SIZE];
uniform vec4 colors[MSDF_TEXT_BATCH_SIZE];
uniform vec4 outlineColors[MSDF_TEXT_BATCH_SIZE];
uniform vec4 shaders[MSDF_TEXT_BATCH_SIZE];
uniform vec4 shader2s[MSDF_TEXT_BATCH_SIZE];
uniform mat4 viewProjection;

out vec2 texCoords;
out vec4 color;
out vec4 outlineColor;
out vec4 shader;
out vec4 shader2;

void main()
{
    int glyphId = gl_VertexID / VERTS;
    int vertexId = gl_VertexID % VERTS;

    vec4 filt = FILTERS[vertexId];
    vec4 perimeter = perimeters[glyphId] * filt;
    vec2 position = vec2(perimeter.x + perimeter.z, perimeter.y + perimeter.w);
    gl_Position = viewProjection * vec4(position.x, position.y, 0, 1);

    vec4 texCoords4 = texCoordses[glyphId] * filt;
    texCoords = vec2(texCoords4.x + texCoords4.z, texCoords4.y + texCoords4.w);
    color = colors[glyphId];
    outlineColor = outlineColors[glyphId];
    shader = shaders[glyphId];
    shader2 = shader2s[glyphId];
}

#shader fragment
#version 460 core

uniform sampler2D tex;

in vec2 texCoords;
in vec4 color;
in vec4 outlineColor;
in vec4 shader;
in vec4 shader2;

layout(location = 0) out vec4 frag;

float median(float r, float g, float b)
{
    return max(min(r, g), min(max(r, g), b));
}

float screenPxRange(vec2 uv, float pxRange)
{
    vec2 texSize = vec2(textureSize(tex, 0));
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
    vec4 sampleColor = texture(tex, texCoords);
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
