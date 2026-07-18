#version 450 core

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

struct Glyph
{
    vec4 perimeter;
    vec4 texCoords;
    vec4 color;
    vec4 outlineColor;
    vec4 shader;
    vec4 shader2;
};

struct ViewProjection
{
    mat4 viewProjection;
};

layout(binding = 0) buffer readonly GlyphBlock
{
    Glyph glyphs[MSDF_TEXT_BATCH_SIZE];
};

layout(binding = 1) buffer readonly ViewProjectionBlock
{
    ViewProjection viewProjection;
};

layout(location = 0) out vec2 texCoords;
layout(location = 1) out vec4 color;
layout(location = 2) out vec4 outlineColor;
layout(location = 3) out vec4 shader;
layout(location = 4) out vec4 shader2;

void main()
{
    int glyphId = gl_VertexIndex / VERTS;
    int vertexId = gl_VertexIndex % VERTS;

    vec4 filt = FILTERS[vertexId];
    Glyph glyph = glyphs[glyphId];
    vec4 perimeter = glyph.perimeter * filt;
    vec2 position = vec2(perimeter.x + perimeter.z, perimeter.y + perimeter.w);
    gl_Position = viewProjection.viewProjection * vec4(position.x, position.y, 0, 1);

    vec4 texCoords4 = glyph.texCoords * filt;
    texCoords = vec2(texCoords4.x + texCoords4.z, texCoords4.y + texCoords4.w);
    color = glyph.color;
    outlineColor = glyph.outlineColor;
    shader = glyph.shader;
    shader2 = glyph.shader2;
}
