#version 450 core
// Slug shader code Copyright 2017 by Eric Lengyel.
// Vulkan GLSL adaptation derived from https://github.com/EricLengyel/Slug.
// SPDX-License-Identifier: MIT
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

const int VERTS = 6;
const int SLUG_TEXT_BATCH_SIZE = 192;

const vec2 FILTERS[VERTS] = vec2[VERTS](
    vec2(0.0, 0.0),
    vec2(1.0, 0.0),
    vec2(1.0, 1.0),
    vec2(1.0, 1.0),
    vec2(0.0, 1.0),
    vec2(0.0, 0.0));

struct Glyph
{
    vec4 perimeter;
    vec4 texCoords;
    vec4 jacobian;
    vec4 banding;
    uvec4 glyph;
    vec4 color;
};

struct ViewProjection
{
    mat4 viewProjection;
    vec4 viewport;
};

layout(binding = 0) buffer readonly GlyphBlock
{
    Glyph glyphs[SLUG_TEXT_BATCH_SIZE];
};

layout(binding = 1) buffer readonly ViewProjectionBlock
{
    ViewProjection viewProjection;
};

layout(location = 0) out vec2 renderCoord;
layout(location = 1) flat out vec4 banding;
layout(location = 2) flat out uvec4 glyphData;
layout(location = 3) out vec4 color;

vec2 SlugDilate(vec4 pos, vec4 tex, vec4 jac, mat4 matrix, vec2 dimensions, out vec2 vertexPosition)
{
    vec2 n = normalize(pos.zw);
    vec4 row0 = vec4(matrix[0].x, matrix[1].x, matrix[2].x, matrix[3].x);
    vec4 row1 = vec4(matrix[0].y, matrix[1].y, matrix[2].y, matrix[3].y);
    vec4 row3 = vec4(matrix[0].w, matrix[1].w, matrix[2].w, matrix[3].w);
    float s = dot(row3.xy, pos.xy) + row3.w;
    float t = dot(row3.xy, n);
    float u = (s * dot(row0.xy, n) - t * (dot(row0.xy, pos.xy) + row0.w)) * dimensions.x;
    float v = (s * dot(row1.xy, n) - t * (dot(row1.xy, pos.xy) + row1.w)) * dimensions.y;
    float s2 = s * s;
    float st = s * t;
    float uv = u * u + v * v;
    float denominator = max(uv - st * st, 1.0e-12);
    vec2 d = pos.zw * (s2 * (st + sqrt(uv)) / denominator);
    vertexPosition = pos.xy + d;
    return tex.xy + vec2(dot(d, jac.xy), dot(d, jac.zw));
}

void main()
{
    int glyphId = gl_VertexIndex / VERTS;
    int vertexId = gl_VertexIndex % VERTS;
    Glyph glyph = glyphs[glyphId];
    vec2 corner = FILTERS[vertexId];
    vec2 position = glyph.perimeter.xy + glyph.perimeter.zw * corner;
    vec2 tex = glyph.texCoords.xy + glyph.texCoords.zw * corner;
    vec4 pos = vec4(position, corner * 2.0 - 1.0);
    vec4 tex4 = vec4(tex, 0.0, 0.0);
    vec2 dilatedPosition;
    renderCoord = SlugDilate(pos, tex4, glyph.jacobian, viewProjection.viewProjection, viewProjection.viewport.xy, dilatedPosition);
    gl_Position = viewProjection.viewProjection * vec4(dilatedPosition, 0.0, 1.0);
    banding = glyph.banding;
    glyphData = glyph.glyph;
    color = glyph.color;
}
