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

const int kLogBandTextureWidth = 12;

layout(set = 0, binding = 2) uniform sampler2D curveSampler;
layout(set = 0, binding = 3) uniform usampler2D bandSampler;

layout(location = 0) in vec2 renderCoord;
layout(location = 1) flat in vec4 banding;
layout(location = 2) flat in uvec4 glyphData;
layout(location = 3) in vec4 color;
layout(location = 0) out vec4 fragmentColor;

uint CalcRootCode(float y1, float y2, float y3)
{
    uint i1 = floatBitsToUint(y1) >> 31u;
    uint i2 = floatBitsToUint(y2) >> 30u;
    uint i3 = floatBitsToUint(y3) >> 29u;
    uint shift = (i2 & 2u) | (i1 & ~2u);
    shift = (i3 & 4u) | (shift & ~4u);
    return ((0x2E74u >> shift) & 0x0101u);
}

vec2 SolveHorizPoly(vec4 p12, vec2 p3)
{
    vec2 a = p12.xy - p12.zw * 2.0 + p3;
    vec2 b = p12.xy - p12.zw;
    float ra = 1.0 / a.y;
    float rb = 0.5 / b.y;
    float d = sqrt(max(b.y * b.y - a.y * p12.y, 0.0));
    float t1 = (b.y - d) * ra;
    float t2 = (b.y + d) * ra;
    if (abs(a.y) < 1.0 / 65536.0) t1 = t2 = p12.y * rb;
    return vec2((a.x * t1 - b.x * 2.0) * t1 + p12.x, (a.x * t2 - b.x * 2.0) * t2 + p12.x);
}

vec2 SolveVertPoly(vec4 p12, vec2 p3)
{
    vec2 a = p12.xy - p12.zw * 2.0 + p3;
    vec2 b = p12.xy - p12.zw;
    float ra = 1.0 / a.x;
    float rb = 0.5 / b.x;
    float d = sqrt(max(b.x * b.x - a.x * p12.x, 0.0));
    float t1 = (b.x - d) * ra;
    float t2 = (b.x + d) * ra;
    if (abs(a.x) < 1.0 / 65536.0) t1 = t2 = p12.x * rb;
    return vec2((a.y * t1 - b.y * 2.0) * t1 + p12.y, (a.y * t2 - b.y * 2.0) * t2 + p12.y);
}

ivec2 CalcBandLoc(ivec2 glyphLoc, uint offset)
{
    ivec2 bandLoc = glyphLoc + ivec2(int(offset), 0);
    bandLoc.y += bandLoc.x >> kLogBandTextureWidth;
    bandLoc.x &= (1 << kLogBandTextureWidth) - 1;
    return bandLoc;
}

float CalcCoverage(float xcov, float ycov, float xwgt, float ywgt, uint flags)
{
    float coverage = max(abs(xcov * xwgt + ycov * ywgt) / max(xwgt + ywgt, 1.0 / 65536.0), min(abs(xcov), abs(ycov)));
    if ((flags & 0x1000u) == 0u)
    {
        coverage = clamp(coverage, 0.0, 1.0);
    }
    else
    {
        coverage = 1.0 - abs(1.0 - fract(coverage * 0.5) * 2.0);
    }
    return coverage;
}

float SlugRender(vec2 coord, vec4 bandTransform, uvec4 glyph)
{
    vec2 emsPerPixel = fwidth(coord);
    vec2 pixelsPerEm = 1.0 / max(emsPerPixel, vec2(1.0e-8));
    uvec2 bandMax = glyph.zw;
    bandMax.y &= 0x00FFu;
    ivec2 bandIndex = clamp(ivec2(coord * bandTransform.xy + bandTransform.zw), ivec2(0), ivec2(bandMax));
    ivec2 glyphLoc = ivec2(glyph.xy);
    float xcov = 0.0;
    float xwgt = 0.0;
    uvec2 hbandData = texelFetch(bandSampler, ivec2(glyphLoc.x + bandIndex.y, glyphLoc.y), 0).xy;
    ivec2 hbandLoc = CalcBandLoc(glyphLoc, hbandData.y);
    for (int curveIndex = 0; curveIndex < int(hbandData.x); curveIndex++)
    {
        ivec2 curveLoc = ivec2(texelFetch(bandSampler, ivec2(hbandLoc.x + curveIndex, hbandLoc.y), 0).xy);
        vec4 p12 = texelFetch(curveSampler, curveLoc, 0) - vec4(coord, coord);
        vec2 p3 = texelFetch(curveSampler, ivec2(curveLoc.x + 1, curveLoc.y), 0).xy - coord;
        if (max(max(p12.x, p12.z), p3.x) * pixelsPerEm.x < -0.5) break;
        uint code = CalcRootCode(p12.y, p12.w, p3.y);
        if (code != 0u)
        {
            vec2 roots = SolveHorizPoly(p12, p3) * pixelsPerEm.x;
            if ((code & 1u) != 0u)
            {
                xcov += clamp(roots.x + 0.5, 0.0, 1.0);
                xwgt = max(xwgt, clamp(1.0 - abs(roots.x) * 2.0, 0.0, 1.0));
            }
            if (code > 1u)
            {
                xcov -= clamp(roots.y + 0.5, 0.0, 1.0);
                xwgt = max(xwgt, clamp(1.0 - abs(roots.y) * 2.0, 0.0, 1.0));
            }
        }
    }
    float ycov = 0.0;
    float ywgt = 0.0;
    uvec2 vbandData = texelFetch(bandSampler, ivec2(glyphLoc.x + bandMax.y + 1u + bandIndex.x, glyphLoc.y), 0).xy;
    ivec2 vbandLoc = CalcBandLoc(glyphLoc, vbandData.y);
    for (int curveIndex = 0; curveIndex < int(vbandData.x); curveIndex++)
    {
        ivec2 curveLoc = ivec2(texelFetch(bandSampler, ivec2(vbandLoc.x + curveIndex, vbandLoc.y), 0).xy);
        vec4 p12 = texelFetch(curveSampler, curveLoc, 0) - vec4(coord, coord);
        vec2 p3 = texelFetch(curveSampler, ivec2(curveLoc.x + 1, curveLoc.y), 0).xy - coord;
        if (max(max(p12.y, p12.w), p3.y) * pixelsPerEm.y < -0.5) break;
        uint code = CalcRootCode(p12.x, p12.z, p3.x);
        if (code != 0u)
        {
            vec2 roots = SolveVertPoly(p12, p3) * pixelsPerEm.y;
            if ((code & 1u) != 0u)
            {
                ycov -= clamp(roots.x + 0.5, 0.0, 1.0);
                ywgt = max(ywgt, clamp(1.0 - abs(roots.x) * 2.0, 0.0, 1.0));
            }
            if (code > 1u)
            {
                ycov += clamp(roots.y + 0.5, 0.0, 1.0);
                ywgt = max(ywgt, clamp(1.0 - abs(roots.y) * 2.0, 0.0, 1.0));
            }
        }
    }
    return CalcCoverage(xcov, ycov, xwgt, ywgt, glyph.w);
}

void main()
{
    float coverage = SlugRender(renderCoord, banding, glyphData);
    fragmentColor = color * coverage;
}
