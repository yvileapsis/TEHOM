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

// Shared SlugShape std430 ABI. This is intentionally identical to SlugShape.vert
// and SlugShape.comp. Curves and band lists remain mathematical source data: the
// bounding quad only limits fragment invocation; it never supplies a boundary.

struct SlugViewState
{
    mat4 viewProjection;
    vec4 viewport;
    vec4 time;
    uvec4 counts;
};

struct SlugLayerState
{
    mat4 transform;
    vec4 color;
    vec4 origin;
    uvec4 stateIds;        // x shape, y fill, z effect, w mask (0xffffffff = none).
    uvec4 resourceIds;     // x gradient, y fill texture, z material, w reserved.
    vec4 effectParams;
    vec4 effectParams2;
    mat4 gradientTransform;
    vec4 maskMaterial;
};

struct SlugShapeMetadata
{
    vec4 bounds;
    vec4 bandTransform;
    uvec4 bandAddress;     // xy headers, zw curve-list origin.
    uvec4 bandFlags;       // x/y maxima, z band row width, w fill flags.
};

struct SlugGradientDescription
{
    uvec4 stopRange;       // x first stop, y count, z kind (linear/radial/sweep), w flags.
    vec4 params;           // linear: origin.xy + direction.zw; radial: center.xy + radii.zw; sweep: center.xy/start.z/scale.w.
    vec4 params2;          // focal radial: center1.xy; reserved for other kinds.
};

struct SlugGradientStop
{
    vec4 color;            // rgba color.
    vec4 offset;           // x stop offset; yzw reserved.
};

struct SlugMaskState
{
    vec4 params;
    vec4 params2;
    uvec4 state;            // x type, y MSDF layer, z invert, w flags.
};

struct SlugComputeConfig
{
    uvec4 dispatch;
    vec4 params0;
    vec4 params1;
};

layout(std430, binding = 0) readonly buffer SlugViewBlock
{
    SlugViewState view;
};
layout(std430, binding = 1) readonly buffer SlugLayerBlock
{
    SlugLayerState layers[];
};
layout(std430, binding = 2) readonly buffer SlugShapeBlock
{
    SlugShapeMetadata shapes[];
};
layout(binding = 3) uniform sampler2D curveSampler;
layout(binding = 4) uniform usampler2D bandSampler;
layout(std430, binding = 5) readonly buffer SlugGradientBlock
{
    SlugGradientDescription gradients[];
};
layout(std430, binding = 6) readonly buffer SlugStopBlock
{
    SlugGradientStop stops[];
};
layout(std430, binding = 7) readonly buffer SlugMaskBlock
{
    SlugMaskState masks[];
};
layout(binding = 8) uniform sampler2D fillTextures[8];
layout(std430, binding = 9) readonly buffer SlugComputeConfigBlock
{
    SlugComputeConfig computeConfig;
};

layout(location = 0) in vec2 emCoord;
layout(location = 1) in vec2 shapeUv;
layout(location = 2) in vec2 compositeCoord;
layout(location = 3) in vec2 gradientCoord;
layout(location = 4) flat in vec4 layerColor;
layout(location = 5) flat in uvec4 layerState;
layout(location = 6) flat in vec4 layerEffectParams;
layout(location = 7) flat in vec4 layerEffectParams2;
layout(location = 8) flat in vec4 layerMaskMaterial;
layout(location = 9) flat in vec4 shapeBandTransform;
layout(location = 10) flat in uvec4 shapeBandAddress;
layout(location = 11) flat in uvec4 shapeBandFlags;
layout(location = 12) flat in uvec4 layerResourceIds;

layout(location = 0) out vec4 fragmentColor;

// The endpoint-safe root eligibility evaluator is retained from SlugText.frag.
// It classifies the signs of all three control-point coordinates before solving,
// so shared endpoints and tangent joins are never double-counted.
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
    vec2 b = p12.xy - p12.zw;
    vec2 c = p3 - p12.zw;
    vec2 a = b + c;
    float ra = 1.0 / a.y;
    float rb = 0.5 / b.y;
    float d = sqrt(max(b.y * b.y - a.y * p12.y, 0.0));
    float t1 = (b.y - d) * ra;
    float t2 = (b.y + d) * ra;
    float linearScale = max(max(abs(b.y), abs(c.y)), 1.0);
    if (abs(a.y) < linearScale / 65536.0) t1 = t2 = p12.y * rb;
    return vec2((a.x * t1 - b.x * 2.0) * t1 + p12.x, (a.x * t2 - b.x * 2.0) * t2 + p12.x);
}

vec2 SolveVertPoly(vec4 p12, vec2 p3)
{
    vec2 b = p12.xy - p12.zw;
    vec2 c = p3 - p12.zw;
    vec2 a = b + c;
    float ra = 1.0 / a.x;
    float rb = 0.5 / b.x;
    float d = sqrt(max(b.x * b.x - a.x * p12.x, 0.0));
    float t1 = (b.x - d) * ra;
    float t2 = (b.x + d) * ra;
    float linearScale = max(max(abs(b.x), abs(c.x)), 1.0);
    if (abs(a.x) < linearScale / 65536.0) t1 = t2 = p12.x * rb;
    return vec2((a.y * t1 - b.y * 2.0) * t1 + p12.y, (a.y * t2 - b.y * 2.0) * t2 + p12.y);
}

ivec2 SlugPackedLoc(uvec2 origin, uint offset, uint rowWidth)
{
    uint width = max(rowWidth, 1u);
    uint linearX = origin.x + offset;
    return ivec2(int(linearX % width), int(origin.y + linearX / width));
}

ivec2 SlugCurveNext(ivec2 loc)
{
    int width = max(textureSize(curveSampler, 0).x, 1);
    int x = loc.x + 1;
    return ivec2(x % width, loc.y + x / width);
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

float SlugRender(vec2 coord, vec4 bandTransform, uvec2 bandMax, uvec4 bandAddress, uvec4 bandFlags)
{
    vec2 emsPerPixel = fwidth(coord);
    vec2 pixelsPerEm = 1.0 / max(emsPerPixel, vec2(1.0e-8));
    ivec2 maxBand = ivec2(min(bandMax, uvec2(32767u)));
    uint rowWidth = max(bandFlags.z, 1u);
    ivec2 bandIndex = clamp(ivec2(coord * bandTransform.xy + bandTransform.zw), ivec2(0), maxBand);

    float xcov = 0.0;
    float xwgt = 0.0;
    uvec4 hbandData = texelFetch(bandSampler, SlugPackedLoc(bandAddress.xy, uint(bandIndex.y), rowWidth), 0);
    uint hcount = min(hbandData.x, 256u);
    for (uint curveIndex = 0u; curveIndex < hcount; curveIndex++)
    {
        ivec2 listLoc = SlugPackedLoc(bandAddress.zw, hbandData.y + curveIndex, rowWidth);
        uvec2 curveRef = texelFetch(bandSampler, listLoc, 0).xy;
        ivec2 curveLoc = ivec2(curveRef);
        vec4 p12 = texelFetch(curveSampler, curveLoc, 0) - vec4(coord, coord);
        vec2 p3 = texelFetch(curveSampler, SlugCurveNext(curveLoc), 0).xy - coord;
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
    uint verticalHeader = bandFlags.y + 1u + uint(bandIndex.x);
    uvec4 vbandData = texelFetch(bandSampler, SlugPackedLoc(bandAddress.xy, verticalHeader, rowWidth), 0);
    uint vcount = min(vbandData.x, 256u);
    for (uint curveIndex = 0u; curveIndex < vcount; curveIndex++)
    {
        ivec2 listLoc = SlugPackedLoc(bandAddress.zw, vbandData.y + curveIndex, rowWidth);
        uvec2 curveRef = texelFetch(bandSampler, listLoc, 0).xy;
        ivec2 curveLoc = ivec2(curveRef);
        vec4 p12 = texelFetch(curveSampler, curveLoc, 0) - vec4(coord, coord);
        vec2 p3 = texelFetch(curveSampler, SlugCurveNext(curveLoc), 0).xy - coord;
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

    uint fillFlags = ((bandFlags.w & 1u) != 0u) ? 0x1000u : bandFlags.w;
    return CalcCoverage(xcov, ycov, xwgt, ywgt, fillFlags);
}

float SlugMaskDistanceCoverage(float distance, vec2 emsPerPixel)
{
    float pixel = max(max(abs(emsPerPixel.x), abs(emsPerPixel.y)), 1.0e-5);
    return clamp(0.5 - distance / pixel, 0.0, 1.0);
}

float SlugSdfCircle(vec2 p, vec2 center, float radius)
{
    return length(p - center) - radius;
}

float SlugSdfBox(vec2 p, vec2 center, vec2 halfExtents)
{
    vec2 d = abs(p - center) - halfExtents;
    return length(max(d, vec2(0.0))) + min(max(d.x, d.y), 0.0);
}

float SlugSdfCapsule(vec2 p, vec2 a, vec2 b, float radius)
{
    vec2 pa = p - a;
    vec2 ba = b - a;
    float h = clamp(dot(pa, ba) / max(dot(ba, ba), 1.0e-8), 0.0, 1.0);
    return length(pa - ba * h) - radius;
}

float SlugSdfPie(vec2 p, vec2 center, float radius, float angle0, float angle1)
{
    vec2 q = p - center;
    float midAngle = (angle0 + angle1) * 0.5;
    float halfSpan = (angle1 - angle0) * 0.5;
    vec2 sc = vec2(sin(halfSpan), cos(halfSpan));
    float cosM = cos(-midAngle), sinM = sin(-midAngle);
    vec2 rp = vec2(q.x * cosM - q.y * sinM, q.x * sinM + q.y * cosM);
    rp.x = abs(rp.x);
    float l = length(rp) - radius;
    float m = length(rp - sc * clamp(dot(rp, sc), 0.0, radius));
    return max(l, m * sign(sc.y * rp.x - sc.x * rp.y));
}

float SlugSdfArcBand(vec2 p, vec2 center, float radius, float angle0, float angle1, float halfWidth)
{
    vec2 q = p - center;
    float midAngle = (angle0 + angle1) * 0.5;
    float halfSpan = (angle1 - angle0) * 0.5;
    float cosM = cos(-midAngle), sinM = sin(-midAngle);
    vec2 rp = vec2(q.x * cosM - q.y * sinM, q.x * sinM + q.y * cosM);
    rp.y = abs(rp.y);
    vec2 sc = vec2(cos(halfSpan), sin(halfSpan));
    float k = (sc.x * rp.y > sc.y * rp.x) ? dot(rp, sc) : length(rp);
    return sqrt(max(dot(rp, rp) + radius * radius - 2.0 * radius * k, 0.0)) - halfWidth;
}

vec2 SlugRotateMask(vec2 p, float angle)
{
    float c = cos(angle), s = sin(angle);
    return vec2(p.x * c - p.y * s, p.x * s + p.y * c);
}

float SlugSdfHexagon(vec2 p, vec2 center, float radius, float rotation)
{
    vec2 q = abs(SlugRotateMask(p - center, -rotation));
    const vec3 k = vec3(-0.866025404, 0.5, 0.577350269);
    q -= 2.0 * min(dot(k.xy, q), 0.0) * k.xy;
    q -= vec2(clamp(q.x, -k.z * radius, k.z * radius), radius);
    return length(q) * sign(q.y);
}

float SlugSdfOctagon(vec2 p, vec2 center, float radius, float rotation)
{
    vec2 q = abs(SlugRotateMask(p - center, -rotation));
    const vec3 k = vec3(-0.9238795325, 0.3826834323, 0.4142135623);
    q -= 2.0 * min(dot(vec2(k.x, k.y), q), 0.0) * vec2(k.x, k.y);
    q -= 2.0 * min(dot(vec2(-k.x, k.y), q), 0.0) * vec2(-k.x, k.y);
    q -= vec2(clamp(q.x, -k.z * radius, k.z * radius), radius);
    return length(q) * sign(q.y);
}

float SlugSdfStar(vec2 p, vec2 center, float radius, float points, float innerRatio, float rotation)
{
    vec2 q = SlugRotateMask(p - center, -rotation);
    float n = max(round(points), 3.0);
    float m = mix(2.0, n, clamp(innerRatio, 0.0, 1.0));
    float an = 3.14159265 / n;
    float en = 3.14159265 / m;
    vec2 acs = vec2(cos(an), sin(an));
    vec2 ecs = vec2(cos(en), sin(en));
    float bn = mod(atan(q.x, q.y), 2.0 * an) - an;
    q = length(q) * vec2(cos(bn), abs(sin(bn)));
    q -= radius * acs;
    q += ecs * clamp(-dot(q, ecs), 0.0, radius * acs.y / max(ecs.y, 1.0e-5));
    return length(q) * sign(q.x);
}

float SlugMaskCoverageFor(vec2 canvasCoord, uvec4 layerIds, vec2 emsPerPixel)
{
    uint maskIndex = layerIds.w;
    if (maskIndex == 0xffffffffu || maskIndex >= masks.length()) return 1.0;
    SlugMaskState mask = masks[maskIndex];
    uint type = mask.state.x;
    if (type == 0xffffffffu) return 1.0;

    float maskFill = 0.0;
    if (type == 0u) // MSDF tile in the composite coordinate system.
    {
        uint textureIndex = min(mask.state.y, 7u);
        if (mask.state.y != 0xffffffffu)
        {
            vec2 center = mask.params.xy;
            float radius = max(mask.params.z, 0.0);
            float range = max(mask.params.w, 1.0e-5);
            vec2 bboxMin = center - vec2(radius + range);
            vec2 bboxMax = center + vec2(radius + range);
            vec2 tileUv = (canvasCoord - bboxMin) / max(bboxMax - bboxMin, vec2(1.0e-5));
            ivec2 textureExtent = textureSize(fillTextures[int(textureIndex)], 0);
            if (all(greaterThanEqual(tileUv, vec2(0.0))) && all(lessThanEqual(tileUv, vec2(1.0))) && all(greaterThan(textureExtent, ivec2(0))))
            {
                vec3 msd = texture(fillTextures[int(textureIndex)], tileUv).rgb;
                float median = max(min(msd.r, msd.g), min(max(msd.r, msd.g), msd.b));
                float pxRange = max(2.0 * range / max(max(abs(emsPerPixel.x), abs(emsPerPixel.y)), 1.0e-5), 1.0);
                maskFill = clamp((median - 0.5) * pxRange + 0.5, 0.0, 1.0);
            }
        }
    }
    else if (type == 1u) // circle
    {
        maskFill = SlugMaskDistanceCoverage(SlugSdfCircle(canvasCoord, mask.params.xy, mask.params.z), emsPerPixel);
    }
    else if (type == 2u) // rectangle: params.xy = minimum, params.zw = size
    {
        vec2 center = mask.params.xy + mask.params.zw * 0.5;
        maskFill = SlugMaskDistanceCoverage(SlugSdfBox(canvasCoord, center, mask.params.zw * 0.5), emsPerPixel);
    }
    else if (type == 3u) // capsule
    {
        maskFill = SlugMaskDistanceCoverage(SlugSdfCapsule(canvasCoord, mask.params.xy, mask.params.zw, mask.params2.x), emsPerPixel);
    }
    else if (type == 4u) // filled arc/pie
    {
        maskFill = SlugMaskDistanceCoverage(SlugSdfPie(canvasCoord, mask.params.xy, mask.params.z, mask.params.w, mask.params2.x), emsPerPixel);
    }
    else if (type == 5u) // stroked arc band
    {
        maskFill = SlugMaskDistanceCoverage(SlugSdfArcBand(canvasCoord, mask.params.xy, mask.params.z, mask.params.w, mask.params2.x, mask.params2.y), emsPerPixel);
    }
    else if (type == 6u) // hexagon
    {
        maskFill = SlugMaskDistanceCoverage(SlugSdfHexagon(canvasCoord, mask.params.xy, mask.params.z, mask.params.w), emsPerPixel);
    }
    else if (type == 7u) // octagon
    {
        maskFill = SlugMaskDistanceCoverage(SlugSdfOctagon(canvasCoord, mask.params.xy, mask.params.z, mask.params.w), emsPerPixel);
    }
    else if (type == 8u) // star
    {
        maskFill = SlugMaskDistanceCoverage(SlugSdfStar(canvasCoord, mask.params.xy, mask.params.z, mask.params.w, mask.params2.x, mask.params2.y), emsPerPixel);
    }
    else if (type == 9u) // analytic Slug shape in composite coordinates
    {
        uint shapeIndex = mask.state.y;
        if (shapeIndex < shapes.length())
        {
            vec2 maskCoord = vec2(
                dot(canvasCoord, mask.params.xy) + mask.params.z,
                canvasCoord.x * mask.params.w + canvasCoord.y * mask.params2.x + mask.params2.y);
            SlugShapeMetadata shape = shapes[shapeIndex];
            maskFill = SlugRender(maskCoord, shape.bandTransform, shape.bandFlags.xy, shape.bandAddress, shape.bandFlags);
        }
    }

    if (mask.state.z != 0u) maskFill = 1.0 - maskFill;
    return clamp(maskFill * clamp(layerMaskMaterial.x, 0.0, 1.0), 0.0, 1.0);
}

vec4 SlugSampleGradient(uint gradientIndex, vec2 coord, vec4 base)
{
    if (gradientIndex >= gradients.length()) return base;
    SlugGradientDescription gradient = gradients[gradientIndex];
    uint first = gradient.stopRange.x;
    uint count = min(gradient.stopRange.y, 256u);
    if (count == 0u || first >= stops.length()) return base;

    float t;
    if (gradient.stopRange.z == 0u) // linear
    {
        vec2 direction = gradient.params.zw;
        t = dot(coord - gradient.params.xy, direction) / max(dot(direction, direction), 1.0e-6);
    }
    else if (gradient.stopRange.z == 1u) // radial
    {
        vec2 radii = max(abs(gradient.params.zw), vec2(1.0e-6));
        t = length((coord - gradient.params.xy) / radii);
    }
    else if (gradient.stopRange.z == 2u) // sweep
    {
        float angle = atan(coord.y - gradient.params.y, coord.x - gradient.params.x);
        t = (angle - gradient.params.z) / (6.28318530718) * gradient.params.w;
    }
    else // focal radial
    {
        vec2 center0 = gradient.params.xy;
        vec2 center1 = gradient.params2.xy;
        float radius0 = gradient.params.z;
        float radius1 = gradient.params.w;
        vec2 q = coord - center0;
        vec2 deltaCenter = center1 - center0;
        float deltaRadius = radius1 - radius0;
        float a = dot(deltaCenter, deltaCenter) - deltaRadius * deltaRadius;
        float b = -2.0 * (dot(q, deltaCenter) + radius0 * deltaRadius);
        float c = dot(q, q) - radius0 * radius0;
        if (abs(a) < 1.0e-7)
        {
            t = abs(b) < 1.0e-7 ? 0.0 : -c / b;
        }
        else
        {
            float discriminant = max(b * b - 4.0 * a * c, 0.0);
            float root = sqrt(discriminant);
            float t0 = (-b - root) / (2.0 * a);
            float t1 = (-b + root) / (2.0 * a);
            bool valid0 = radius0 + t0 * deltaRadius >= 0.0;
            bool valid1 = radius0 + t1 * deltaRadius >= 0.0;
            t = valid0 && valid1 ? max(t0, t1) : (valid0 ? t0 : (valid1 ? t1 : 0.0));
        }
    }
    if (gradient.stopRange.w == 1u) t = fract(t);
    else if (gradient.stopRange.w == 2u) t = 1.0 - abs(mod(t, 2.0) - 1.0);
    else t = clamp(t, 0.0, 1.0);

    SlugGradientStop previous = stops[first];
    SlugGradientStop next = previous;
    for (uint i = 0u; i < count; i++)
    {
        uint stopIndex = first + i;
        if (stopIndex >= stops.length()) break;
        next = stops[stopIndex];
        if (t <= next.offset.x)
        {
            float span = max(next.offset.x - previous.offset.x, 1.0e-6);
            float localT = clamp((t - previous.offset.x) / span, 0.0, 1.0);
            vec4 sampled = mix(previous.color, next.color, localT);
            sampled.a *= base.a;
            return sampled;
        }
        previous = next;
    }
    vec4 sampled = next.color;
    sampled.a *= base.a;
    return sampled;
}

vec4 SlugSampleEnvironmentFace(uint baseSlot, uint face, vec2 faceUv, float lod)
{
    return textureLod(
        fillTextures[int(min(baseSlot + face, 7u))],
        faceUv * 0.5 + 0.5,
        lod);
}

vec4 SlugSampleEnvironmentCube(uint baseSlot, vec3 direction, float lod)
{
    vec3 axis = max(abs(direction), vec3(1.0e-6));
    uint faceX = direction.x >= 0.0 ? 0u : 1u;
    uint faceY = direction.y >= 0.0 ? 2u : 3u;
    uint faceZ = direction.z >= 0.0 ? 4u : 5u;
    vec2 uvX =
        direction.x >= 0.0 ?
        vec2(-direction.z, -direction.y) / axis.x :
        vec2(direction.z, -direction.y) / axis.x;
    vec2 uvY =
        direction.y >= 0.0 ?
        vec2(direction.x, direction.z) / axis.y :
        vec2(direction.x, -direction.z) / axis.y;
    vec2 uvZ =
        direction.z >= 0.0 ?
        vec2(direction.x, -direction.y) / axis.z :
        vec2(-direction.x, -direction.y) / axis.z;
    vec3 weights = pow(axis, vec3(32.0));
    weights /= max(weights.x + weights.y + weights.z, 1.0e-6);
    return
        SlugSampleEnvironmentFace(baseSlot, faceX, uvX, lod) * weights.x +
        SlugSampleEnvironmentFace(baseSlot, faceY, uvY, lod) * weights.y +
        SlugSampleEnvironmentFace(baseSlot, faceZ, uvZ, lod) * weights.z;
}

vec3 SlugSafeNormalize(vec3 value)
{
    float lengthSquared = dot(value, value);
    return value * inversesqrt(max(lengthSquared, 1.0e-8));
}

vec3 SlugSrgbToLinear(vec3 color)
{
    bvec3 cutoff = lessThanEqual(color, vec3(0.04045));
    vec3 lower = color / 12.92;
    vec3 higher = pow((color + 0.055) / 1.055, vec3(2.4));
    return mix(higher, lower, cutoff);
}

vec3 SlugFresnelSchlick(float cosine, vec3 f0)
{
    return f0 + (1.0 - f0) * pow(1.0 - clamp(cosine, 0.0, 1.0), 5.0);
}

float SlugDistributionGgx(float normalDotHalf, float roughness)
{
    float alpha = roughness * roughness;
    float alphaSquared = alpha * alpha;
    float denominator = normalDotHalf * normalDotHalf * (alphaSquared - 1.0) + 1.0;
    return alphaSquared / max(3.14159265 * denominator * denominator, 1.0e-6);
}

float SlugGeometrySchlickGgx(float normalDotDirection, float roughness)
{
    float radius = roughness + 1.0;
    float k = radius * radius * 0.125;
    return normalDotDirection / max(normalDotDirection * (1.0 - k) + k, 1.0e-6);
}

float SlugGeometrySmith(float normalDotView, float normalDotLight, float roughness)
{
    return SlugGeometrySchlickGgx(normalDotView, roughness) *
           SlugGeometrySchlickGgx(normalDotLight, roughness);
}

// Lazarov's split-sum environment BRDF fit, as popularized by the UE4
// real-time PBR workflow. It retains the view/roughness response without
// requiring a dedicated BRDF LUT texture.
vec3 SlugEnvironmentBrdf(vec3 f0, float roughness, float normalDotView)
{
    const vec4 c0 = vec4(-1.0, -0.0275, -0.572, 0.022);
    const vec4 c1 = vec4(1.0, 0.0425, 1.04, -0.04);
    vec4 r = roughness * c0 + c1;
    float a004 = min(r.x * r.x, exp2(-9.28 * normalDotView)) * r.x + r.y;
    vec2 ab = vec2(-1.04, 1.04) * a004 + r.zw;
    return max(f0 * ab.x + ab.y, vec3(0.0));
}

// Khronos PBR Neutral keeps highlight compression mostly hue-preserving,
// avoiding the orange shift of filmic curves and the desaturation of
// component-wise Reinhard.
vec3 SlugTonemapPbrNeutral(vec3 color)
{
    const float startCompression = 0.76;
    const float desaturation = 0.15;
    float darkest = min(color.r, min(color.g, color.b));
    float offset = darkest < 0.08 ? darkest - 6.25 * darkest * darkest : 0.04;
    color -= offset;
    float peak = max(color.r, max(color.g, color.b));
    if (peak < startCompression) return color;
    float distance = 1.0 - startCompression;
    float newPeak = 1.0 - distance * distance / (peak + distance - startCompression);
    color *= newPeak / max(peak, 1.0e-6);
    float desaturationAmount =
        1.0 - 1.0 / (desaturation * (peak - newPeak) + 1.0);
    return mix(color, vec3(newPeak), desaturationAmount);
}

float SlugRayScene(vec3 point, float seconds)
{
    // A bounded, deterministic signed-distance sphere with a small animated
    // surface ripple.  The ripple is intentionally finite so every march
    // remains stable for all fragment coordinates and times.
    float c = cos(seconds * 0.65);
    float s = sin(seconds * 0.65);
    vec3 q = vec3(point.x * c - point.z * s, point.y, point.x * s + point.z * c);
    float sphere = length(q) - 0.62;
    float ripple = 0.035 * sin(q.x * 6.0 + q.z * 5.0 + seconds * 1.7);
    return sphere + ripple;
}

vec3 SlugRaymarchCircle(vec2 uv, float seconds)
{
    vec2 screen = uv * 2.0 - 1.0;
    vec3 rayOrigin = vec3(0.0, 0.0, 2.15);
    vec3 rayDirection = SlugSafeNormalize(vec3(screen, -1.8));
    float distanceAlongRay = 0.0;
    float hit = 0.0;
    vec3 hitPoint = vec3(0.0);

    for (int step = 0; step < 48; step++)
    {
        vec3 point = rayOrigin + rayDirection * distanceAlongRay;
        float signedDistance = SlugRayScene(point, seconds);
        if (abs(signedDistance) < 0.0015)
        {
            hit = 1.0;
            hitPoint = point;
            break;
        }
        distanceAlongRay += clamp(signedDistance * 0.72, 0.004, 0.18);
        if (distanceAlongRay > 4.0) break;
    }

    if (hit == 0.0)
    {
        float backdrop = clamp(0.5 + 0.5 * screen.y, 0.0, 1.0);
        return mix(vec3(0.012, 0.018, 0.040), vec3(0.080, 0.130, 0.240), backdrop);
    }

    const float normalStep = 0.003;
    vec3 normal = SlugSafeNormalize(vec3(
        SlugRayScene(hitPoint + vec3(normalStep, 0.0, 0.0), seconds) - SlugRayScene(hitPoint - vec3(normalStep, 0.0, 0.0), seconds),
        SlugRayScene(hitPoint + vec3(0.0, normalStep, 0.0), seconds) - SlugRayScene(hitPoint - vec3(0.0, normalStep, 0.0), seconds),
        SlugRayScene(hitPoint + vec3(0.0, 0.0, normalStep), seconds) - SlugRayScene(hitPoint - vec3(0.0, 0.0, normalStep), seconds)));
    vec3 viewDirection = SlugSafeNormalize(rayOrigin - hitPoint);
    vec3 lightDirection = SlugSafeNormalize(vec3(-0.45, 0.75, 1.2));
    float diffuse = max(dot(normal, lightDirection), 0.0);
    vec3 halfDirection = SlugSafeNormalize(lightDirection + viewDirection);
    float specular = pow(max(dot(normal, halfDirection), 0.0), 32.0);
    float rim = pow(1.0 - max(dot(normal, viewDirection), 0.0), 2.0);
    return clamp(vec3(0.10, 0.28, 0.58) * (0.22 + diffuse * 0.78) + vec3(0.20, 0.42, 0.95) * specular + vec3(0.06, 0.18, 0.35) * rim, vec3(0.0), vec3(1.0));
}

vec4 SlugFill(vec2 coord, vec2 composite, vec2 uv, vec2 gradient, vec4 base, uvec4 ids, uvec4 resources, vec4 params, vec4 params2, vec4 material)
{
    uint fillId = ids.y;
    vec4 result = base;
    if (fillId == 0u) // solid
    {
        result = base;
    }
    else if (fillId == 1u) // linear gradient
    {
        result = SlugSampleGradient(ids.x + 0u, gradient, base);
    }
    else if (fillId == 2u) // radial gradient
    {
        result = SlugSampleGradient(ids.x + 0u, gradient, base);
    }
    else if (fillId == 3u) // sweep gradient
    {
        result = SlugSampleGradient(ids.x + 0u, gradient, base);
    }
    else if (fillId == 4u) // sampled texture array
    {
        vec4 texel = texture(fillTextures[int(min(resources.y, 7u))], uv);
        result = vec4(mix(base.rgb, texel.rgb, texel.a), base.a * texel.a);
    }
    else if (fillId == 5u) // antialiased checker
    {
        vec2 cells = coord * max(abs(params.xy), vec2(8.0));
        float checker = mod(floor(cells.x) + floor(cells.y), 2.0);
        result.rgb = mix(base.rgb, min(base.rgb + vec3(0.25), vec3(1.0)), checker);
    }
    else if (fillId == 6u) // pixel grid
    {
        vec2 cells = uv * max(abs(params.xy), vec2(16.0));
        vec2 f = fract(cells);
        vec2 edge = min(f, 1.0 - f);
        vec2 width = max(fwidth(cells), vec2(1.0e-4));
        float line = max(1.0 - smoothstep(vec2(0.0), width * 1.5, edge).x, 1.0 - smoothstep(vec2(0.0), width * 1.5, edge).y);
        result.rgb = mix(base.rgb, base.rgb * 0.5, clamp(line, 0.0, 1.0));
    }
    else if (fillId == 7u) // stripe
    {
        float stripe = 0.5 + 0.5 * sin((uv.x + uv.y) * max(abs(params.x), 4.0) + params.y);
        result.rgb = mix(base.rgb * 0.55, min(base.rgb + vec3(0.2), vec3(1.0)), stripe);
    }
    else if (fillId == 8u) // reveal
    {
        float progress = clamp(params.x + params.y * view.time.x, 0.0, 1.0);
        float edge = fwidth(uv.x) * 1.5 + 1.0e-4;
        result.a *= smoothstep(progress - edge, progress + edge, uv.x);
    }
    else if (fillId == 9u) // halo
    {
        float edge = min(min(uv.x, uv.y), min(1.0 - uv.x, 1.0 - uv.y));
        float halo = exp(-max(edge, 0.0) * max(abs(params.x), 8.0));
        result.rgb = min(base.rgb + vec3(halo * 0.4), vec3(1.0));
        result.a *= clamp(halo + 0.35, 0.0, 1.0);
    }
    else if (fillId == 10u) // outline-compatible fill
    {
        float edge = min(min(uv.x, uv.y), min(1.0 - uv.x, 1.0 - uv.y));
        float width = max(fwidth(edge) * max(abs(params.x), 2.0), 1.0e-4);
        result.a *= 1.0 - smoothstep(width, width * 2.0, edge);
    }
    else if (fillId == 11u) // wave fill
    {
        float wave = 0.5 + 0.5 * sin(uv.x * max(abs(params.y), 4.0) + view.time.x * params.x);
        result.rgb = mix(base.rgb * 0.45, min(base.rgb + vec3(0.3), vec3(1.0)), wave);
    }
    else if (fillId == 12u) // pulse fill
    {
        float pulse = 0.75 + 0.25 * sin(view.time.x * max(abs(params.x), 0.25) + params.y);
        result.rgb *= pulse;
        result.a *= pulse;
    }
    else if (fillId == 13u) // interference fill
    {
        float interference = 0.5 + 0.5 * sin((uv.x + view.time.x * params.x) * max(abs(params.y), 4.0)) * cos(uv.y * max(abs(params.z), 4.0));
        result.rgb = mix(base.rgb * 0.4, min(base.rgb + vec3(0.35), vec3(1.0)), interference);
    }
    else if (fillId == 14u) // radial procedural fill
    {
        float radial = 0.5 + 0.5 * sin(length(uv - vec2(0.5)) * max(abs(params.x), 12.0) - view.time.x * params.y);
        result.rgb = mix(base.rgb * 0.35, min(base.rgb + vec3(0.25), vec3(1.0)), radial);
    }
    else if (fillId == 15u) // spiral procedural fill
    {
        float angle = atan(uv.y - 0.5, uv.x - 0.5);
        float spiral = 0.5 + 0.5 * sin(angle * max(abs(params.x), 3.0) + length(uv - vec2(0.5)) * max(abs(params.y), 20.0) - view.time.x);
        result.rgb = mix(base.rgb * 0.25, min(base.rgb + vec3(0.45), vec3(1.0)), spiral);
    }
    else if (fillId == 17u) // canonical compute-demo raymarched circle fill.
    {
        // Keep the contour evaluator in charge of coverage; this color-only
        // fill leaves base alpha untouched for the final analytic mask.
        result.rgb = SlugRaymarchCircle(uv, view.time.x);
        result.a = base.a;
    }
    else if (fillId == 16u) // metallic GGX material with environment + orbiting lights.
    {
        // The analytic contour owns silhouette coverage. Its normalized bounds
        // supply a smooth hemispherical normal across the circular badge.
        vec2 p = uv * 2.0 - 1.0;
        float radiusSquared = clamp(dot(p, p), 0.0, 0.9999);
        float normalZ = sqrt(max(1.0 - radiusSquared, 1.0e-4));
        vec3 normal = SlugSafeNormalize(vec3(p, normalZ));
        vec3 viewDirection = vec3(0.0, 0.0, 1.0);
        float normalDotView = clamp(dot(normal, viewDirection), 0.0, 1.0);

        float metallic = material.x == material.x ? clamp(material.x, 0.0, 1.0) : 1.0;
        float roughness = material.y == material.y ? clamp(material.y, 0.04, 1.0) : 0.08;
        float directIntensity =
            material.z == material.z ? max(material.z, 0.0) : 0.0;

        // Render2d color textures are UNORM rather than hardware-sRGB, so both
        // the authored material tint and PNG environment must be decoded before
        // participating in BRDF math.
        vec3 baseColor = SlugSrgbToLinear(clamp(base.rgb, vec3(0.0), vec3(1.0)));
        vec3 f0 = mix(vec3(0.04), baseColor, metallic);
        vec3 reflection = SlugSafeNormalize(reflect(-viewDirection, normal));
        vec3 environmentRadiance = SlugSrgbToLinear(clamp(
            SlugSampleEnvironmentCube(resources.y, reflection, roughness * 6.0).rgb,
            vec3(0.0),
            vec3(1.0)));
        if (any(isnan(environmentRadiance)) || any(isinf(environmentRadiance)))
        {
            environmentRadiance = vec3(0.0);
        }

        // The six dedicated cube faces remain the actual radiance source.
        // Their mip chain approximates GGX prefiltering, while the analytic
        // split-sum fit supplies the view-dependent environment BRDF term.
        vec3 color =
            environmentRadiance *
            SlugEnvironmentBrdf(f0, roughness, normalDotView);

        // Non-metals retain a small image-based diffuse lobe. The showcase
        // badge is fully metallic, so this term correctly vanishes there.
        if (metallic < 1.0)
        {
            vec3 diffuseRadiance = SlugSrgbToLinear(clamp(
                SlugSampleEnvironmentCube(resources.y, normal, 6.0).rgb,
                vec3(0.0),
                vec3(1.0)));
            vec3 viewFresnel = SlugFresnelSchlick(normalDotView, f0);
            vec3 diffuseWeight = (1.0 - viewFresnel) * (1.0 - metallic);
            color += diffuseRadiance * baseColor * diffuseWeight;
        }

        vec2 normalizedPosition = (composite - params2.xy) * params2.z;
        vec3 position = vec3(normalizedPosition, 0.0);
        float seconds = view.time.x;

        // Match the source example's animated warm, cool, and magenta point
        // lights. A finite roughness floor models small area lights and keeps
        // their GGX highlights stable instead of sub-pixel singularities.
        float lightRoughness = max(roughness, 0.15);
        for (int i = 0; i < 3; i++)
        {
            float orbit = seconds * (i == 0 ? 0.90 : (i == 1 ? 1.13 : 0.73)) +
                (i == 0 ? 0.0 : (i == 1 ? 2.0943951 : 4.1887902));
            vec3 lightPosition = i == 0
                ? vec3(cos(orbit) * 1.8, sin(orbit) * 1.1 + 0.35, 1.35)
                : (i == 1
                    ? vec3(cos(orbit) * 1.45, sin(orbit) * 0.9 - 0.15, 1.05)
                    : vec3(cos(orbit) * 2.0, sin(orbit) * 1.3 + 0.20, 0.90));
            vec3 lightColor = i == 0
                ? vec3(1.00, 0.95, 0.80)
                : (i == 1
                    ? vec3(0.55, 0.70, 1.00)
                    : vec3(1.00, 0.45, 0.70));
            vec3 toLight = lightPosition - position;
            float distanceSquared = max(dot(toLight, toLight), 0.25);
            vec3 lightDirection = SlugSafeNormalize(toLight);
            float normalDotLight = max(dot(normal, lightDirection), 0.0);
            if (normalDotLight <= 0.0) continue;

            vec3 halfDirection = SlugSafeNormalize(lightDirection + viewDirection);
            float normalDotHalf = max(dot(normal, halfDirection), 0.0);
            float viewDotHalf = max(dot(viewDirection, halfDirection), 0.0);
            float distribution = SlugDistributionGgx(normalDotHalf, lightRoughness);
            float geometry =
                SlugGeometrySmith(normalDotView, normalDotLight, lightRoughness);
            vec3 fresnel = SlugFresnelSchlick(viewDotHalf, f0);
            vec3 specular =
                distribution * geometry * fresnel /
                max(4.0 * normalDotView * normalDotLight, 1.0e-5);
            vec3 diffuseWeight = (1.0 - fresnel) * (1.0 - metallic);
            vec3 brdf = diffuseWeight * baseColor / 3.14159265 + specular;
            color +=
                brdf * lightColor *
                (directIntensity / distanceSquared) * normalDotLight;
        }

        color = SlugTonemapPbrNeutral(max(color, vec3(0.0)));
        color = pow(clamp(color, vec3(0.0), vec3(1.0)), vec3(1.0 / 2.2));
        // Analytic Slug coverage is multiplied after this material returns.
        result = vec4(color, base.a);
    }
    return result;
}

vec4 SlugEffectFill(vec4 fill, vec2 coord, vec2 uv, uvec4 ids, vec4 params, vec4 params2)
{
    uint effectId = ids.z;
    if (effectId == 1u) // wave color/effect
    {
        float wave = 0.5 + 0.5 * sin(coord.x * max(abs(params.y), 2.0) + view.time.x * params.x);
        fill.rgb = mix(fill.rgb * 0.55, min(fill.rgb + vec3(0.25), vec3(1.0)), wave);
    }
    else if (effectId == 2u) // pulse alpha/effect
    {
        float pulse = 0.75 + 0.25 * sin(view.time.x * max(abs(params.y), 0.25) + params.x);
        fill.rgb *= pulse;
        fill.a *= pulse;
    }
    else if (effectId == 3u) // rotate effect also modulates highlights, while vertex rotates geometry
    {
        float highlight = 0.85 + 0.15 * cos(view.time.x + atan(coord.y, coord.x));
        fill.rgb *= highlight;
    }
    else if (effectId == 4u) // reveal-compatible alpha gate
    {
        float progress = clamp(params.x + params.y * view.time.x, 0.0, 1.0);
        fill.a *= smoothstep(progress - fwidth(uv.x), progress + fwidth(uv.x), uv.x);
    }
    else if (effectId == 5u) // morph-compatible shimmer
    {
        float shimmer = 0.5 + 0.5 * sin((coord.x + coord.y) * 8.0 + params2.x + view.time.x * params2.y);
        fill.rgb = mix(fill.rgb, min(fill.rgb + vec3(0.35), vec3(1.0)), shimmer * 0.35);
    }
    else if (effectId == 6u) // spiral effect
    {
        float spiral = sin(length(coord) * max(abs(params.y), 1.0) + atan(coord.y, coord.x) * params.x + view.time.x);
        fill.rgb *= 0.78 + 0.22 * spiral;
    }
    else if (effectId == 7u) // interference effect
    {
        float interference = sin(coord.x * params.x + view.time.x) * cos(coord.y * params.y - view.time.x);
        fill.rgb *= 0.75 + 0.25 * interference;
    }
    else if (effectId == 8u) // radial effect
    {
        float radial = sin(length(coord) * max(abs(params.x), 1.0) - view.time.x * params.y);
        fill.rgb *= 0.8 + 0.2 * radial;
    }
    else if (effectId == 9u) // warm vertical treatment used by the text-effects demo
    {
        float vertical = clamp(uv.y, 0.0, 1.0);
        fill.rgb = mix(vec3(1.00, 0.76, 0.08), vec3(0.84, 0.24, 0.01), vertical);
    }
    else if (effectId == 10u) // chipped pigment used by the text-effects demo
    {
        float seed = params.z;
        float grain =
            sin(uv.x * 13.0 + sin(uv.y * 7.0 + seed) * 2.2) *
            cos(uv.y * 11.0 + sin(uv.x * 5.0 - seed) * 2.0);
        float fracture = sin((uv.x + uv.y) * 29.0 + seed * 1.7) * 0.24;
        float pigment = smoothstep(-0.55, -0.08, grain + fracture);
        float colorNoise = 0.5 + 0.5 * sin(uv.x * 11.0 - uv.y * 9.0 + seed);
        fill.rgb = mix(vec3(0.12, 0.15, 0.48), vec3(0.30, 0.23, 0.64), colorNoise);
        fill.a *= pigment;
    }
    return fill;
}

void main()
{
    // Mask coordinates are composite/world coordinates, not the current layer's
    // rectangle. This keeps one mask coherent across differently transformed layers.
    vec2 emsPerPixel = max(fwidth(emCoord), vec2(1.0e-7));
    float maskCoverage = SlugMaskCoverageFor(compositeCoord, layerState, emsPerPixel);
    if (maskCoverage <= 0.0) discard;

    uvec4 fillIds = uvec4(layerResourceIds.x, layerState.y, layerResourceIds.z, layerResourceIds.w);
    vec4 fill = SlugFill(emCoord, compositeCoord, shapeUv, gradientCoord, layerColor, fillIds, layerResourceIds, layerEffectParams, layerEffectParams2, layerMaskMaterial);
    fill = SlugEffectFill(fill, emCoord, shapeUv, layerState, layerEffectParams, layerEffectParams2);

    // The only shape boundary is the analytic curve/band evaluator. The quad is
    // merely conservative raster geometry and contributes no implicit alpha.
    float slugCoverage = SlugRender(emCoord, shapeBandTransform, shapeBandFlags.xy, shapeBandAddress, shapeBandFlags);
    float alpha = clamp(maskCoverage * slugCoverage, 0.0, 1.0);
    fragmentColor = fill * alpha;
}
