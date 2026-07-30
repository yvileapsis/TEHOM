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

// Shared SlugShape std430 ABI. SlugShape.frag and SlugShape.comp intentionally
// repeat these declarations byte-for-byte; changing a member requires changing
// all three stages and the managed upload structs together.
//
// Binding 0: view + projection + viewport + deterministic clock/counts.
// Binding 1: mutable per-frame SlugLayerState[] (compute writes this same SSBO).
// Binding 2: immutable SlugShapeMetadata[] (bounds and curve/band addressing).
// Bindings 3/4: exact nearest-filtered quadratic-curve and band-index samplers.
// Bindings 5/6: gradient descriptions and variable-range stop arrays.
// Binding 7: composite-coordinate mask descriptors.
// Binding 8: optional fill/MSDF texture array.
// Binding 9: deterministic compute dispatch configuration.

struct SlugViewState
{
    mat4 viewProjection;
    vec4 viewport;       // xy = pixel size, zw = reciprocal pixel size.
    vec4 time;           // x = seconds, y = delta, z = frame, w = deterministic seed.
    uvec4 counts;         // x = graphics layer count, y = shape count, z/w reserved.
};

struct SlugLayerState
{
    mat4 transform;       // local em coordinates -> composite/world coordinates.
    vec4 color;           // linear RGBA base color.
    vec4 origin;          // xy = em pivot/composite origin, zw = layer scale/phase.
    uvec4 stateIds;        // x shape, y fill, z effect, w mask (0xffffffff = none).
    uvec4 resourceIds;     // x gradient, y fill texture, z material, w reserved.
    vec4 effectParams;     // x/y primary effect parameters, z/w effect auxiliaries.
    vec4 effectParams2;    // x/y secondary effect parameters, z/w morph/reveal addressing.
    mat4 gradientTransform;// local/composite coordinates -> gradient coordinates.
    vec4 maskMaterial;    // x mask opacity, y roughness, z metallic, w fill texture slot.
};

struct SlugShapeMetadata
{
    vec4 bounds;          // xy = minimum, zw = maximum in em coordinates.
    vec4 bandTransform;   // xy = band scale, zw = band offset.
    uvec4 bandAddress;    // xy = header origin, zw = curve-list origin in band texture.
    uvec4 bandFlags;      // x/y = band maxima, z = band texture row width, w = fill flags.
};

struct SlugGradientDescription
{
    uvec4 stopRange;      // x = first stop, y = count, z = 0 linear/1 radial/2 sweep, w = flags.
    vec4 params;          // linear: origin.xy + direction.zw; radial: center.xy + radii.zw; sweep: center.xy/start.z/scale.w.
    vec4 params2;         // focal radial: center1.xy; reserved for other kinds.
};

struct SlugGradientStop
{
    vec4 color;
    vec4 offset;
};

struct SlugMaskState
{
    vec4 params;          // type-specific parameters (MSDF/circle/rect/capsule/arc).
    vec4 params2;         // type-specific parameters (arc-band/hex/octagon/star).
    uvec4 state;          // x type, y MSDF texture layer, z invert (0/1), w flags.
};

struct SlugComputeConfig
{
    uvec4 dispatch;       // x layer count, y mode (radial/spiral/interference), z flags, w seed.
    vec4 params0;         // speed, amplitude, frequency, phase.
    vec4 params1;         // mode-specific center/scale controls.
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

const int SLUG_QUAD_VERTICES = 6;
const vec2 SLUG_CORNERS[SLUG_QUAD_VERTICES] = vec2[SLUG_QUAD_VERTICES](
    vec2(0.0, 0.0), vec2(1.0, 0.0), vec2(1.0, 1.0),
    vec2(1.0, 1.0), vec2(0.0, 1.0), vec2(0.0, 0.0));
const float SLUG_CONSERVATIVE_PAD = 0.01;

layout(location = 0) out vec2 emCoord;
layout(location = 1) out vec2 shapeUv;
layout(location = 2) out vec2 compositeCoord;
layout(location = 3) out vec2 gradientCoord;
layout(location = 4) flat out vec4 layerColor;
layout(location = 5) flat out uvec4 layerState;
layout(location = 6) flat out vec4 layerEffectParams;
layout(location = 7) flat out vec4 layerEffectParams2;
layout(location = 8) flat out vec4 layerMaskMaterial;
layout(location = 9) flat out vec4 shapeBandTransform;
layout(location = 10) flat out uvec4 shapeBandAddress;
layout(location = 11) flat out uvec4 shapeBandFlags;
layout(location = 12) flat out uvec4 layerResourceIds;
vec2 SlugRotate2d(vec2 p, vec2 pivot, float angle)
{
    float c = cos(angle);
    float s = sin(angle);
    vec2 q = p - pivot;
    return pivot + vec2(q.x * c - q.y * s, q.x * s + q.y * c);
}

vec2 SlugApplyVertexEffect(
    vec2 local,
    SlugShapeMetadata shape,
    SlugLayerState layer,
    float seconds)
{
    uint effectId = layer.stateIds.z;
    vec2 size = max(shape.bounds.zw - shape.bounds.xy, vec2(1.0e-5));
    vec2 pivot = layer.origin.xy;
    float a = layer.effectParams.x;
    float b = layer.effectParams.y;
    float c = layer.effectParams2.x;
    float d = layer.effectParams2.y;

    if (effectId == 1u) // wave: vertical displacement in em space.
    {
        float frequency = max(abs(b), 0.25);
        local.y += sin((local.x + seconds * max(abs(d), 0.1)) * frequency + c) * a;
    }
    else if (effectId == 2u) // pulse: uniform pivot-relative breathing.
    {
        float frequency = max(abs(b), 0.25);
        float scale = 1.0 + a * sin(seconds * frequency + c);
        local = pivot + (local - pivot) * max(scale, 0.01);
    }
    else if (effectId == 3u) // rotate: actual per-layer pivot rotation.
    {
        local = SlugRotate2d(local, pivot, a + seconds * b);
    }
    else if (effectId == 4u) // reveal: slide the em domain while the fill clips it.
    {
        float progress = clamp(a + seconds * b, 0.0, 1.0);
        local.x += (progress - 0.5) * size.x * 0.15;
    }
    else if (effectId == 5u) // morph-compatible addressing: map toward a target shape bbox.
    {
        uint targetIndex = uint(max(layer.effectParams2.w, 0.0) + 0.5);
        float amount = clamp(c + seconds * d, 0.0, 1.0);
        if (targetIndex < shapes.length())
        {
            SlugShapeMetadata target = shapes[targetIndex];
            vec2 uv = clamp((local - shape.bounds.xy) / size, vec2(0.0), vec2(1.0));
            vec2 targetSize = max(target.bounds.zw - target.bounds.xy, vec2(1.0e-5));
            vec2 targetLocal = target.bounds.xy + uv * targetSize;
            local = mix(local, targetLocal, amount);
        }
        else
        {
            float frequency = max(abs(layer.effectParams.y), 0.25);
            local += vec2(sin(seconds + local.y * frequency), cos(seconds + local.x * frequency)) * a;
        }
    }
    else if (effectId == 6u) // spiral: radius-dependent angular deformation.
    {
        vec2 q = local - pivot;
        float radius = length(q);
        local = pivot + SlugRotate2d(q, vec2(0.0), a * radius + seconds * b);
    }
    else if (effectId == 7u) // interference: two deterministic waves in orthogonal axes.
    {
        local.x += sin(local.y * max(abs(b), 0.25) + seconds * d) * a;
        local.y += cos(local.x * max(abs(c), 0.25) - seconds * b) * a;
    }
    else if (effectId == 8u) // radial: pulsing radial displacement around origin.
    {
        vec2 q = local - pivot;
        float radius = length(q);
        float displacement = sin(radius * max(abs(b), 0.25) - seconds * d + c) * a;
        local += (radius > 1.0e-5 ? q / radius : vec2(0.0)) * displacement;
    }
    else if (effectId == 20u) // historical osgslug-font-animation effect shown in the reference media.
    {
        float u = local.x;
        float v = local.y;
        float wave = sin(u * 6.28318 * 2.0 - seconds * 3.0);
        float center = clamp(1.0 - abs(v - 0.5) * 2.0, 0.0, 1.0);
        center *= center;
        local.y += wave * center * 0.3;
        local.x += (u - 0.5) * sin(seconds) * 0.3;
    }
    else if (effectId == 21u) // canonical simple-animation triangle morph.
    {
        // The second equation intentionally observes the first displacement.
        local.x += sin(local.y * 6.0 + seconds * 2.0) * 0.2;
        local.y += sin(local.x * 4.0 + seconds * 1.5) * 0.1;
    }
    else if (effectId == 22u) // canonical twelve-pill horizontal width animation.
    {
        float i = layer.effectParams.x;
        const float barWidth = 0.82;
        float amp = 0.5 + 0.5 * sin(seconds * 4.0 + i * 0.7);
        float sx = 0.15 + amp * 0.85;
        float uvX = clamp((local.x - shape.bounds.x) / size.x, 0.0, 1.0);
        float leftX = local.x - uvX * barWidth;
        const float capFrac = 0.122;
        float capW = capFrac * barWidth;
        float bodyW = (1.0 - 2.0 * capFrac) * barWidth;

        // This is the original 9-slice equation.  The analytic renderer uses
        // a conservative six-vertex quad, so its endpoint vertices also carry
        // a left-anchored fallback scale to expose the same width animation;
        // a subdivided caller naturally takes the exact body branch.
        if (uvX > (1.0 - capFrac))
        {
            float localT = (uvX - (1.0 - capFrac)) / capFrac;
            local.x = leftX + capW + sx * bodyW + localT * capW;
        }
        else if (uvX > capFrac)
        {
            float bodyT = (uvX - capFrac) / (1.0 - 2.0 * capFrac);
            local.x = leftX + capW + bodyT * sx * bodyW;
        }
        else
        {
            // Keep the left cap fixed while the right endpoint follows the
            // animated width when only the conservative quad is available.
            local.x = shape.bounds.x + (local.x - shape.bounds.x) * sx;
        }
    }

    return local;
}

void main()
{
    uint layerIndex = gl_InstanceIndex;
    uint vertexIndex = uint(gl_VertexIndex % SLUG_QUAD_VERTICES);

    if (layerIndex >= view.counts.x || layerIndex >= layers.length())
    {
        emCoord = vec2(0.0);
        shapeUv = vec2(0.0);
        compositeCoord = vec2(0.0);
        gradientCoord = vec2(0.0);
        layerColor = vec4(0.0);
        layerState = uvec4(0u);
        layerEffectParams = vec4(0.0);
        layerEffectParams2 = vec4(0.0);
        layerMaskMaterial = vec4(0.0);
        shapeBandTransform = vec4(0.0);
        shapeBandAddress = uvec4(0u);
        shapeBandFlags = uvec4(0u);
        layerResourceIds = uvec4(0u);
        gl_Position = vec4(0.0);
        return;
    }

    SlugLayerState layer = layers[layerIndex];
    uint shapeIndex = layer.stateIds.x;
    if (shapeIndex >= shapes.length())
    {
        gl_Position = vec4(0.0);
        return;
    }
    SlugShapeMetadata shape = shapes[shapeIndex];

    vec2 corner = SLUG_CORNERS[vertexIndex];
    vec2 size = max(shape.bounds.zw - shape.bounds.xy, vec2(1.0e-5));
    // The quad is deliberately expanded in em space. It remains a conservative
    // raster bound under arbitrary affine transforms and perspective projection;
    // no interior tessellation is used.
    vec2 pad = max(size * SLUG_CONSERVATIVE_PAD, vec2(1.0e-4));
    vec2 minBound = shape.bounds.xy - pad;
    vec2 maxBound = shape.bounds.zw + pad;
    vec2 local = mix(minBound, maxBound, corner);
    vec2 effectedLocal = SlugApplyVertexEffect(local, shape, layer, view.time.x);

    vec4 world = layer.transform * vec4(effectedLocal, 0.0, 1.0);
    vec2 uv = clamp((local - shape.bounds.xy) / size, vec2(0.0), vec2(1.0));
    vec2 gradient = (layer.gradientTransform * vec4(local, 0.0, 1.0)).xy;

    emCoord = local;
    shapeUv = uv;
    compositeCoord = world.xy;
    gradientCoord = gradient;
    layerColor = layer.color;
    layerState = layer.stateIds;
    layerEffectParams = layer.effectParams;
    layerEffectParams2 = layer.effectParams2;
    layerMaskMaterial = layer.maskMaterial;
    shapeBandTransform = shape.bandTransform;
    shapeBandAddress = shape.bandAddress;
    shapeBandFlags = shape.bandFlags;
    layerResourceIds = layer.resourceIds;
    gl_Position = view.viewProjection * world;
}
