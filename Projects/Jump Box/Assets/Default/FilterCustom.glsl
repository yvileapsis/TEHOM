#shader vertex
#version 410

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords; 
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410

const float FXAA_SPAN_MAX = 8.0;
const float FXAA_REDUCE_MIN = 1.0 / 128.0;
const float FXAA_REDUCE_MUL = 1.0 / 8.0;

uniform sampler2D inputTexture;

in vec2 texCoordsOut;

layout(location = 0) out vec4 frag;

const float bayerMatrix8x8[64] = float[64](
    0.0/ 64.0, 48.0/ 64.0, 12.0/ 64.0, 60.0/ 64.0,  3.0/ 64.0, 51.0/ 64.0, 15.0/ 64.0, 63.0/ 64.0,
    32.0/ 64.0, 16.0/ 64.0, 44.0/ 64.0, 28.0/ 64.0, 35.0/ 64.0, 19.0/ 64.0, 47.0/ 64.0, 31.0/ 64.0,
    8.0/ 64.0, 56.0/ 64.0,  4.0/ 64.0, 52.0/ 64.0, 11.0/ 64.0, 59.0/ 64.0,  7.0/ 64.0, 55.0/ 64.0,
    40.0/ 64.0, 24.0/ 64.0, 36.0/ 64.0, 20.0/ 64.0, 43.0/ 64.0, 27.0/ 64.0, 39.0/ 64.0, 23.0/ 64.0,
    2.0/ 64.0, 50.0/ 64.0, 14.0/ 64.0, 62.0/ 64.0,  1.0/ 64.0, 49.0/ 64.0, 13.0/ 64.0, 61.0/ 64.0,
    34.0/ 64.0, 18.0/ 64.0, 46.0/ 64.0, 30.0/ 64.0, 33.0/ 64.0, 17.0/ 64.0, 45.0/ 64.0, 29.0/ 64.0,
    10.0/ 64.0, 58.0/ 64.0,  6.0/ 64.0, 54.0/ 64.0,  9.0/ 64.0, 57.0/ 64.0,  5.0/ 64.0, 53.0/ 64.0,
    42.0/ 64.0, 26.0/ 64.0, 38.0/ 64.0, 22.0/ 64.0, 41.0/ 64.0, 25.0/ 64.0, 37.0/ 64.0, 21.0 / 64.0
);

const vec3 palette[30] = vec3[30](
    vec3(1.0, 0.0, 0.0),  // Red
    vec3(0.0, 1.0, 0.0),  // Green
    vec3(0.0, 0.0, 1.0),  // Blue

    vec3(0.5, 0.0, 0.0),  // Red
    vec3(0.0, 0.5, 0.0),  // Green
    vec3(0.0, 0.0, 0.5),  // Blue

    vec3(0.7, 0.0, 0.0),  // Red
    vec3(0.0, 0.7, 0.0),  // Green
    vec3(0.0, 0.0, 0.7),  // Blue

    vec3(1.0, 1.0, 0.0),  // Yellow
    vec3(1.0, 0.0, 1.0),  // Magenta
    vec3(0.0, 1.0, 1.0),  // Cyan

    vec3(0.5, 0.5, 0.0),  // Yellow
    vec3(0.5, 0.0, 0.5),  // Magenta
    vec3(0.0, 0.5, 0.5),  // Cyan

    vec3(0.7, 0.7, 0.0),  // Yellow
    vec3(0.7, 0.0, 0.7),  // Magenta
    vec3(0.0, 0.7, 0.7),  // Cyan

    vec3(1.0, 0.5, 0.0),  // Orange
    vec3(0.5, 0.0, 1.0),  // Purple
    vec3(0.5, 1.0, 0.0),  // Lime

    vec3(1.0, 0.0, 0.5),  // Pink
    vec3(0.0, 0.5, 1.0),  // Sky Blue
    vec3(0.0, 1.0, 0.5),  // Mint

    vec3(0.3, 0.3, 0.3),  // White
    vec3(0.9, 0.9, 0.9),  // White
    vec3(1.0, 1.0, 1.0),  // White

    vec3(0.2, 0.2, 0.2),  // Dark gray
    vec3(0.1, 0.1, 0.1),  // Darker gray
    vec3(0.0, 0.0, 0.0)   // Black
);
const int paletteLength = 30;


vec3 hsl2rgb(vec3 c)
{
    vec3 rgb = clamp(abs(mod(c.x*6.+vec3(0.,4.,2.),6.)-3.)-1.,0.,1.);

    return c.z + c.y * (rgb - .5) * (1. - abs(2. * c.z - 1.));
}

vec3 rgb2hsl(vec3 c) {
    float h = 0.;
    float s = 0.;
    float l = 0.;
    float r = c.r;
    float g = c.g;
    float b = c.b;
    float cMin = min(r, min(g, b));
    float cMax = max(r, max(g, b));

    l = (cMax + cMin) / 2.;
    if (cMax > cMin) {
    float cDelta = cMax - cMin;
    s = l < .0 ? cDelta / (cMax+cMin) : cDelta / (2. - (cMax + cMin));
    if(r == cMax) {
    h = (g - b) / cDelta;
    } else if(g == cMax) {
    h = 2. + (b - r) / cDelta;
    } else {
    h = 4. + (r - g) / cDelta;
    }

    if(h < 0.) {
    h += 6.;
    }
    h = h / 6.;
    }
    return vec3(h, s, l);
}

float hueDistance(vec3 h1, vec3 h2)
{
    vec3 diff = abs(h1 - h2);

    return dot(diff, diff);
}

vec3[2] closestColors(vec3 hue) {
    vec3 ret[2];
    vec3 closest = vec3(-2, -2, -2);
    vec3 secondClosest = vec3(-2, -2, -2);
    vec3 temp;
    for (int i = 0; i < paletteLength; ++i) {
        temp = palette[i];
        float tempDistance = hueDistance(temp, hue);
        if (tempDistance < hueDistance(closest, hue)) {
            secondClosest = closest;
            closest = temp;
        } else {
            if (tempDistance < hueDistance(secondClosest, hue)) {
                secondClosest = temp;
            }
        }
    }
    ret[0] = closest;
    ret[1] = secondClosest;
    return ret;
}

vec3 dither1(vec2 uv, vec3 color) {
    int x = int(uv.x) % 8;
    int y = int(uv.y) % 8;
    float threshold = bayerMatrix8x8[y * 8 + x];
    return color.rgb + threshold;// - 0.2;
}

void main()
{
/*
    // compute texel size
    vec2 texelSize = 1.0 / textureSize(inputTexture, 0).xy;

    // compute luminosity values
    vec3 lum = vec3(0.299, 0.587, 0.114);
    float lumTL = dot(lum, texture(inputTexture, vec2(-1.0, -1.0) * texelSize + texCoordsOut.xy).xyz);
    float lumTR = dot(lum, texture(inputTexture, vec2(+1.0, -1.0) * texelSize + texCoordsOut.xy).xyz);
    float lumBL = dot(lum, texture(inputTexture, vec2(-1.0, +1.0) * texelSize + texCoordsOut.xy).xyz);
    float lumBR = dot(lum, texture(inputTexture, vec2(+1.0, +1.0) * texelSize + texCoordsOut.xy).xyz);
    float lumCC = dot(lum, texture(inputTexture, texCoordsOut.xy).xyz);

    // compute blur direction
    vec2 dir;
    dir.x = -((lumTL + lumTR) - (lumBL + lumBR));
    dir.y = +((lumTL + lumBL) - (lumTR + lumBR));
    float dirReduce = max((lumTL + lumTR + lumBL + lumBR) * FXAA_REDUCE_MUL * 0.25, FXAA_REDUCE_MIN);
    float inverseDirAdjustment = 1.0/(min(abs(dir.x), abs(dir.y)) + dirReduce);
    dir = min(vec2(FXAA_SPAN_MAX, FXAA_SPAN_MAX), max(vec2(-FXAA_SPAN_MAX, -FXAA_SPAN_MAX), dir * inverseDirAdjustment)) * texelSize;

    // sample the texture in two locations along the computed direction to create an initial blurred color
    vec3 result1 = 0.5 * (
        texture(inputTexture, dir * vec2(1.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz +
        texture(inputTexture, dir * vec2(2.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz);

    // sample the texture at additional points and blend them with the initial blur to refine the result
    vec3 result2 = result1 * 0.5 + 0.25 * (
        texture(inputTexture, dir * vec2(0.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz +
        texture(inputTexture, dir * vec2(3.0 / 3.0 - 0.5) + texCoordsOut.xy).xyz);

    // compute the minimum and maximum luminosity of the surrounding texels to use for edge detection
    float lumMin = min(lumCC, min(min(lumTL, lumTR), min(lumBL, lumBR)));
    float lumMax = max(lumCC, max(max(lumTL, lumTR), max(lumBL, lumBR)));
    float lumResult2 = dot(lum, result2);*/


    float pixelSize = 2.0;

    vec2 textureSize = vec2(1920, 1056);

    vec2 texelSize = pixelSize / textureSize;

    vec2 coords = texelSize * floor(texCoordsOut.xy / texelSize);

    vec3 color = texture(inputTexture, coords).xyz;

    float brightness = dot(vec3(0.299, 0.587, 0.114), color);

    vec3 dither = dither1(coords * textureSize, color);

    vec3[2] closest = closestColors(dither);

    vec3 colorbanding = floor((4 - 1.0f) * dither + vec3(0.5)) / (4 - 1.0f);

    vec4 colorOut = vec4(closest[0], 1.0);

    // write
    frag = colorOut; // vec4(texture(inputTexture, texCoordsOut.xy).xyz, 1.0);
}