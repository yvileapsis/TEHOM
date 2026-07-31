#version 460 core

layout(set = 0, binding = 0) uniform PortalUniform
{
    mat4 viewProjection;
    mat4 model;
    vec4 viewport;
    vec4 tint;
} portal;

layout(set = 0, binding = 1) uniform sampler2D portalTexture;

layout(location = 0) in vec2 localPosition;
layout(location = 0) out vec4 colorOut;

void main()
{
    vec2 texCoords = gl_FragCoord.xy / portal.viewport.xy;
    if (portal.viewport.z > 0.5)
    {
        colorOut = vec4(portal.tint.rgb, 1.0);
        return;
    }

    vec3 portalColor = texture(portalTexture, texCoords).rgb;
    if (any(isnan(portalColor)) || dot(portalColor, portalColor) < 0.00001)
    {
        portalColor = vec3(0.0);
    }

    float edgeDistance = 1.0 - max(abs(localPosition.x), abs(localPosition.y));
    float edgeTint = 1.0 - smoothstep(0.0, 0.18, edgeDistance);
    portalColor = mix(portalColor, portal.tint.rgb, edgeTint * 0.65 * portal.tint.a);
    portalColor += portal.tint.rgb * edgeTint * 0.08 * portal.tint.a;
    colorOut = vec4(portalColor, 1.0);
}
