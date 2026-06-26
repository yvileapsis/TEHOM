#shader vertex
#version 460 core

uniform mat4 model;
uniform mat4 viewProjection;

layout(location = 0) in vec3 position;

out vec2 localPosition;

void main()
{
    localPosition = position.xy;
    gl_Position = viewProjection * model * vec4(position, 1.0);
}

#shader fragment
#version 460 core

uniform sampler2D portalTexture;
uniform vec2 viewPort;
uniform bool fillOnly;
uniform vec4 tint;

in vec2 localPosition;

layout(location = 0) out vec4 frag;

void main()
{
    if (fillOnly)
    {
        frag = vec4(tint.rgb, 1.0);
        return;
    }

    vec2 texCoords = gl_FragCoord.xy / viewPort;
    vec3 portalColor = texture(portalTexture, texCoords).rgb;
    if (any(isnan(portalColor)) || dot(portalColor, portalColor) < 0.00001)
    {
        portalColor = vec3(0.0);
    }

    float edgeDistance = 1.0 - max(abs(localPosition.x), abs(localPosition.y));
    float edgeTint = 1.0 - smoothstep(0.0, 0.18, edgeDistance);
    portalColor = mix(portalColor, tint.rgb, edgeTint * 0.65 * tint.a);
    portalColor += tint.rgb * edgeTint * 0.08 * tint.a;
    frag = vec4(portalColor, 1.0);
}
