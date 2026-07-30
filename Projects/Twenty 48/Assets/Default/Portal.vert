#version 460 core

layout(set = 0, binding = 0) uniform PortalUniform
{
    mat4 viewProjection;
    mat4 model;
    vec4 viewport;
    vec4 tint;
} portal;

layout(location = 0) out vec2 localPosition;

const vec2 POSITIONS[6] = vec2[]
(
    vec2(-0.5, -0.5),
    vec2( 0.5, -0.5),
    vec2( 0.5,  0.5),
    vec2( 0.5,  0.5),
    vec2(-0.5,  0.5),
    vec2(-0.5, -0.5)
);

void main()
{
    vec2 position = POSITIONS[gl_VertexIndex];
    localPosition = position * 2.0;
    gl_Position = portal.viewProjection * portal.model * vec4(position, 0.0, 1.0);
}
