#shader vertex
#version 410

const int VERTS = 4;
const vec4 FILTERS[VERTS] =
    vec4[VERTS](
        vec4(1.0, 1.0, 0.0, 1.0),
        vec4(1.0, 1.0, 0.0, 0.0),
        vec4(1.0, 1.0, 1.0, 1.0),
        vec4(1.0, 1.0, 1.0, 0.0));

const vec2 VERTEX_CONST[VERTS] =
    vec2[VERTS](
        vec2(0.0, 1.0),
        vec2(0.0, 0.0),
        vec2(1.0, 1.0),
        vec2(1.0, 0.0));


uniform mat4 modelViewProjection;
uniform vec4 texCoords4;

layout (location = 0) in vec2 index;

out vec2 texCoords;
out float test;

void main()
{
    int vertexId = gl_VertexID % VERTS;
    vec2 position = VERTEX_CONST[vertexId];
    vec4 filt = FILTERS[vertexId];
    test = index.x;
    gl_Position = modelViewProjection * vec4(position.x + index.y / 64.0, position.y, 0, 1);
    texCoords = vec2(texCoords4.x * filt.x + texCoords4.z * filt.z, texCoords4.y * filt.y + texCoords4.w * filt.w);
}

#shader fragment
#version 410

uniform sampler2DArray tex;
uniform vec4 color;

in vec2 texCoords;
in float test;

layout(location = 0) out vec4 frag;

void main()
{
    frag = color * texture(tex, vec3(texCoords, test));
}