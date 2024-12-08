#shader vertex
#version 410

const int TEX_COORDS_OFFSET_VERTS = 6;
const int TERRAIN_LAYERS_MAX = 6;

const vec2 TEX_COORDS_OFFSET_FILTERS[TEX_COORDS_OFFSET_VERTS] =
    vec2[TEX_COORDS_OFFSET_VERTS](
        vec2(1,1),
        vec2(0,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,0));

const vec2 TEX_COORDS_OFFSET_FILTERS_2[TEX_COORDS_OFFSET_VERTS] =
    vec2[TEX_COORDS_OFFSET_VERTS](
        vec2(0,0),
        vec2(1,0),
        vec2(1,1),
        vec2(0,0),
        vec2(1,1),
        vec2(0,1));

uniform mat4 view;
uniform mat4 projection;
uniform mat4 view_translate;
uniform mat4 view_rotate;

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texCoords;
layout(location = 2) in vec3 colors;
layout(location = 3) in vec3 tint;
layout(location = 4) in vec4 blends[2];
layout(location = 6) in mat4 model;
layout(location = 10) in vec4 texCoordsOffset;
layout(location = 11) in vec4 albedo;
layout(location = 12) in vec4 material;
layout(location = 13) in vec4 heightPlus;

out vec4 positionOut;
out vec2 texCoordsOut;
out vec3 normalOut;
out vec4 blendsOut[2];
out vec3 tintOut;
flat out vec4 albedoOut;
flat out vec4 materialOut;
flat out vec4 heightPlusOut;

flat out vec4 positionTest;
flat out mat4 viewTest;
flat out mat4 projectionTest;
flat out mat4 modelTest;
flat out mat4 view_translateTest;
flat out mat4 view_rotateTest;

void main()
{
    vec3 position5 = position;

    positionOut = model * ( vec4(position5, 1.0)) ;

    int texCoordsOffsetIndex = gl_VertexID % TEX_COORDS_OFFSET_VERTS;
    vec2 texCoordsOffsetFilter = TEX_COORDS_OFFSET_FILTERS[texCoordsOffsetIndex];
    vec2 texCoordsOffsetFilter2 = TEX_COORDS_OFFSET_FILTERS_2[texCoordsOffsetIndex];
    texCoordsOut = texCoords + texCoordsOffset.xy * texCoordsOffsetFilter + texCoordsOffset.zw * texCoordsOffsetFilter2;

    albedoOut = albedo;
    materialOut = material;
    normalOut = colors; // transpose(inverse(mat3(model))) *
    heightPlusOut = heightPlus;
    blendsOut[0] = blends[0];
    blendsOut[1] = blends[1];
    tintOut = tint;

    viewTest = view;
    projectionTest = projection;
    modelTest = model;
    view_translateTest = view_translate;
    view_rotateTest = view_rotate;

    float u_lod = 1;

    positionOut = view_translate * model * (vec4(position5, 1.0) + vec4(0.5, 0.5, 0.5, 0.0)*u_lod);
    gl_Position = (projection * view_rotate) * positionOut;
    positionTest = gl_Position;

    float ratio = 1920.0 / 1080.0;
    float reduce = max(
        abs( gl_Position.x*ratio/gl_Position.w  ),
        abs( gl_Position.y/gl_Position.w  )
    );
    // Following two values directly affect performance.
    reduce += 0.03; 		// Overdraw nearing edges

    float size = ( 1080 * 1.5 ) / gl_Position.z * max(reduce, 1.0);

    gl_PointSize = size * u_lod;

    float stochasticCoverage = gl_PointSize * gl_PointSize;

    if ((stochasticCoverage < 0.8) && ((gl_VertexID & 0xffff) > stochasticCoverage * (0xffff / 0.8))) {
        // "Cull" small voxels in a stable, stochastic way by moving past the z = 0 plane.
        // Assumes voxels are in randomized order.
        gl_Position = vec4(-1,-1,-1,-1);
        gl_PointSize = 1.0;
    }

}

#shader fragment
#version 410

const float GAMMA = 2.2;
const int TERRAIN_LAYERS_MAX = 6;

uniform mat4 inv_view;
uniform mat4 inv_projection;
uniform vec2 viewPort;
uniform vec4 viewPortBounds;
uniform vec3 eyeCenter;
uniform int layersCount;
uniform sampler2D albedoTextures[TERRAIN_LAYERS_MAX];
uniform sampler2D roughnessTextures[TERRAIN_LAYERS_MAX];
uniform sampler2D ambientOcclusionTextures[TERRAIN_LAYERS_MAX];
uniform sampler2D normalTextures[TERRAIN_LAYERS_MAX];
uniform sampler2D heightTextures[TERRAIN_LAYERS_MAX];

in vec4 positionOut;
in vec2 texCoordsOut;
in vec3 normalOut;
in vec4 blendsOut[2];
in vec3 tintOut;
flat in vec4 albedoOut;
flat in vec4 materialOut;
flat in vec4 heightPlusOut;

flat in vec4 positionTest;
flat in mat4 viewTest;
flat in mat4 projectionTest;
flat in mat4 modelTest;

flat in mat4 view_translateTest;
flat in mat4 view_rotateTest;

layout(location = 0) out vec4 position;
layout(location = 1) out vec3 albedo;
layout(location = 2) out vec4 material;
layout(location = 3) out vec4 normalPlus;

vec2 AABBIntersect(vec3 ro, vec3 rd, vec3 minV, vec3 maxV)
{
    vec3 invR = 1.0 / rd;

    float t0, t1;

    vec3 tbot = (minV - ro) * invR;
    vec3 ttop = (maxV - ro) * invR;

    vec3 tmin = min(ttop, tbot);
    vec3 tmax = max(ttop, tbot);


    vec2 t = max(tmin.xx, tmin.yz);

    t0 = max(t.x, t.y);
    t = min(tmax.xx, tmax.yz);
    t1 = min(t.x, t.y);

    return vec2(t0, t1);
    // if (t0 <= t1) { did hit } else { did not hit }
}

vec4 ss2wsVec() {
    vec4 ndcPos;
    ndcPos.xy = ((2.0 * gl_FragCoord.xy) - (2.0 * viewPortBounds.xy)) / (viewPortBounds.zw) - 1;
    ndcPos.z = (2.0 * gl_FragCoord.z - gl_DepthRange.near - gl_DepthRange.far) / (gl_DepthRange.far - gl_DepthRange.near);
    ndcPos.w = 1.0;

    vec4 clipPos = ndcPos / gl_FragCoord.w;
    vec4 eyePos = inv_projection * clipPos;
    vec4 worldPos = inv_view * eyePos;

    return worldPos;
}

void main()
{

    // ensure layers count is in range
    float layersCountCeil = max(min(layersCount, TERRAIN_LAYERS_MAX), 0);

    // forward position, marking w for written
    position.xyz = positionOut.xyz;
    position.w = 1.0;


    // compute spatial converters
    vec3 q1 = dFdx(positionOut.xyz);
    vec3 q2 = dFdy(positionOut.xyz);
    vec2 st1 = dFdx(texCoordsOut);
    vec2 st2 = dFdy(texCoordsOut);
    vec3 normal = normalize(normalOut);
    vec3 tangent = normalize(q1 * st2.t - q2 * st1.t);
    vec3 binormal = -normalize(cross(normal, tangent));
    mat3 toWorld = mat3(tangent, binormal, normal);
    mat3 toTangent = transpose(toWorld);

    // compute height blend, height, and ignore local light maps
    float heightBlend = 0.0;
    for (int i = 0; i < layersCountCeil; ++i) heightBlend += texture(heightTextures[i], texCoordsOut).r * blendsOut[i/4][i%4];
    float height = heightBlend * heightPlusOut.x;

    // compute tex coords in parallax space
    vec3 eyeCenterTangent = toTangent * eyeCenter;
    vec3 positionTangent = toTangent * positionOut.xyz;
    vec3 toEyeTangent = normalize(eyeCenterTangent - positionTangent);
    vec2 parallax = toEyeTangent.xy * height;
    vec2 texCoords = texCoordsOut - parallax;

    // compute albedo and material blends
    vec4 albedoBlend = vec4(0.0);
    float roughnessBlend = 0.0;
    float ambientOcclusionBlend = 0.0;
    vec3 normalBlend = vec3(0.0);
    for (int i = 0; i < layersCountCeil; ++i)
    {
        float blend = blendsOut[i/4][i%4];
        albedoBlend += texture(albedoTextures[i], texCoords) * blend;
        vec4 roughness = texture(roughnessTextures[i], texCoords);
        roughnessBlend += (roughness.a == 1.0f ? roughness.r : roughness.a) * blend;
        ambientOcclusionBlend += texture(ambientOcclusionTextures[i], texCoords).b * blend;
        normalBlend += (texture(normalTextures[i], texCoords).xyz * 2.0 - 1.0) * blend;
    }

    // discard fragment if even partly transparent
    if (albedoBlend.w < 0.5) discard;

    // populate albedo, material, and normalPlus
    // albedo = pow(albedoBlend.rgb, vec3(GAMMA)) * tintOut * albedoOut.rgb;
    material = vec4(roughnessBlend * materialOut.g, 0.0, ambientOcclusionBlend * materialOut.b, 0.0);
    normalPlus.xyz = normalize(toWorld * normalize(normalBlend));
    normalPlus.w = heightPlusOut.y;

// gl_FragCoord, gl_PointCoord and coordinates of the point

    // trying to make a ray from eye to fragment's 3d position, like screen2world

    float u_lod = 1;

    vec3 vxl = positionOut.xyz;

    vec3 ray = ss2wsVec().xyz;

    vec2 result = AABBIntersect(
        vec3(0.0), ray,
        vec3( vxl-0.5*u_lod),
        vec3( vxl+0.5*u_lod)
    );

    if( !(result.x<=result.y) ) {

        //gl_FragDepth = 0.99;
        //out_Color = vec4(1);

        discard;
        return;

    }

    vec3 hit = vxl - result.x * ray;

    vec3  hit_abs = abs(hit);
    float max_dim = max( max( hit_abs.x, hit_abs.y), hit_abs.z  );

    albedo =
        normalOut +

        vec3(
            float(hit_abs.x == max_dim),
            float(hit_abs.y == max_dim),
            float(hit_abs.z == max_dim)
        ) * 0.05;

    // the stupidest solution, should optimize by checking the math
    vec4 hitPos = vec4(result.x * ray + eyeCenter, 1.0);
    vec4 hitPos2 = projectionTest * viewTest * modelTest * hitPos;

    float test = hitPos2.z / hitPos2.w;
    gl_FragDepth = 0.5 * (gl_DepthRange.far - gl_DepthRange.near) * test + 0.5 * (gl_DepthRange.far + gl_DepthRange.near);

}