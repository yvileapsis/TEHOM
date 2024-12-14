#shader vertex
#version 410

uniform mat4 view;
uniform mat4 projection;
uniform mat4 view_translate;
uniform mat4 view_rotate;

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 colors;

layout(location = 6) in mat4 model;
layout(location = 10) in vec4 size;
layout(location = 11) in vec4 offset;

out vec4 positionOut;
out vec3 colorsOut;
out vec3 sizesOut;

flat out mat4 modelViewProjectionOut;

void main()
{
    vec3 sizes = size.xyz;

    vec3 position1 = vec3(position) * sizes + offset.xyz;

    float u_lod = max(max(sizes.x, sizes.y), sizes.z);

    positionOut = view_translate * model * (vec4(position1, 1.0) );

    colorsOut = vec3(colors);
    sizesOut = sizes;

    modelViewProjectionOut = projection * view * model;

    vec4 positionNew = (projection * view_rotate) * positionOut;

    float ratio = 1920.0 / 1080.0;
    float reduce = max(
        abs( positionNew.x*ratio/positionNew.w  ),
        abs( positionNew.y/positionNew.w  )
    );

    // Following two values directly affect performance.
    reduce += 0.03; 		// Overdraw nearing edges

    float size = u_lod * ( 1080 * 1.5 ) / positionNew.z * max(reduce, 1.0);

    float stochasticCoverage = size * size;

    if ((stochasticCoverage < 0.8) && ((gl_VertexID & 0xffff) > stochasticCoverage * (0xffff / 0.8))) {
        // "Cull" small voxels in a stable, stochastic way by moving past the z = 0 plane.
        // Assumes voxels are in randomized order.
        gl_Position = vec4(-1,-1,-1,-1);
        gl_PointSize = 1.0;
    }
    else {
        gl_Position = positionNew;
        gl_PointSize = size;
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

in vec4 positionOut;
in vec3 colorsOut;
in vec3 sizesOut;

flat in mat4 modelViewProjectionOut;

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
//    float layersCountCeil = max(min(layersCount, TERRAIN_LAYERS_MAX), 0);

    // forward position, marking w for written
    position.xyz = positionOut.xyz;
    position.w = 1.0;

    /*
    vec2 texCoordsOut = vec2(1.0);
    vec4 blendsOut = vec4(1.0);
    vec3 tintOut = vec3(1.0);
    vec4 albedoOut = vec4(1.0);
    vec4 materialOut = vec4(1.0);
    vec4 heightPlusOut = vec4(1.0);
    vec3 normalOut = vec3(1.0);

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
//    for (int i = 0; i < layersCountCeil; ++i) heightBlend += texture(heightTextures[i], texCoordsOut).r * blendsOut[i%4];
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
        float blend = blendsOut[i%4];
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
*/

    normalPlus = vec4(1.0);
    material = vec4(1.0);
// gl_FragCoord, gl_PointCoord and coordinates of the point

    // trying to make a ray from eye to fragment's 3d position, like screen2world

    float u_lod = 1;

    vec3 vxl = positionOut.xyz;

    vec3 ray = ss2wsVec().xyz;

    vec2 result = AABBIntersect(
        vec3(0.0), ray,
        vxl - 0.5 * sizesOut,
        vxl + 0.5 * sizesOut
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

    vec3 light = normalize( vec3(-1.0, 1.25, 2)  );

    vec3 normal = vec3(
        float(hit_abs.x == max_dim),
        float(hit_abs.y == max_dim),
        float(hit_abs.z == max_dim)
    ) * sign(hit);

    vec3 r = -normal;

    float ndotl = 0.3;

    albedo = colorsOut * max( max(0.7, ndotl), sign( dot(r, light) ) );

    // the stupidest solution, should optimize by checking the math
    vec4 hitPos = vec4(result.x * ray + eyeCenter, 1.0);
    vec4 hitPos2 = modelViewProjectionOut * hitPos;

    float test = hitPos2.z / hitPos2.w;
    gl_FragDepth = 0.5 * (gl_DepthRange.far - gl_DepthRange.near) * test + 0.5 * (gl_DepthRange.far + gl_DepthRange.near);

}