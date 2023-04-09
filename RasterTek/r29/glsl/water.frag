#version 460 core

in vec2 tex;
in vec4 reflectpos;
in vec4 refractpos;

out vec4 color;

uniform sampler2D reflectex;
uniform sampler2D refractex;
uniform sampler2D normaltex;
uniform float watertrans;
uniform float refscale;

void main()
{	
	vec2 reflect = vec2(reflectpos.x/reflectpos.w/2+0.5, reflectpos.y/reflectpos.w/2+0.5);
	vec2 refract = vec2(refractpos.x/refractpos.w/2+0.5, refractpos.y/refractpos.w/2+0.5);
	
	vec3 normal = 2*texture(normaltex, tex + vec2(0, watertrans)).xyz - 1;
	
	vec4 refleclr = texture(reflectex, reflect + refscale*normal.xy);
	vec4 refraclr = texture(refractex, refract + refscale*normal.xy);
	
	color = mix(refleclr, refraclr, 0.6);
}
