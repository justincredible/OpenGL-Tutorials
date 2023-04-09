#version 460 core

in vec2 tex;
in vec4 refractpos;

out vec4 color;

uniform sampler2D colortex;
uniform sampler2D normaltex;
uniform sampler2D refractex;
uniform float refrascale;

void main()
{
	vec3 normal = 2*texture(normaltex, tex).xyz - 1;
	
	vec2 reftex = vec2(refractpos.x/refractpos.w/2+0.5, refractpos.y/refractpos.w/2+0.5) + normal.xy*refrascale;
	
	vec4 refraclr = texture(refractex, reftex);
	
	vec4 texclr = texture(colortex, tex);
	
	color = mix(refraclr, texclr, 0.5);
}
