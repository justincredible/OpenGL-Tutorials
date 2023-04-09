#version 460 core

in vec2 tex;
in vec3 nrm;
in vec3 tng;
in vec3 btn;

out vec4 color;

uniform sampler2D ture;
uniform sampler2D nrmtex;
uniform vec3 direction;
uniform vec4 diffuse;

void main()
{	
	vec4 bumpmap = 2*texture(nrmtex, tex) - 1;
	
	vec3 bumpnrm = normalize(bumpmap.x*tng + bumpmap.y*btn + bumpmap.z*nrm);
	
	float intensity = clamp(dot(bumpnrm, -direction),0,1);
	
	color = clamp(diffuse*intensity,0,1);
	
	color *= texture(ture, tex);
}
