#version 460 core

in vec2 tex;
in vec3 nrm;
in vec3 tng;
in vec3 btn;
in vec3 viewdir;

out vec4 color;

uniform sampler2DArray texas;
uniform vec3 direction;
uniform vec4 diffuse;
uniform vec4 specular;
uniform float power;

void main()
{
	vec4 bumpmap = texture(texas, vec3(tex,1))*2 - 1;
	
	vec3 bumpnrm = normalize(bumpmap.x*tng + bumpmap.y*btn + bumpmap.z*nrm);
	
	float intensity = clamp(dot(bumpnrm, -direction),0,1);
	
	color = clamp(diffuse*intensity,0,1);
	
	color *= texture(texas, vec3(tex,0));
	
	if (intensity > 0)
	{
		vec4 speclr = texture(texas, vec3(tex,2));
		
		vec3 reflection = normalize(2*intensity*bumpnrm + direction);
		
		vec4 specintensity = speclr*pow(clamp(dot(reflection, viewdir),0,1), power);
		
		color = clamp(color + specintensity*specular,0,1);
	}
}
