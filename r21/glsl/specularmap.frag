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
	vec4 texclr, bumpmap, speclr, specintensity;
	vec3 bumpnrm, lightdir, reflection;
	float intensity;
	
	texclr = texture(texas, vec3(tex,0));
	
	bumpmap = texture(texas, vec3(tex,1));
	bumpmap = bumpmap*2-1;
	
	bumpnrm = normalize(bumpmap.x*tng + bumpmap.y*btn + bumpmap.z*nrm);
	
	lightdir = -direction;
	
	intensity = clamp(dot(bumpnrm,lightdir),0,1);
	
	color = clamp(diffuse*intensity,0,1);
	
	color *= texclr;
	
	if (intensity > 0)
	{
		speclr = texture(texas, vec3(tex,2));
		
		reflection = normalize(2*intensity*bumpnrm - lightdir);
		
		specintensity = speclr*pow(clamp(dot(reflection,viewdir),0,1), power);
		
		color = clamp(color + specintensity*specular,0,1);
	}
}
