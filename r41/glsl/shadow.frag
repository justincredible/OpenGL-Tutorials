#version 460 core

#define NUM_LIGHTS 2

in vec2 tex;
in vec3 normal;
in vec4 lightviewpos[NUM_LIGHTS];
in vec3 lightpos[NUM_LIGHTS];

out vec4 color;

uniform sampler2D ture;
uniform sampler2D depths[NUM_LIGHTS];
uniform vec4 ambient;
uniform vec4 diffuses[NUM_LIGHTS];

void main()
{	
	float bias = 0.0025;
	
	color = ambient;
	
	for (int i = 0; i < NUM_LIGHTS; i++)
	{
		vec2 projectex = vec2(lightviewpos[i].x/lightviewpos[i].w/2 + 0.5,lightviewpos[i].y/lightviewpos[i].w/2 + 0.5);
		
		if (clamp(projectex,0,1) == projectex)
		{
			float depthval = texture(depths[i], projectex).r;
			
			float lightdepth = lightviewpos[i].z/lightviewpos[i].w;
			
			if (lightdepth - bias < depthval)
			{
				float intensity = clamp(dot(normal, lightpos[i]),0,1);
				
				color += diffuses[i]*intensity;
				
				if (intensity > 0) color = clamp(color,0,1);
			}
		}
	}
	
	vec4 texclr = texture(ture, tex);
	
	color *= texclr;
}
