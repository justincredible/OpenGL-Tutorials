#version 460 core

#define NUM_LIGHTS 2

layout(location=0)in vec3 position;
layout(location=1)in vec2 texcoord;
layout(location=2)in vec3 facenrml;

out vec2 tex;
out vec3 normal;
out vec4 lightviewpos[NUM_LIGHTS];
out vec3 lightpos[NUM_LIGHTS];

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;
uniform mat4 lightviews[NUM_LIGHTS];
uniform mat4 lightprojects[NUM_LIGHTS];
uniform vec3 lightpositions[NUM_LIGHTS];

void main()
{
	vec4 worldpos = world*vec4(position,1);
	gl_Position = projection*view*worldpos;
	
	tex = texcoord;
	
	normal = normalize(mat3(world)*facenrml);
	
	for (int i = 0; i < NUM_LIGHTS; i++)
	{
		lightviewpos[i] = lightprojects[i]*lightviews[i]*worldpos;
		
		lightpos[i] = normalize(lightpositions[i] - worldpos.xyz);
	}
}
