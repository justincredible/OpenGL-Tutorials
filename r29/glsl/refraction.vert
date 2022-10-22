#version 460 core

layout(location = 0)in vec3 position;
layout(location = 1)in vec2 texcoord;
layout(location = 2)in vec3 normal;

out vec2 tex;
out vec3 nrml;
out float clip;

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;
uniform vec4 clipplane;

void main()
{
	vec4 worldpos = world*vec4(position, 1.0);
	gl_Position = view*worldpos;
	gl_Position = projection*gl_Position;
	
	tex = texcoord;
	
	nrml = normalize(mat3(world)*normal);
	
	clip = dot(worldpos, clipplane);
}
