#version 460 core

layout(location = 0)in vec3 pos;
layout(location = 1)in vec2 tex;
layout(location = 2)in vec3 nml;

out vec2 coord;
out vec3 normal;
out vec3 viewdir;

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;

uniform vec3 camera;

void main()
{
	vec4 worldpos;
	
	worldpos = world*vec4(pos, 1.0);
	gl_Position = view*worldpos;
	gl_Position = projection*gl_Position;
	
	coord = tex;
	
	normal = normalize(mat3(world)*nml);
	
	viewdir = normalize(camera - worldpos.xyz);
}
