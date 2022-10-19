#version 460 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texcoord;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec3 tangent;
layout(location = 4) in vec3 bitangent;

out vec2 tex;
out vec3 nrm;
out vec3 tng;
out vec3 btn;
out vec3 viewdir;

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;
uniform vec3 camerapos;

void main()
{
	vec4 worldpos;
	
	worldpos = world*vec4(position, 1.0f);
	gl_Position = view*worldpos;
	gl_Position = projection*gl_Position;
	
	tex = texcoord;
	
	nrm = normalize(mat3(world)*normal);
	tng = normalize(mat3(world)*tangent);
	btn = normalize(mat3(world)*bitangent);
	
	viewdir = normalize(camerapos - worldpos.xyz);
}
