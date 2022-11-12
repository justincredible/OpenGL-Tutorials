#version 460 core

layout(location=0)in vec3 position;
layout(location=1)in vec2 texcoord;
layout(location=2)in vec3 normal;
layout(location=3)in vec3 tangent;
layout(location=4)in vec3 bitangent;

out vec2 tex;
out vec3 nrm;
out vec3 tng;
out vec3 btn;

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;

void main()
{
	gl_Position = projection*view*world*vec4(position, 1.0f);
	
	tex = texcoord;
	
	nrm = normalize(mat3(world)*normal);
	tng = normalize(mat3(world)*tangent);
	btn = normalize(mat3(world)*bitangent);
}
