#version 460 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texcoord;

out vec2 tex;
out vec4 reflectpos;
out vec4 refractpos;

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;
uniform mat4 reflection;

void main()
{
	vec4 worldpos = world*vec4(position, 1.0f);
	gl_Position = view*worldpos;
	gl_Position = projection*gl_Position;
	
	tex = texcoord;
	
	reflectpos = projection*reflection*worldpos;
	refractpos = gl_Position;
}
