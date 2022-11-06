#version 460 core

layout(location = 0)in vec3 position;
layout(location = 1)in vec2 texcoord;
layout(location = 2)in vec3 facenrml;

out vec2 tex;
out vec3 normal;
out vec4 lightviewpos;

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;
uniform mat4 lightview;
uniform mat4 lightproject;

void main()
{
	vec4 worldpos = world*vec4(position, 1);
	gl_Position = projection*view*worldpos;
	
	lightviewpos = lightproject*lightview*worldpos;
	
	tex = texcoord;
	
	normal = normalize(mat3(world)*facenrml);
}
