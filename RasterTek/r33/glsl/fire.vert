#version 460 core

layout(location=0)in vec3 position;
layout(location=1)in vec2 texcoord;

out vec2 tex;
out vec2 texcrd1;
out vec2 texcrd2;
out vec2 texcrd3;

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;
uniform float frametime;
uniform vec3 scrolls;
uniform vec3 scales;

void main()
{
	gl_Position = projection*view*world*vec4(position,1);
	
	tex = texcoord;
	
	texcrd1 = texcoord*scales.x;
	texcrd1.y = texcrd1.y - frametime*scrolls.x;
	
	texcrd2 = texcoord*scales.y;
	texcrd2.y = texcrd2.y - frametime*scrolls.y;
	
	texcrd3 = texcoord*scales.z;
	texcrd3.y = texcrd3.y - frametime*scrolls.z;
}
