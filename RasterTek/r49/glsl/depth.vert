#version 460 core

layout(location=0)in vec3 position;

out vec4 depthpos;

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;

void main()
{
	gl_Position = projection*view*world*vec4(position,1);
	
	depthpos = gl_Position;
}
