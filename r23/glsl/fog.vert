#version 460 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texcoord;

out vec2 tex;
out float factor;

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;
uniform vec2 range;

void main()
{
	vec4 camerapos;
	
	gl_Position = world*vec4(position, 1.0f);
	gl_Position = view*gl_Position;
	camerapos = gl_Position;
	gl_Position = projection*camerapos;
	
	tex = texcoord;
	
	factor = clamp((range.y - camerapos.z)/(range.y - range.x),0,1);
}
