#version 460 core

in vec2 tex;
in vec3 normal;

layout(location=0)out vec4 color;
layout(location=1)out vec4 normpos;

uniform sampler2D ture;

void main()
{
	color = texture(ture, tex);
	
	normpos = vec4(normal,1);
}
