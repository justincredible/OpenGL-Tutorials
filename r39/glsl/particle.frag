#version 460 core

in vec2 tex;
in vec4 clr;

out vec4 color;

uniform sampler2D ture;

void main()
{
	vec4 texclr = texture(ture, tex);
	
	color = clr*texclr;
}
