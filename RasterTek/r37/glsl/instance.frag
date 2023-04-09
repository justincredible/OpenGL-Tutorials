#version 460 core

in vec2 tex;
in vec4 clr;

out vec4 color;

uniform sampler2D ture;

void main()
{
	color = texture(ture, tex);
}
