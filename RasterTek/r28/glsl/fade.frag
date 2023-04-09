#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2D ture;
uniform float fadeamt;

void main()
{
	color = fadeamt*texture(ture, tex);
	color.a = 1;
}
