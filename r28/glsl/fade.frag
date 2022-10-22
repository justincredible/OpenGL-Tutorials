#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2D ture;
uniform float fadeamt;

void main()
{
	vec4 texclr = texture(ture, tex);
	
	color = fadeamt*texclr;
	
	color.a = 1;
}
