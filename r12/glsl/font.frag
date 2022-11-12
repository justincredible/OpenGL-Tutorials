#version 460 core

in vec2 tex;

out vec4 color;

uniform vec4 pxlclr;
uniform sampler2D ture;

void main()
{
	
	if (texture(ture, tex).r == 0) discard; // or .g or .b or and all three
	
	color = pxlclr;
}
