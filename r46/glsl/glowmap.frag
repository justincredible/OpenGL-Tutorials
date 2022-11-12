#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2D ture;
uniform sampler2D glow;

void main()
{
	vec4 glowclr = texture(glow, tex);
	
	if (glowclr.r == 0 && glowclr.g == 0 && glowclr.b == 0) discard;
	
	color = texture(ture, tex);
}
