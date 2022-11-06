#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2D ture;
uniform sampler2D glow;
uniform float strength;

void main()
{	
	vec4 texclr = texture(ture, tex);
	
	vec4 glowclr = texture(glow, tex);
	
	color = clamp(texclr + glowclr*strength,0,1);
}
