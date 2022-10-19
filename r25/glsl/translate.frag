#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2D ture;
uniform float textrans;

void main()
{
	vec4 texclr = texture(ture, tex + vec2(textrans,0));
	
	color = texclr;
}
