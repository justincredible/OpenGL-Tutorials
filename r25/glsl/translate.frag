#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2D ture;
uniform float textrans;

void main()
{
	color = texture(ture, tex + vec2(textrans,0));
}
