#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2D ture;
uniform float blendamt;

void main()
{
	color = texture(ture, tex);
	
	color.a = blendamt;
}
