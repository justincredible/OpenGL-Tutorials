#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2D tex0;

void main()
{
	vec4 texclr;
	
	texclr = texture(tex0, tex);
	
	color = texclr;
}
