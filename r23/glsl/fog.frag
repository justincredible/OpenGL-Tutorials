#version 460 core

in vec2 tex;
in float factor;

out vec4 color;

uniform sampler2D image;

void main()
{
	vec4 texclr = texture(image, tex);
	
	vec4 fogclr = vec4(0.5,0.5,0.5,1);
	
	color = factor*texclr + (1-factor)*fogclr;
}
