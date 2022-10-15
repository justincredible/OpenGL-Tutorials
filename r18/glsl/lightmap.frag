#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2DArray texas;

void main()
{
	vec4 texclr, lightclr, finalclr;
	
	texclr = texture(texas, vec3(tex,0));
	lightclr = texture(texas, vec3(tex,1));
	
	finalclr = texclr*lightclr;
	
	color = finalclr;
}
