#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2DArray texas;

void main()
{
	vec4 texclr1, texclr2, blendclr;
	
	texclr1 = texture(texas, vec3(tex,0));
	texclr2 = texture(texas, vec3(tex,1));
	
	blendclr = clamp(texclr1*texclr2*2, 0, 1);
	
	color = blendclr;
}
