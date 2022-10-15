#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2DArray texas;

void main()
{
	vec4 texclr1, texclr2, alphaval, blendclr;
	
	texclr1 = texture(texas, vec3(tex,0));
	texclr2 = texture(texas, vec3(tex,1));
	alphaval = texture(texas, vec3(tex,2));
	
	blendclr = clamp(alphaval*texclr1 + (1-alphaval)*texclr2, 0, 1);
	
	color = blendclr;
}
