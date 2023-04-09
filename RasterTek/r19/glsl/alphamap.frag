#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2DArray texas;

void main()
{
	vec4 texclr1 = texture(texas, vec3(tex,0));
	vec4 texclr2 = texture(texas, vec3(tex,1));
	vec4 alphaval = texture(texas, vec3(tex,2));
	
	color = clamp(alphaval*texclr1 + (1-alphaval)*texclr2,0,1);
}
