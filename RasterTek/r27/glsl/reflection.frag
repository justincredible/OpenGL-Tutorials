#version 460 core

in vec2 tex;
in vec4 reflectpos;

out vec4 color;

uniform sampler2D ture;
uniform sampler2D reflect;

void main()
{
	vec4 texclr = texture(ture, tex);
	
	vec2 reflectex = vec2(reflectpos.x/reflectpos.w/2+0.5, reflectpos.y/reflectpos.w/2+0.5);
	
	vec4 refleclr = texture(reflect, reflectex);
	
	color = mix(texclr, refleclr, 0.15);
}
