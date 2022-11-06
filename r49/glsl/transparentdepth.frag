#version 460 core

in vec2 tex;
in vec4 depthpos;

out vec4 color;

uniform sampler2D ture;

void main()
{
	if (texture(ture, tex).a <= 0.8) discard;
	
	float depthval = depthpos.z/depthpos.w;
	
	color = vec4(depthval, depthval, depthval, 1);
}
