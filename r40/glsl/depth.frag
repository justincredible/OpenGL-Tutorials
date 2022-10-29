#version 460 core

in vec4 depthpos;

out vec4 color;

void main()
{
	float depthval = depthpos.z/depthpos.w;
	
	color = vec4(depthval, depthval, depthval, 1);
}
