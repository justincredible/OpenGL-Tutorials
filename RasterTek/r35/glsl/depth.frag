#version 460 core

in vec4 depthpos;

out vec4 color;

void main()
{
	float depthval = depthpos.z/depthpos.w;
	
	if (depthval < 0.9) color = vec4(1,0,0,1);
	else if (depthval > 0.925) color = vec4(0,0,1,1);
	else color = vec4(0,1,0,1);
}
