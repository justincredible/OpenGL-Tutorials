#version 460 core

in vec3 colorp;

out vec4 color;

void main()
{
	color = vec4(colorp, 1.0f);
}
