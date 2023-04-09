#version 460 core

in vec2 tex;
in vec3 normal;
in vec4 viewpos;
in vec3 lightpos;

out vec4 color;

uniform sampler2D ture;
uniform sampler2D project;
uniform vec4 ambient;
uniform vec4 diffuse;

void main()
{
	float brightness = 1.5;
	
	color = vec4(1);
	
	float intensity = clamp(dot(normal, lightpos),0,1);
	
	if (intensity > 0) color = diffuse*intensity*brightness;
	
	vec4 texclr = texture(ture, tex);
	
	vec2 projectex = vec2(viewpos.x/viewpos.w/2 + 0.5, viewpos.y/viewpos.w/2 + 0.5);
	
	if (clamp(projectex,0,1) != projectex) color = ambient*texclr;
	else color = clamp(color*texture(project, projectex)*texclr + ambient*texclr,0,1);
}
