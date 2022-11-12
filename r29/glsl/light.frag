#version 460 core

in vec2 tex;
in vec3 normal;

out vec4 color;

uniform sampler2D ture;
uniform vec3 direction;
uniform vec4 ambient;
uniform vec4 diffuse;

void main()
{	
	color = ambient;
	
	float intensity = clamp(dot(normal, -direction), 0, 1);
	
	color = clamp(color + intensity*diffuse,0,1);
	
	color *= texture(ture, tex);
}
