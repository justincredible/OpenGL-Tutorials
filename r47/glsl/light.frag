#version 460 core

in vec2 tex;
in vec3 normal;
in vec3 viewdir;

out vec4 color;

uniform sampler2D ture;
uniform vec3 direction;
uniform vec4 ambient;
uniform vec4 diffuse;
uniform vec4 specular;
uniform float power;

void main()
{
	vec4 texclr = texture(ture, tex);
	
	color = ambient;
	
	float intensity = clamp(dot(normal, -direction),0,1);
	
	color += intensity*diffuse;
	color = clamp(color,0,1);
	
	vec3 reflection = normalize(2*intensity*normal + direction);
	
	vec4 speclr = intensity*specular*pow(clamp(dot(reflection, viewdir),0,1), power);
	
	color *= texclr;
	
	color = clamp(color + speclr,0,1);
}
