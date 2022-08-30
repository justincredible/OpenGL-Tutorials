#version 460 core

in vec2 coord;
in vec3 normal;
in vec3 viewdir;

out vec4 color;

uniform sampler2D tex0;
uniform vec3 direction;
uniform vec4 ambient;
uniform vec4 diffuse;
uniform vec4 specular;
uniform float power;

void main()
{
	vec4 texclr, speclr;
	vec3 lightdir, reflection;
	float intensity;
	
	texclr = texture(tex0, coord);
	
	color = ambient;
	
	speclr = vec4(0, 0, 0, 0);
	
	lightdir = -direction;
	
	intensity = clamp(dot(normal, lightdir), 0, 1);
	
	color += intensity*diffuse;
	color = clamp(color, 0, 1);
	
	reflection = normalize(2*intensity*normal - lightdir);
	
	speclr = intensity*specular*pow(clamp(dot(reflection, viewdir), 0, 1), power);
	
	color *= texclr;
	
	color = clamp(color + speclr, 0, 1);
}
