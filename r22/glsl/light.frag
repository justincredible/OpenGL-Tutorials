#version 460 core

in vec2 coord;
in vec3 normal;

out vec4 color;

uniform sampler2D tex0;
uniform vec3 direction;
uniform vec4 diffuse;

void main()
{
	vec4 texclr;
	float intensity;
	
	texclr = texture(tex0, coord);
	
	intensity = clamp(dot(normal, -direction), 0, 1);
	
	color += intensity*diffuse;
	color = clamp(color, 0, 1);
	
	color *= texclr;
}
