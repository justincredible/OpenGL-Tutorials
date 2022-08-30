#version 460 core

in vec2 txcrd;
in vec3 nrml;

out vec4 color;

uniform sampler2D tex0;
uniform vec3 direction;
uniform vec4 diffuse;
uniform vec4 ambient;

void main()
{
	vec4 texclr;
	vec3 lightdir;
	float intensity;
	
	texclr = texture(tex0, txcrd);
	
	color = ambient;
	
	lightdir = -direction;
	
	intensity = clamp(dot(nrml, lightdir), 0.0f, 1.0f);
	
	if (intensity > 0.0f) color += clamp(diffuse*intensity, 0.0f, 1.0f);
	
	color *= texclr;
}
