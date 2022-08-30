#version 460 core

in vec2 txcrd;
in vec3 nrml;

out vec4 color;

uniform sampler2D tex0;
uniform vec3 direction;
uniform vec4 diffuse;

void main()
{
	vec4 texclr;
	vec3 lightdir;
	float intensity;
	
	texclr = texture(tex0, txcrd);
	
	lightdir = -direction;
	
	intensity = clamp(dot(nrml, lightdir), 0.0f, 1.0f);
	
	color = clamp(diffuse*intensity, 0.0f, 1.0f);
	
	color *= texclr;
}
