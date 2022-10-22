#version 460 core

in vec2 tex;
in vec3 nrml;
in float clip;

out vec4 color;

uniform sampler2D ture;
uniform vec3 direction;
uniform vec4 ambient;
uniform vec4 diffuse;

void main()
{	
	if (clip < 0) discard;
	
	vec4 texclr = texture(ture, tex);
	
	color = ambient;
	
	float intensity = clamp(dot(nrml, -direction), 0, 1);
	
	if (intensity > 0) color += intensity*diffuse;
	
	color = clamp(color, 0, 1);
	
	color *= texclr;
}
