#version 460 core

in vec2 tex;
in vec3 normal;

out vec4 color;

uniform sampler2D ture;
uniform vec3 direction;
uniform vec4 diffuse;

void main()
{	
	float intensity = clamp(dot(normal, -direction),0,1);
	
	color = diffuse*intensity;
	
	color *= texture(ture, tex);
}
