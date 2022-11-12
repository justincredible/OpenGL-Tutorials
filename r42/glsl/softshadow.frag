#version 460 core

in vec2 tex;
in vec3 normal;
in vec4 viewpos;
in vec3 lightpos;

out vec4 color;

uniform sampler2D ture;
uniform sampler2D shadow;
uniform vec4 ambient;
uniform vec4 diffuse;

void main()
{	
	color = ambient;
	
	float intensity = clamp(dot(normal, lightpos),0,1);
	
	color = clamp(color + diffuse*intensity,0,1);
	
	color *= texture(ture, tex);
	
	vec2 projectex = vec2(viewpos.x/viewpos.w/2 + 0.5,viewpos.y/viewpos.w/2 + 0.5);
	
	color *= texture(shadow, projectex).r;
}
