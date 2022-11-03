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
	
	if (intensity > 0)
	{
		color += diffuse*intensity;
		
		color = clamp(color,0,1);
	}
	
	vec4 texclr = texture(ture, tex);
	
	color *= texclr;
	
	vec2 projectex = vec2(viewpos.x/viewpos.w/2 + 0.5,viewpos.y/viewpos.w/2 + 0.5);
	
	float shadowval = texture(shadow, projectex).r;
	
	color *= shadowval;
}
