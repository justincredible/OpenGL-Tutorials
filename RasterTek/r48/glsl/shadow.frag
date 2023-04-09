#version 460 core

in vec2 tex;
in vec3 normal;
in vec4 lightviewpos;

out vec4 color;

uniform sampler2D ture;
uniform sampler2D depth;
uniform vec4 ambient;
uniform vec4 diffuse;
uniform vec3 direction;

void main()
{	
	float bias = 0.0025;
	
	color = ambient;
	
	vec2 projectex = vec2(lightviewpos.x/lightviewpos.w/2 + 0.5,lightviewpos.y/lightviewpos.w/2 + 0.5);
	
	float intensity = clamp(dot(normal, -direction),0,1);
	
	if (clamp(projectex,0,1) == projectex)
	{
		float depthval = texture(depth, projectex).r;
		
		float lightdepth = lightviewpos.z/lightviewpos.w;
		
		if (lightdepth - bias < depthval) color = clamp(color + diffuse*intensity,0,1);
	}
	else color = clamp(color + diffuse*intensity,0,1);
	
	color *= texture(ture, tex);
}
