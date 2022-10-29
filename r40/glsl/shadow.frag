#version 460 core

in vec2 tex;
in vec3 normal;
in vec4 lightviewpos;
in vec3 lightpos;

out vec4 color;

uniform sampler2D ture;
uniform sampler2D depth;
uniform vec4 ambient;
uniform vec4 diffuse;

void main()
{	
	float bias = 0.0025;
	
	color = ambient;
	
	vec2 projectex = vec2(lightviewpos.x/lightviewpos.w/2 + 0.5,lightviewpos.y/lightviewpos.w/2 + 0.5);
	
	if (clamp(projectex,0,1) == projectex)
	{
		float depthval = texture(depth, projectex).r;
		
		float lightdepth = lightviewpos.z/lightviewpos.w;
		
		if (lightdepth - bias < depthval)
		{
			float intensity = clamp(dot(normal, lightpos),0,1);
			
			color += diffuse*intensity;
			
			if (intensity > 0) color = clamp(color,0,1);
		}
	}
	
	vec4 texclr = texture(ture, tex);
	
	color *= texclr;
}
