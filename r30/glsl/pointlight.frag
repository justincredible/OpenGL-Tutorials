#version 460 core

#define NUM_LIGHTS 4

in vec2 tex;
in vec3 normal;
in vec3 lightpos[NUM_LIGHTS];

out vec4 color;

uniform sampler2D ture;
uniform vec4 diffuseClrs[NUM_LIGHTS];

void main()
{	
	float intensity[NUM_LIGHTS];
	vec4 lightclr = vec4(0);
	
	for (int i = 0; i < NUM_LIGHTS; i++) {
		intensity[i] = clamp(dot(normal,lightpos[i]),0,1);
		
		lightclr += diffuseClrs[i]*intensity[i];
	}
	
	vec4 texclr = texture(ture, tex);
	
	color = clamp(lightclr,0,1)*texclr;
}
