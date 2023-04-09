#version 460 core

in vec2 tex;

out vec4 color;

uniform sampler2D ture;
uniform sampler2D normals;
uniform vec3 direction;

void main()
{
	vec4 texclr = texture(ture, tex);
	
	vec3 normal = texture(normals, tex).xyz;
	
	float intensity = clamp(dot(normal, -direction),0,1);
	
	color = clamp(texclr*intensity,0,1);
}
