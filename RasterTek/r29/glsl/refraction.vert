#version 460 core

layout(location=0)in vec3 position;
layout(location=1)in vec2 texcoord;
layout(location=2)in vec3 facenrml;

out vec2 tex;
out vec3 normal;
out float clip;

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;
uniform vec4 clipplane;

void main()
{
	vec4 worldpos = world*vec4(position, 1.0);
	gl_Position = projection*view*worldpos;
	
	tex = texcoord;
	
	normal = normalize(mat3(world)*facenrml);
	
	clip = dot(worldpos, clipplane);
}
