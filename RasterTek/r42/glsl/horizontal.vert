#version 460 core

layout(location=0)in vec3 position;
layout(location=1)in vec2 texcoord;

out vec2 tex;
out vec2 tex1;
out vec2 tex2;
out vec2 tex3;
out vec2 tex4;
out vec2 tex5;
out vec2 tex6;
out vec2 tex7;
out vec2 tex8;
out vec2 tex9;

uniform mat4 world;
uniform mat4 view;
uniform mat4 projection;
uniform float width;

void main()
{
	gl_Position = projection*view*world*vec4(position,1);
	
	tex = texcoord;
	
	float texelsz = 1/width;
	
	tex1 = tex + vec2(texelsz*-4,0);
	tex2 = tex + vec2(texelsz*-3,0);
	tex3 = tex + vec2(texelsz*-2,0);
	tex4 = tex + vec2(texelsz*-1,0);
	tex5 = tex + vec2(texelsz*0,0);
	tex6 = tex + vec2(texelsz*1,0);
	tex7 = tex + vec2(texelsz*2,0);
	tex8 = tex + vec2(texelsz*3,0);
	tex9 = tex + vec2(texelsz*4,0);
}
