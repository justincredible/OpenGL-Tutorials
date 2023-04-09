#version 460 core

in vec2 tex;
in vec2 tex1;
in vec2 tex2;
in vec2 tex3;
in vec2 tex4;
in vec2 tex5;
in vec2 tex6;
in vec2 tex7;
in vec2 tex8;
in vec2 tex9;

out vec4 color;

uniform sampler2D ture;

void main()
{	
	float normalization = 1 + 2*(0.9 + 0.55 + 0.18 + 0.1);
	float weight0 = 1/normalization;
	float weight1 = 0.9/normalization;
	float weight2 = 0.55/normalization;
	float weight3 = 0.18/normalization;
	float weight4 = 0.1/normalization;
	
	color = vec4(0);
	
	color += texture(ture, tex1)*weight4;
	color += texture(ture, tex2)*weight3;
	color += texture(ture, tex3)*weight2;
	color += texture(ture, tex4)*weight1;
	color += texture(ture, tex5)*weight0;
	color += texture(ture, tex6)*weight1;
	color += texture(ture, tex7)*weight2;
	color += texture(ture, tex8)*weight3;
	color += texture(ture, tex9)*weight4;
	
	color.a = 1;
}
