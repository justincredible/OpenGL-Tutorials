#version 460 core

#define NUM_DISTORTS 3

in vec2 tex;
in vec2 texcrd1;
in vec2 texcrd2;
in vec2 texcrd3;

out vec4 color;

uniform sampler2D fire;
uniform sampler2D noise;
uniform sampler2D alpha;
uniform vec2 distortions[NUM_DISTORTS];
uniform float distortscale;
uniform float distortbias;


void main()
{
	vec4 finalnoise =
		2*(texture(noise, texcrd1) - 0.5)*vec4(distortions[0],1,1) +
		2*(texture(noise, texcrd2) - 0.5)*vec4(distortions[1],1,1) +
		2*(texture(noise, texcrd3) - 0.5)*vec4(distortions[2],1,1);
	
	float perturb = tex.y*distortscale + distortbias;
	
	vec2 noisecrd = finalnoise.xy*perturb + tex;
	
	vec4 alphaclr = texture(alpha, noisecrd);
	
	color = texture(fire, noisecrd);
	color.a = alphaclr.x;
}
