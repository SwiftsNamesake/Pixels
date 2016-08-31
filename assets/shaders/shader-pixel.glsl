#version 440

precision mediump float;

in highp vec2 vTexCoord;
in vec4 gl_FragCoord; // Not sure if this is needed

uniform float     uTime;
uniform vec3      uMouseVec;
uniform sampler2D uTex0;
// uniform sampler2D tex1;

// layout(location = 0) out vec3 color; // Output to texture

varying vec4 vColor;


void attenuate(in float dist, in float a, in float b, out float intensity) {
	intensity = a / (a + b*dist + b*b*dist*dist);
}


// void cone() {
  // Directional lighting
// }


void main(void) {

	float total=0.0, intensity, a=2.0, b=0.02;

	// float d = distance(gl_FragCoord.xy, uMouseVec.xy);
	// const float π  = 3.1415926535897932384626433832795; // Unicode allowed in comments?
	const float pi = 3.1415926535897932384626433832795;

	float omega = 1.2;
	float theta = 2*pi*uTime*omega;

	vec4 lights[4] = vec4[4](vec4(100, 100, 30.0, 0.10),
		                       vec4(360, 120,  2.2, 0.05),
		                       vec4(700, 200,  4.5, 0.01),
		                       vec4(uMouseVec.xy, 2.0 + 8*0.5*(1+sin(theta)), 0.02));

  for (int i=0; i < lights.length(); ++i) {
	  attenuate(distance(gl_FragCoord.xy, lights[i].xy), lights[i].z, lights[i].w, intensity);
	  total += intensity;
  }
  
  gl_FragColor = clamp(total, 0.0, 1.0) * vColor * texture(uTex0, vTexCoord);
  
}
