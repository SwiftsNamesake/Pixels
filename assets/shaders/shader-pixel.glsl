#version 440

precision mediump float;
//

uniform sampler2D tex0;
uniform sampler2D tex1;

varying vec4 vColor;

void main(void) {
	gl_FragColor = vColor;
}
