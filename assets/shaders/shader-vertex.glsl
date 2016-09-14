#version 440

precision mediump float;

attribute vec3 aVertexPosition;
attribute vec4 aVertexColor;
attribute vec2 aTexCoord;

uniform mat4 uMVMatrix;
uniform mat4 uPMatrix;

uniform sampler2D uTex0;
uniform vec3 uMouseVec;

varying vec4 vColor;
varying vec2 vTexCoord;

in int gl_VertexID;
in int gl_InstanceID;

void main(void) {
	gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
  // gl_Position.z += 0.0005 * distance(gl_Position.xy, uMouseVec.xy);

	vColor    = aVertexColor;
	vTexCoord = vec2(aTexCoord.s, 1-aTexCoord.t);
}