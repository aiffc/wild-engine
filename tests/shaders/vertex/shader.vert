#version 450

layout(location = 0) in vec3 v;
layout(location = 1) in vec3 vt;

layout(location = 0) out vec3 fragColor;

void main() {
    gl_Position = vec4(v, 1.0);
    fragColor = vt;
}