#version 450

layout(location = 0) in vec3 inVertices;
layout(location = 1) in vec3 inTextureCoords;
layout(location = 2) in vec3 inNormals;
layout(location = 3) in vec4 inColors;

layout(location = 0) out vec4 fragColor;

void main() {
    gl_Position = vec4(inVertices, 1.0);
    fragColor = inColors;
}
