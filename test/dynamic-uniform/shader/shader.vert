#version 450

layout(binding = 0) uniform UniformBufferObject {
    mat4 proj;
    mat4 view;
} ubo;

layout(binding = 1) uniform uboModel{
    mat4 model;
} ubom;

layout(location = 0) in vec3 inVertices;
layout(location = 1) in vec3 inColors;

layout(location = 0) out vec3 outColor;

void main() {
    gl_Position = ubom.model * vec4(inVertices, 1.0);
    outColor = inColors;
}
