#version 450

layout(binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 proj;
    mat4 view;
} ubo;

layout(location = 0) in vec3 inVertices;
layout(location = 1) in vec3 inTextureCoords;
layout(location = 2) in vec3 inNormals;
layout(location = 3) in vec4 inColors;

layout(location = 0) out vec3 outTextureCoords;

void main() {
    gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inVertices, 1.0);
    outTextureCoords = inTextureCoords;
}
