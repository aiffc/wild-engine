#version 450

layout(push_constant) uniform PushConsts {
	vec4 position;
	vec4 color;
} pushConsts;

layout(location = 0) in vec3 inPosition;

layout(location = 0) out vec4 fragColor;

void main() {
    gl_Position = vec4(inPosition, 1.0) + pushConsts.position;
    fragColor = pushConsts.color.rgba;
}
