#version 450

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec2 inTexCoord;

layout (binding = 1) uniform UboInstance 
{
	mat4 model; 
} uboInstance;


layout(location = 0) out vec2 fragTexCoord;

void main() {
    gl_Position = uboInstance.model * vec4(inPosition, 0.0, 1.0);
    fragTexCoord = inTexCoord;
}

