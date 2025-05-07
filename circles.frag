#version 440 core

out vec4 FragColor;

#define CIRCLES_COUNT 400

layout (std430, binding = 0) buffer circlePositionsBuffer
{
    vec2 circlePositions[CIRCLES_COUNT];
};

layout (std430, binding = 1) buffer circleColorsBuffer
{
    vec3 circleColors[CIRCLES_COUNT];
};

layout (std430, binding = 2) buffer circlesRadiiBuffer
{
    float circleRadii[CIRCLES_COUNT];
};

uniform vec2 camera = vec2(0.0, 0.0);
uniform float zoom = 1.0;
uniform int windowWidth = 1920;
uniform int windowHeight = 1080;

float smoothMin(float a, float b, float k) {
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return mix(b, a, h) - k*h*(1.0-h);
}

float smax(float a, float b, float k) {
    return -smoothMin(-a, -b, k);
}

void main() {
    vec2 uv = gl_FragCoord.xy / max(windowHeight, windowWidth);

    vec3 finalColor = vec3(0.0, 0.0, 0.0);
    float minimumDistance = 2000.0;
    float totalWeight = 0.0;

    for (int i = 0; i < CIRCLES_COUNT; i++) {
        vec2 position = circlePositions[i];
        vec3 color = circleColors[i];
        float radius = circleRadii[i];

        float distance = distance(uv, (position - camera) * zoom);
        float signedDistance = distance - radius * zoom;

        if (distance > (radius * zoom) + zoom * 10)
            continue;

        minimumDistance = smoothMin(minimumDistance, signedDistance, radius * zoom);
        float weight = smoothstep(0.0, distance, radius * zoom);
        finalColor += color * weight;
        totalWeight += weight;
    }

    if (totalWeight > 0.0) {
        finalColor /= totalWeight;
    }

    //FragColor = vec4(finalColor, 1.0);
    FragColor = mix(vec4(finalColor, 1.0), vec4(0.0, 0.0, 0.0, 0.0), step(0.0, minimumDistance));
}
