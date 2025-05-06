#version 440 core

out vec4 FragColor;

struct Circle {
    vec2 location;
    vec3 color;
    float radius;
};

uniform vec2 camera = vec2(0.0, 0.0);
uniform Circle circles[40];
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

    for (int i = 0; i < circles.length(); i++) {
        Circle circle = circles[i];
        float distance = distance(uv, circle.location * zoom - camera);
        float signedDistance = distance - circle.radius * zoom;

        // if (distance > (circle.radius * zoom) + zoom * 50)
        //     continue;

        minimumDistance = smoothMin(minimumDistance, signedDistance, circle.radius * zoom);
        float weight = smoothstep(0.0, distance, circle.radius * zoom);
        finalColor += circle.color * weight;
        totalWeight += weight;
    }

    if (totalWeight > 0.0) {
        finalColor /= totalWeight;
    }

    FragColor = mix(vec4(finalColor, 1.0), vec4(0.0, 0.0, 0.0, 0.0), step(0.0, minimumDistance));
}
