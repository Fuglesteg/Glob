#version 330 core

out vec4 FragColor;

struct Circle {
    vec2 location;
    vec3 color;
    float radius;
};

uniform vec2 camera = vec2(0.0, 0.0);
uniform Circle circles[40];
uniform float zoom = 1.0;

float smoothMin(float a, float b, float k) {
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return mix(b, a, h) - k*h*(1.0-h);
}

float smax(float a, float b, float k) {
    return -smoothMin(-a, -b, k);
}

float sdCircle(vec2 uv, float r, vec2 offset) {
    float x = uv.x - offset.x;
    float y = uv.y - offset.y;

    return length(vec2(x, y)) - r;
}

void main() {
    vec2 uv = gl_FragCoord.xy / 1000; // TODO: Get fragment size??

    vec3 finalColor = vec3(0.0, 0.0, 0.0);
    float minimumDistance = 2000.0;
    float totalWeight = 0.0;

    for (int i = 0; i < circles.length(); i++) {
        Circle circle = circles[i];
        float distance = sdCircle(uv, circle.radius * zoom, circle.location * zoom - camera);
        minimumDistance = smoothMin(minimumDistance, distance, circle.radius * zoom);
        float t = smoothstep(0.0, circle.radius, distance / zoom);
        float h = clamp(0.5+0.5*(minimumDistance - distance)/(circle.radius * zoom), 0.0, circle.radius * zoom);
        float weight = h - (circle.radius * zoom) * h * (1.0-h);
        finalColor += circle.color * weight;
        totalWeight += weight;
    }

    if (totalWeight > 0.0) {
        finalColor /= totalWeight;
    }

    finalColor = mix(finalColor, vec3(0.0, 0.0, 0.0), step(0.0, minimumDistance));
    FragColor = vec4(finalColor, 1.0);
    // AI slop
    /*
    vec2 uv = gl_FragCoord.xy / 1000.0;

    vec3 finalColor = vec3(0.0);
    float totalWeight = 0.0;
    float blendedDistance = 2000.0; // Track blended distance separately
    float influence[40]; // Stores each circle's contribution

    // First pass: Compute blended distance and influences
    for (int i = 0; i < circles.length(); i++) {
        Circle circle = circles[i];
        float distance = sdCircle(uv, circle.radius * zoom, circle.location * zoom - camera);
        float k = circle.radius * zoom;

        // Compute blending factor `h` for this circle
        float h = clamp(0.5 + 0.5*(blendedDistance - distance)/k, 0.0, 1.0);
        influence[i] = (1.0 - h); // Influence of this circle

        // Update blended distance using smoothMin logic
        blendedDistance = mix(blendedDistance, distance, h) - k * h * (1.0 - h);
    }

    // Second pass: Calculate weights based on final blended distance
    for (int i = 0; i < circles.length(); i++) {
        Circle circle = circles[i];
        float distance = sdCircle(uv, circle.radius * zoom, circle.location * zoom - camera);

        // Weight = influence * falloff based on distance to final shape
        float weight = influence[i] * smoothstep(1.0, 0.0, abs(distance - blendedDistance));
        finalColor += circle.color * weight;
        totalWeight += weight;
    }

    // Normalize and apply background
    if (totalWeight > 0.0) finalColor /= totalWeight;
    finalColor = mix(finalColor, vec3(0.0), smoothstep(-1.0, 1.0, blendedDistance));
    FragColor = vec4(finalColor, 1.0);
    */
}
