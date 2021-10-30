#version 460 core

layout(pixel_center_integer) in vec4 gl_FragCoord;
out vec3 colorOut;

uniform int win_height = 1400;
uniform int win_width = 1400;

uniform float pan_x = 0.0;
uniform float pan_y = 0.0;
uniform float zoom = 100.0;

vec3 palette[4] = {
    vec3(0.0, 0.0, 0.0),
    vec3(1.0, 0.0, 0.0),
    vec3(0.0, 1.0, 0.0),
    vec3(0.0, 0.0, 1.0)
};
int palette_size = 3; // 3 because we ignore the first entry

// Use vec2 to represent complex, like a savage

vec2 product(vec2 a, vec2 b) {
    return vec2(a.x*b.x-a.y*b.y, a.x*b.y+a.y*b.x);
}

float magnitude(vec2 a) {
    return sqrt(a.x * a.x + a.y * a.y);
}

vec3 mandel_color(float x, float y) {
    int max_iterations = 5000;

    vec2 c = vec2(x, y);
    vec2 z = vec2(0.0, 0.0);

    for (int i = 0; i < max_iterations; i++) {
        if (isinf(magnitude(z))) {
            float v0 = float(i) / float(max_iterations);
            // Palette decay so the color distribution isn't so close to the edge
            float v = pow(1.0 - v0, 30.0);

            float pal_coord = v * palette_size;

            vec3 floor_col = palette[int(pal_coord)];
            vec3 ceil_col  = palette[int(pal_coord) + 1];
            float dist = pal_coord - floor(pal_coord);

            // Linear interpolate between upper and lower palette values
            return mix(floor_col, ceil_col, dist);
        }

        z = product(z, z) + c;
    }

    // In the set, paint it black
    return vec3(0.0, 0.0, 0.0);
}

void main() {
    float h = float(win_height);
    float w = float(win_width);

    float screen_x = gl_FragCoord.x;
    float screen_y = h - gl_FragCoord.y;

    float real_pan_x = pan_x + (1 - zoom) / 2;
    float real_pan_y = pan_y + (1 - zoom) / 2;

    float x = screen_x / w;
    float y = screen_y / h;

    colorOut = mandel_color(zoom * x + real_pan_x, zoom * y + real_pan_y);
}
