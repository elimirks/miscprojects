#version 460 core
#define IN_SET_SPLIT_COUNT 1000

layout(pixel_center_integer) in vec4 gl_FragCoord;
out vec3 colorOut;

uniform int win_height = 1400;
uniform int win_width = 1400;

uniform float pan_x = 0.0;
uniform float pan_y = 0.0;
uniform float zoom = 100.0;

// orig refers to the top of the triangle
bool in_triangle(float x, float y, float orig_x, float orig_y, float height) {
    if (y > orig_y + height || y < orig_y) {
        return false;
    }
    if (x > orig_x + (y - orig_y) * tan(radians(30))) {
        return false;
    }
    if (x < orig_x - (y - orig_y) * tan(radians(30))) {
        return false;
    }
    return true;
}

struct SierpRec {
    float orig_x;
    float orig_y;
    float height;
    int splits;
};

int sierpienski_split_count(float x, float y, float orig_x, float orig_y, float height, int splits) {
    int min_splits = 1000000;
    // Because GLSL doesn't support recusnion :(
    SierpRec stack[1024];
    int len = 1;
    stack[0].orig_x = orig_x;
    stack[0].orig_y = orig_y;
    stack[0].height = height;
    stack[0].splits = splits;
    while (len > 0) {
        SierpRec it = stack[len - 1];
        len--;
        if (!in_triangle(x, y, it.orig_x, it.orig_y, it.height)) {
            continue;
        } else if (it.splits == 0) {
            // If it's in the triangle and we're at the base case
            return IN_SET_SPLIT_COUNT;
        }
        min_splits = min(min_splits, it.splits);

        float new_height = it.height / 2.0;
        // Upper triangle
        stack[len].orig_x = it.orig_x;
        stack[len].orig_y = it.orig_y;
        stack[len].height = new_height;
        stack[len].splits = it.splits - 1;
        len++;
        // Bottom left triangle
        stack[len].orig_x = it.orig_x - new_height * tan(radians(30));
        stack[len].orig_y = it.orig_y + new_height;
        stack[len].height = it.height / 2;
        stack[len].splits = it.splits - 1;
        len++;
        // Bottom right triangle
        stack[len].orig_x = it.orig_x + new_height * tan(radians(30));
        stack[len].orig_y = it.orig_y + new_height;
        stack[len].height = it.height / 2;
        stack[len].splits = it.splits - 1;
        len++;
    }
    return min_splits;
}

vec3 palette[4] = {
    vec3(0.0, 0.0, 0.0),
    vec3(0.0, 0.0, 1.0),
    vec3(1.0, 0.0, 1.0),
    vec3(1.0, 1.0, 0.0),
};
int palette_size = 3;

// Bounds: x from 0 to 1, y from 0 to 1
// At what points should we colour in the triangle?
vec3 fract_color(float x, float y) {
    int splits = sierpienski_split_count(x, y, 0.0, 0.0, 0.7, 12);
    if (splits == IN_SET_SPLIT_COUNT) {
        return vec3(1.0, 1.0, 1.0);
    } else {
        float v = pow(1.0 - (float(splits) / 10.0), 1.0);
        float pal_coord = v * palette_size;

        vec3 floor_col = palette[int(pal_coord)];
        vec3 ceil_col  = palette[int(pal_coord) + 1];
        float dist = pal_coord - floor(pal_coord);
        return mix(floor_col, ceil_col, dist);
    }
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

    colorOut = fract_color(zoom * x + real_pan_x, zoom * y + real_pan_y);
}
