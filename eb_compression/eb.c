#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Region {
    uint8_t type;
    uint32_t color;
    uint32_t x, y, width, height;
};

struct EBImage {
    uint32_t width, height;
    uint16_t region_count;
    struct Region *regions;
    // At most width*height. If regions exist, it will be less.
    uint64_t data_length;
    uint32_t *data;
};

/*
 * Returns a raw bitmap array for the given EBImage.
 */
uint32_t * EBImage_to_raw(struct EBImage *im) {
    uint32_t *raw = malloc(sizeof(uint32_t) * im->width * im->height);
    // Map out which pixels have been set so far (via regions)
    uint8_t *region_map = malloc(im->width * im->height);
    memset(region_map, 0, im->width * im->height);

    // draw the regions
    for (uint16_t i = 0; i < im->region_count; i++) {
        struct Region *reg = &im->regions[i];
        for (uint32_t y = reg->y; y < reg->y + reg->height; y++) {
            for (uint32_t x = reg->x; x < reg->x + reg->width; x++) {
                raw[y * im->width + x] = reg->color;
                region_map[y * im->width + x] = 1;
            }
        }
    }

    // draw remaining data
    uint8_t *map_end = &region_map[im->width * im->height];
    uint64_t data_index;
    uint8_t *iter;
    for (iter = region_map, data_index = 0; iter <= map_end; iter++) {
        if (*iter == 1) {
            continue;
        }

        raw[iter - region_map] = im->data[data_index];
        data_index++;
    }

    return raw;
}

void save_ppm(uint32_t *raw, uint32_t width, uint32_t height,
              const char *path) {
    FILE *fp = fopen(path, "wb");
    fprintf(fp, "P6\n%d %d\n255\n", width, height);

    for (uint32_t y = 0; y < height; y++) {
        for (uint32_t x = 0; x < width; x++) {
            uint8_t *rawColor = (uint8_t*)&raw[y * width + x];
            uint8_t color[3];

            // Write RGB channels (PPM doesn't have alpha)
            color[0] = rawColor[3]; // r
            color[1] = rawColor[2]; // g
            color[2] = rawColor[1]; // b
            fwrite(color, 1, 3, fp);
        }
    }
}

int main() {
    struct Region regions[1];
    regions[0] = (struct Region){ .type   = 1,
                                  .color  = 0x00ff00ff,
                                  .x      = 0,
                                  .y      = 0,
                                  .width  = 8,
                                  .height = 8 };

    struct EBImage im = { .width        = 16,
                          .height       = 16,
                          .region_count = 1 };
    im.regions = regions;
    im.data_length = im.width * im.height -
        regions[0].width * regions[0].height;
    im.data = (uint32_t*)malloc(im.data_length * sizeof(uint32_t));

    for (uint64_t i = 0; i < im.data_length; i++) {
        im.data[i] = 0xff0000ff;
    }

    uint32_t *raw = EBImage_to_raw(&im);

    save_ppm(raw, im.width, im.height, "test.ppm");

    free(im.data);
    free(raw);
}
