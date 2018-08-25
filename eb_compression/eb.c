#include <getopt.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// It's not worth compressing if the region is this tiny
#define MINIMUM_REGION_SIZE (4)

struct Region {
    uint32_t color;
    uint16_t x, y, width, height;
};

struct EBImage {
    uint16_t width, height;
    uint32_t region_count;
    struct Region *regions;
    // At most width*height. If regions exist, it will be less.
    uint32_t data_length;
    uint32_t *data;
};

/* 
 * Raw image data type. Mainly for loading and saving PPMs.
 */
struct RawImage {
    uint16_t width, height;
    uint32_t *data;
};

/*
 * Returns a raw bitmap array for the given EBImage.
 */
struct RawImage EBImage_to_raw(struct EBImage *im) {
    struct RawImage raw;
    raw.width = im->width;
    raw.height = im->height;
    raw.data = malloc(sizeof(uint32_t) * im->width * im->height);

    // Map out which pixels have been set so far (via regions)
    uint8_t *region_map = malloc(im->width * im->height);
    memset(region_map, 0, im->width * im->height);

    // draw the regions
    for (uint32_t i = 0; i < im->region_count; i++) {
        struct Region *reg = &im->regions[i];
        for (uint32_t y = reg->y; y < reg->y + reg->height; y++) {
            for (uint32_t x = reg->x; x < reg->x + reg->width; x++) {
                raw.data[y * im->width + x] = reg->color;
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

        raw.data[iter - region_map] = im->data[data_index];
        data_index++;
    }

    free(region_map);
    return raw;
}

void save_ppm(struct RawImage *raw, const char *path) {
    FILE *fp = fopen(path, "wb");
    fprintf(fp, "P6\n%d %d\n255\n", raw->width, raw->height);

    for (uint32_t y = 0; y < raw->height; y++) {
        for (uint32_t x = 0; x < raw->width; x++) {
            uint8_t *rawColor =
                (uint8_t*)&raw->data[y * raw->width + x];
            uint8_t color[3];

            // Write RGB channels (PPM doesn't have alpha)
            color[0] = rawColor[3]; // r
            color[1] = rawColor[2]; // g
            color[2] = rawColor[1]; // b
            fwrite(color, 1, 3, fp);
        }
    }
}

struct RawImage load_ppm(const char *path) {
    FILE *fp = fopen(path, "rb");
    uint32_t width, height;
    fscanf(fp, "P6\n%d %d\n255\n", &width, &height);

    struct RawImage raw;
    raw.width  = width;
    raw.height = height;
    raw.data = malloc(sizeof(uint32_t) * width * height);

    for (uint32_t y = 0; y < height; y++) {
        for (uint32_t x = 0; x < width; x++) {
            uint8_t color[3];
            fread(color, 1, 3, fp);

            raw.data[y * raw.width + x] =
                (color[0] << 24) // r
                + (color[1] << 16) // g
                + (color[2] << 8); // b
        }
    }

    return raw;
}

void grow_rect(struct RawImage *raw,
               uint8_t *visited_map,
               const uint32_t x, const uint32_t y,
               uint32_t *out_width, uint32_t *out_height) {
    uint32_t color = raw->data[y * raw->height + x];
    uint32_t rwidth = 1, rheight = 1;
    uint8_t width_done = 0, height_done = 0;

    while (!width_done || !height_done) {
        if (y + rheight >= raw->height) {
            height_done = 1;
        }
        if (x + rwidth >= raw->width) {
            width_done = 1;
        }

        // Grow the width, if appropriate
        if (!width_done) {
            for (uint32_t i = y; i < y + rheight; i++) {
                uint32_t coord = i * raw->height + x + rwidth;
                if (raw->data[coord] != color || visited_map[coord]) {
                    width_done = 1;
                    break;
                }
            }

            if (!width_done) {
                rwidth++;
            }
        }

        // Grow the height, if appropriate
        if (!height_done) {
            for (uint32_t i = x; i < x + rwidth; i++) {
                uint32_t coord = (y + rheight) * raw->height + i;
                if (raw->data[coord] != color || visited_map[coord]) {
                    height_done = 1;
                    break;
                }
            }

            if (!height_done) {
                rheight++;
            }
        }
    }

    *out_width  = rwidth;
    *out_height = rheight;
}

void EBImage_add_region(struct EBImage *eb, const struct Region r) {
    const size_t new_region_mem =
        sizeof(struct Region) * ++(eb->region_count);
    eb->regions = realloc(eb->regions, new_region_mem);
    if (eb->regions == 0) {
        fprintf(stderr, "Out of memory! "
                "(tried allocating %lu regions)\n", new_region_mem);
        exit(1);
    }
    eb->regions[eb->region_count - 1] = r;
}

void EBImage_append_data(struct EBImage *eb, const uint32_t color) {
    eb->data_length++;
    eb->data = realloc(eb->data,
                       sizeof(uint32_t) * eb->data_length);
    eb->data[eb->data_length - 1] = color;
}

struct EBImage EBImage_compress_raw(struct RawImage *raw) {
    struct EBImage eb = { .width        = raw->width,
                          .height       = raw->height,
                          .region_count = 0,
                          .regions      = malloc(1),
                          .data_length  = 0,
                          .data         = malloc(1)};
    
    uint8_t *visited_map = malloc(raw->width * raw->height);
    memset(visited_map, 0, raw->width * raw->height);

    for (uint32_t y = 0; y < raw->height; y++) {
        for (uint32_t x = 0; x < raw->width; x++) {
            if (visited_map[y * raw->width + x]) {
                continue;
            }

            const uint32_t color = raw->data[y * raw->width + x];

            uint32_t rwidth, rheight;
            grow_rect(raw, visited_map, x, y, &rwidth, &rheight);

            if (rwidth * rheight < MINIMUM_REGION_SIZE) {
                // Boring 'ol raw data
                EBImage_append_data(&eb, color);
                continue;
            }

            struct Region r;
            r.color  = color;
            r.x      = x;
            r.y      = y;
            r.width  = rwidth;
            r.height = rheight;
            EBImage_add_region(&eb, r);

            // Set the visited region
            for (uint32_t i = x; i < x + rwidth; i++) {
                for (uint32_t j = y; j < y + rheight; j++) {
                    visited_map[j * raw->width + i] = 1;
                }
            }
        }
    }

    free(visited_map);
    return eb;
}

void EBImage_save(struct EBImage *eb, const char *path) {
    FILE *fp = fopen(path, "wb");
    fwrite(&eb->width, 2, 1, fp);
    fwrite(&eb->height, 2, 1, fp);
    fwrite(&eb->region_count, 4, 1, fp);

    // Write the compressed regions
    for (uint32_t i = 0; i < eb->region_count; i++) {
        struct Region *r = &eb->regions[i];

        // RBG (no alpha)
        fwrite(&r->color, 3, 1, fp);
        fwrite(&r->x, 2, 1, fp);
        fwrite(&r->y, 2, 1, fp);
        fwrite(&r->width, 2, 1, fp);
        fwrite(&r->height, 2, 1, fp);
    }

    // Write remaining uncompressed data
    fwrite(&eb->data_length, 4, 1, fp);
    for (uint32_t i = 0; i < eb->data_length; i++) {
        fwrite(&eb->data[i], 3, 1, fp);
    }
}

struct EBImage EBImage_load(const char *path) {
    struct EBImage eb;
    eb.region_count = 0;

    FILE *fp = fopen(path, "rb");
    fread(&eb.width, 2, 1, fp);
    fread(&eb.height, 2, 1, fp);

    uint32_t region_count;
    fread(&region_count, 4, 1, fp);
    eb.regions = malloc(1);

    // The area taken up by regions, not raw data.
    uint32_t region_area = 0;

    for (uint32_t i = 0; i < region_count; i++) {
        struct Region r;

        fread(&r.color, 3, 1, fp);
        fread(&r.x, 2, 1, fp);
        fread(&r.y, 2, 1, fp);
        fread(&r.width, 2, 1, fp);
        fread(&r.height, 2, 1, fp);

        EBImage_add_region(&eb, r);

        region_area += r.width * r.height;
    }

    eb.data_length = eb.width * eb.height - region_area;

    eb.data = malloc(sizeof(uint32_t) * eb.data_length);
    for (uint32_t i = 0; i < eb.data_length; i++) {
        fread(&eb.data[i], 3, 1, fp);
    }

    return eb;
}

void EBImage_dealloc(struct EBImage *eb) {
    free(eb->regions);
    free(eb->data);
}

void raw_dealloc(struct RawImage *raw) {
    free(raw->data);
}

void exit_usage() {
    printf("Usage: eb <in>.ppm <out>.eb\n"
           " or    eb <in>.eb <out>.ppm\n");
    exit(1);
}

int has_extension(char *str, const char *ext) {
    str = strrchr(str, '.');
    if (str == 0 || ++str == 0) {
        return 0;
    }

    return strcmp(str, ext) == 0;
}

int main(int argc, char **argv) {
    if (argc != 3) {
        exit_usage();
    }

    char *from_file = argv[1];
    char *to_file = argv[2];

    int should_convert_to_eb = has_extension(to_file, "eb");

    if (should_convert_to_eb) {
        if (!has_extension(from_file, "ppm")) {
            exit_usage();
        }
    } else {
        if (!has_extension(from_file, "eb")) {
            exit_usage();
        }
    }

    if (should_convert_to_eb) {
        struct RawImage raw = load_ppm(from_file);
        struct EBImage eb = EBImage_compress_raw(&raw);
        EBImage_save(&eb, to_file);
        EBImage_dealloc(&eb);
        raw_dealloc(&raw);
    } else {
        struct EBImage eb = EBImage_load(from_file);
        struct RawImage raw = EBImage_to_raw(&eb);
        save_ppm(&raw, to_file);
        EBImage_dealloc(&eb);
        raw_dealloc(&raw);
    }
}
