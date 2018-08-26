#include "eb.h"

struct EBRegion;

/*
 * Returns a raw bitmap array for the given EBImage.
 */
struct PPMImage EBImage_to_ppm(struct EBImage *eb) {
    struct PPMImage ppm;
    ppm.width = eb->width;
    ppm.height = eb->height;
    ppm.data = malloc(sizeof(uint32_t) * eb->width * eb->height);

    // Map out which pixels have been set so far (via regions)
    uint8_t *region_map = malloc(eb->width * eb->height);
    memset(region_map, 0, eb->width * eb->height);

    // draw the regions
    for (uint32_t i = 0; i < eb->region_count; i++) {
        struct EBRegion *reg = &eb->regions[i];
        for (uint32_t y = reg->y; y < reg->y + reg->height; y++) {
            for (uint32_t x = reg->x; x < reg->x + reg->width; x++) {
                ppm.data[y * eb->width + x] = reg->color;
                region_map[y * eb->width + x] = 1;
            }
        }
    }

    // draw remaining data
    uint8_t *map_end = &region_map[eb->width * eb->height];
    uint64_t data_index;
    uint8_t *iter;
    for (iter = region_map, data_index = 0; iter <= map_end; iter++) {
        if (*iter == 1) {
            continue;
        }

        ppm.data[iter - region_map] = eb->data[data_index];
        data_index++;
    }

    free(region_map);
    return ppm;
}

void PPMImage_save(struct PPMImage *ppm, const char *path) {
    FILE *fp = fopen(path, "wb");
    fprintf(fp, "P6\n%d %d\n255\n", ppm->width, ppm->height);

    for (uint32_t y = 0; y < ppm->height; y++) {
        for (uint32_t x = 0; x < ppm->width; x++) {
            fwrite(&ppm->data[y * ppm->width + x], 1, 3, fp);
        }
    }
}

struct PPMImage PPMImage_load(const char *path) {
    FILE *fp = fopen(path, "rb");
    uint32_t width, height;
    fscanf(fp, "P6\n%d %d\n255\n", &width, &height);

    struct PPMImage raw;
    raw.width  = width;
    raw.height = height;
    raw.data = malloc(sizeof(uint32_t) * width * height);

    for (uint32_t y = 0; y < height; y++) {
        for (uint32_t x = 0; x < width; x++) {
            fread(&raw.data[y * raw.width + x], 1, 3, fp);
        }
    }

    return raw;
}

void EBImage_grow_rect(struct PPMImage *raw,
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

void EBImage_add_region(struct EBImage *eb, const struct EBRegion r) {
    const size_t new_region_mem =
        sizeof(struct EBRegion) * ++(eb->region_count);
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

struct EBImage EBImage_compress_ppm(struct PPMImage *ppm) {
    struct EBImage eb = { .width        = ppm->width,
                          .height       = ppm->height,
                          .region_count = 0,
                          .regions      = malloc(1),
                          .data_length  = 0,
                          .data         = malloc(1)};
    
    uint8_t *visited_map = malloc(ppm->width * ppm->height);
    memset(visited_map, 0, ppm->width * ppm->height);

    for (uint32_t y = 0; y < ppm->height; y++) {
        for (uint32_t x = 0; x < ppm->width; x++) {
            if (visited_map[y * ppm->width + x]) {
                continue;
            }

            const uint32_t color = ppm->data[y * ppm->width + x];

            uint32_t rwidth, rheight;
            EBImage_grow_rect(ppm, visited_map, x, y, &rwidth, &rheight);

            if (rwidth * rheight < MINIMUM_REGION_SIZE) {
                // Boring 'ol ppm data
                EBImage_append_data(&eb, color);
                continue;
            }

            struct EBRegion r;
            r.color  = color;
            r.x      = x;
            r.y      = y;
            r.width  = rwidth;
            r.height = rheight;
            EBImage_add_region(&eb, r);

            // Set the visited region
            for (uint32_t i = x; i < x + rwidth; i++) {
                for (uint32_t j = y; j < y + rheight; j++) {
                    visited_map[j * ppm->width + i] = 1;
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
        struct EBRegion *r = &eb->regions[i];

        // RBG (no alpha)
        fwrite(&r->color, 1, 3, fp);
        fwrite(&r->x, 2, 1, fp);
        fwrite(&r->y, 2, 1, fp);
        fwrite(&r->width, 2, 1, fp);
        fwrite(&r->height, 2, 1, fp);
    }

    // Write remaining uncompressed data
    for (uint32_t i = 0; i < eb->data_length; i++) {
        fwrite(&eb->data[i], 1, 3, fp);
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
        struct EBRegion r;

        fread(&r.color, 1, 3, fp);
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
        fread(&eb.data[i], 1, 3, fp);
    }

    return eb;
}

void EBImage_dealloc(struct EBImage *eb) {
    free(eb->regions);
    free(eb->data);
}

void PPMImage_dealloc(struct PPMImage *raw) {
    free(raw->data);
}
