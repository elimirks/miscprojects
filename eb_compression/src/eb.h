#ifndef __EB_H__
#define __EB_H__

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// It's not worth compressing if the region is this tiny
#define MINIMUM_REGION_SIZE (4)

struct EBRegion {
    uint32_t color;
    uint16_t x, y, width, height;
};

struct EBImage {
    uint16_t width, height;
    uint32_t region_count;
    struct EBRegion *regions;
    // At most width*height. If regions exist, it will be less.
    uint32_t data_length;
    uint32_t *data;
};

/* 
 * Raw image data type.
 */
struct PPMImage {
    uint16_t width, height;
    uint32_t *data;
};

/*
 * Returns a PPM for the given EBImage.
 */
struct PPMImage EBImage_to_ppm(struct EBImage *eb);
void PPMImage_save(struct PPMImage *ppm, const char *path);
struct PPMImage PPMImage_load(const char *path);
void PPMImage_dealloc(struct PPMImage *ppm);

void EBImage_add_region(struct EBImage *eb,
                        const struct EBRegion r);
void EBImage_append_data(struct EBImage *eb, const uint32_t color);
struct EBImage EBImage_compress_ppm(struct PPMImage *ppm);
void EBImage_save(struct EBImage *eb, const char *path);
struct EBImage EBImage_load(const char *path);
void EBImage_dealloc(struct EBImage *eb);

#endif
