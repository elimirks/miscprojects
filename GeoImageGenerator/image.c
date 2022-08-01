#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"
#include "image.h"

void saveAsPPM(Image image, const char *path) {
    FILE *fp = fopen(path, "wb");
    fprintf(fp, "P6\n%d %d\n255\n", image.width, image.height);

    for (u32 y = 0; y < image.height; y++) {
        for (u32 x = 0; x < image.width; x++) {
            u32 rgbPixel = image.data[y * image.width + x];

            // Flip to BGR, as per PPM spec
            u32 bgrPixel =
                ((rgbPixel & 0xff0000) >> 16) + 
                ((rgbPixel & 0x0000ff) << 16) + 
                ((rgbPixel & 0x00ff00));

            fwrite(&bgrPixel, 1, 3, fp);
        }
    }
}

Image imageAlloc(u16 width, u16 height) {
    size_t size = sizeof(u32) * width * height;

    Image blank;
    blank.height = height;
    blank.width = width;
    blank.data = malloc(size);
    memset(blank.data, 0, size);

    return blank;
}

void imageDealloc(Image image) {
    free(image.data);
}

u32 getPixel(Image image, u16 x, u16 y) {
    return image.data[x + image.width * y];
}

void setPixel(Image image, u16 x, u16 y, u32 color) {
    image.data[x + image.width * y] = color;
}
