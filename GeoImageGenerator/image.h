#ifndef __IMAGE_H__
#define __IMAGE_H__

#include "types.h"

/**
 * Bitmap image type
 * data is a list of size width*height, of RGB colors
 */
typedef struct {
    u16 width, height;
    u32 *data;
} Image;

void saveAsPPM(Image ppm, const char *path);
Image loadFromPPM(const char *path);
/**
 * Creates a new image, fills background with black
 */
Image imageAlloc(u16 width, u16 height);
void imageDealloc(Image ppm);

u32 getPixel(Image image, u16 x, u16 y);
void setPixel(Image image, u16 x, u16 y, u32 color);
#endif
