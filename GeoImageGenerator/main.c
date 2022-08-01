#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "types.h"
#include "image.h"
#include "color.h"

/**
 * Loads an image based on a given geocode file list
 * The data for each pixel are the counts of POIs at that location
 * Renders as an equirectangular projection
 */
Image loadGeocodesAsImage(const char *path, u16 width, u16 height) {
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    fp = fopen(path, "r");
    if (fp == NULL) {
        exit(EXIT_FAILURE);
    }

    Image image = imageAlloc(width, height);
    while ((read = getline(&line, &len, fp)) != -1) {
        char *latString = strtok(line, ",");
        char *lonString = strtok(NULL, ",");

        if (latString == NULL || lonString == NULL) {
            continue;
        }

        float lat = strtof(latString, NULL);
        float lon = strtof(lonString, NULL);

        float rectLat = 1.f - (lat + 90.f) / 180.f;
        float rectLon = (lon + 180.f) / 360.f;

        u16 x = rectLon * width;
        u16 y = rectLat * height;

        u32 currentValue = getPixel(image, x, y);
        setPixel(image, x, y, currentValue + 1);
    }
    fclose(fp);

    return image;
}

u32 findMaxPixel(Image image) {
    u32 maxPixel = 0;
    for (int i = 0; i < image.width * image.height; i++) {
        if (image.data[i] > maxPixel) {
            maxPixel = image.data[i];
        }
    }
    return maxPixel;
}

/**
 * Converts a hue into an RGB color, with max saturation & value
 * Hue can be (0, 360), any high values will reset back to 0
 */
u32 hueToRgb(double hue) {
    hsv hsvColor = {
        .h = remainder(hue, 360.0),
        .s = 1.0,
        .v = 1.0
    };
    
    rgb rgbColor = hsv2rgb(hsvColor);
    
    u32 color =
        ((u32)(rgbColor.r * 0xff) << 16) +
        ((u32)(rgbColor.g * 0xff) << 8) +
        ((u32)(rgbColor.b * 0xff));
    
    return color;
}

/**
 * Prettifies the image
 * It will render each pixel hue based on the number of POIs at that latlong
 */
void prettifyImage(Image image) {
    //u32 densityCap = findMaxPixel(image);
    u32 densityCap = 10000;

    for (int i = 0; i < image.width * image.height; i++) {
        u32 count = image.data[i];

        // Let missing data stay black
        if (count == 0) continue;
        if (count > densityCap) count = densityCap;

        // (0,1) bounds
        double colorX = (double)count / densityCap;
        // Semicircle with origin (1,0)
        // Gives a nicer color distribution for rural areas
        // (0,360) bounds
        double colorY = sqrt(1.0 - pow(1.0 - colorX, 2.0));

        // Origin for the hue to begin
        double hueOrigin = 120.0 / 360.0;

        // Range modifier, to select only a portion of the hue wheel
        double hueRange  = 240.0 / 360.0;
        
        double hue = (hueOrigin + hueRange * colorY) * 360.0;

        image.data[i] = hueToRgb(hue);
    }
}

int main() {
    // Dimensions of generated image

    // const u16 width = 1960;
    // const u16 height = 1080;

    // 2K res
    const u16 width = 2560;
    const u16 height = 1440;

    // 4K res
    // const u16 width = 2560;
    // const u16 height = 1440;

    // The geocode file. It should be delimited by newlines
    const char *geocodePath = "points.txt"; 

    Image image = loadGeocodesAsImage(geocodePath, width, height);
    prettifyImage(image);

    saveAsPPM(image, "out.ppm");
}
