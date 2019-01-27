#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eb.h"

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
        struct PPMImage raw = PPMImage_load(from_file);
        /*
        struct EBImage eb = EBImage_compress_ppm(&raw);
        EBImage_save(&eb, to_file);
        EBImage_dealloc(&eb);
        */

        uint32_t len = ((uint32_t)raw.width) * ((uint32_t)raw.height);

        struct PPMImage delta;
        delta.width  = raw.width;
        delta.height = raw.height;
        delta.data   = malloc(sizeof(uint32_t) * len);

        uint8_t prev_r = 0, prev_g = 0, prev_b = 0;
        for (uint32_t i = 0; i < len; i++) {
            uint8_t raw_r = ((uint8_t*)&raw.data[i])[0];
            uint8_t raw_g = ((uint8_t*)&raw.data[i])[1];
            uint8_t raw_b = ((uint8_t*)&raw.data[i])[2];

            uint8_t *d = (uint8_t*)&delta.data[i];
            d[0] = raw_r - prev_r;
            d[1] = raw_g - prev_g;
            d[2] = raw_b - prev_b;

            prev_r = raw_r;
            prev_g = raw_g;
            prev_b = raw_b;
        }

        PPMImage_save(&delta, "delta.ppm");

        PPMImage_dealloc(&delta);
        PPMImage_dealloc(&raw);
    } else {
        struct EBImage eb = EBImage_load(from_file);
        struct PPMImage raw = EBImage_to_ppm(&eb);
        PPMImage_save(&raw, to_file);
        EBImage_dealloc(&eb);
        PPMImage_dealloc(&raw);
    }
}
