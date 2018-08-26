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
        struct EBImage eb = EBImage_compress_ppm(&raw);
        EBImage_save(&eb, to_file);
        EBImage_dealloc(&eb);
        PPMImage_dealloc(&raw);
    } else {
        struct EBImage eb = EBImage_load(from_file);
        struct PPMImage raw = EBImage_to_ppm(&eb);
        PPMImage_save(&raw, to_file);
        EBImage_dealloc(&eb);
        PPMImage_dealloc(&raw);
    }
}
