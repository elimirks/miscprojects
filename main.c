#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ISPOWEROF2(x) (x && !(x & (x - 1)))

enum { NORMAL = 0, START, END };
struct node {
    int type;
    int child_count;
    struct node *children;
};

struct grid {
    int width, height;
    char **map;
};

struct grid *readgrid(char *filename) {
    FILE *fp = fopen(filename, "r");
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    if (fp == NULL) {
        perror(filename);
        exit(1);
    }

    struct grid *g = malloc(sizeof(struct grid));
    g->width = -1;
    g->height = 0;

    while ((read = getline(&line, &len, fp)) != -1) {
        g->height++;

        if (g->width == -1) {
            g->width = read - 1; // -1 to ignore the newline
        } else if (g->width != read - 1) {
            fprintf(stderr, "Inconsistent maze width: %d, expected %d\n",
                    read - 1, g->width);
            exit(1);
        }

        // Don't realloc too often
        if (ISPOWEROF2(g->height)) {
            g->map = realloc(g->map, sizeof(char *) * g->height);
        }
        g->map[g->height - 1] = malloc(g->width);
        strncpy(g->map[g->height - 1], line, g->width);
    }

    fclose(fp);
    return g;
}

void printgrid(struct grid *g) {
    for (int y = 0; y < g->height; y++) {
        for (int x = 0; x < g->width; x++) {
            printf("%c", g->map[y][x]);
        }
        printf("\n");
    }
}

int main(int argc, char **argv) {
    struct grid *g = readgrid(argv[1]);
    printgrid(g);
    return 0;
}

