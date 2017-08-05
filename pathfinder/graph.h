#pragma once
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

enum {
    START = 'S',
    END = 'E',
    PATH = ' ',
    WALL = '.'
};
struct node {
    int x, y;
    int type;
    int child_count;
    // We have at most 4 children
    struct node *children[4];
    // Distances to the children (since we prune)
    int distances[4];
};
struct graph {
    struct node *start;
    struct node *end;
};
struct grid {
    int width, height;
    char **map;
};

void prune_graph(struct grid *g, struct graph *graph);
struct graph *create_graph(struct grid *g);
struct grid *read_grid(char *filename);
void print_grid(struct grid *g);
void annotate_grid(struct grid *g, struct graph *graph);
