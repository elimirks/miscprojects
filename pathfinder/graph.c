#include "graph.h"

#define ISPOWEROF2(x) (x && !(x & (x - 1)))

struct grid *read_grid(char *filename) {
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

    // FIXME this should realloc, not just malloc every time!
    g->map = malloc(sizeof(char *) * 128);

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
            //g->map = realloc(g->map, sizeof(char *) * g->height);
        }
        g->map[g->height - 1] = malloc(g->width);
        strncpy(g->map[g->height - 1], line, g->width);
    }

    fclose(fp);
    return g;
}

void print_grid(struct grid *g) {
    for (int y = 0; y < g->height; y++) {
        for (int x = 0; x < g->width; x++) {
            printf("%c", g->map[y][x]);
        }
        printf("\n");
    }
}

struct graph *create_graph(struct grid *g) {
    struct graph *graph = malloc(sizeof(struct graph));
    graph->start = NULL;
    graph->end = NULL;

    struct node *previous_line = NULL;

    for (int y = 0; y < g->height; y++) {
        struct node *line = malloc(sizeof(struct node) * g->width);
        for (int x = 0; x < g->width; x++) {
            line[x].x = x;
            line[x].y = y;
            line[x].child_count = 0;
            line[x].distances[0] = line[x].distances[1] =
                line[x].distances[2] = line[x].distances[3] = 1;

            line[x].type = g->map[y][x];
            switch (line[x].type) {
            case 'S':
                if (graph->start != NULL) {
                    fprintf(stderr, "Multiple start nodes are not allowed\n");
                    exit(1);
                }
                graph->start = &line[x];
                line[x].type = START;
                break;
            case 'E':
                if (graph->end != NULL) {
                    fprintf(stderr, "Multiple end nodes are not allowed\n");
                    exit(1);
                }
                graph->end = &line[x];
                line[x].type = END;
                break;
            case ' ':
                line[x].type = PATH;
                break;
            case '.':
                line[x].type = WALL;
                break;
            default:
                fprintf(stderr, "Unexpected character: %c\n", g->map[y][x]);
                exit(1);
                break;
            }

            // Connect this node to it's neighbors
            if (line[x].type != WALL) {
                // Connect the left node, if it isn't a wall
                if (x > 0 && line[x-1].type != WALL) {
                    line[x-1].children[line[x-1].child_count++] = &line[x];
                    line[x].children[line[x].child_count++] = &line[x-1];
                }

                // Connect the above node, if it isn't a wall
                if (y > 0 && previous_line[x].type != WALL) {
                    struct node *above = &previous_line[x];
                    above->children[above->child_count++] = &line[x];
                    line[x].children[line[x].child_count++] = above;
                }
            }
        }
        previous_line = line;
    }

    prune_graph(g, graph);
    return graph;
}

// Used to prune useless nodes
void prune_joining_node(struct node *first,
                        struct node *second,
                        struct node *joining) {
    for (int i = 0; i < first->child_count; i++) {
        if (first->children[i] == joining) {
            first->children[i] = second;
            first->distances[i]++;
            break;
        }
    }
    for (int i = 0; i < second->child_count; i++) {
        if (second->children[i] == joining) {
            second->children[i] = first;
            second->distances[i]++;
            break;
        }
    }
}

// Prunes middle nodes
// That is, if a node simply joins a single node to another, ignore it
void prune_graph(struct grid *g, struct graph *graph) {
    char **visited_map = malloc(sizeof(char *) * g->height);
    for (int y = 0; y < g->height; y++) {
        visited_map[y] = malloc(sizeof(char) * g->width);
        for (int x = 0; x < g->width; x++) {
            visited_map[y][x] = '0';
        }
    }

    int stack_size = 1;
    // Static 128 size stack for now... but this should really be dynamic
    struct node **stack = malloc(sizeof(struct node *) * 128);
    stack[0] = graph->start;

    while (stack_size > 0) {
        struct node *current = stack[--stack_size];
        if (visited_map[current->y][current->x] == '1') {
            continue;
        }
        visited_map[current->y][current->x] = '1';

        // Add the children to the stack
        for (int i = 0; i < current->child_count; i++) {
            stack[stack_size++] = current->children[i];
        }

        if (current->child_count == 2 && current->type == PATH) {
            prune_joining_node(current->children[0], current->children[1], current);
        }
    }

    for (int y = 0; y < g->height; y++) {
        free(visited_map[y]);
    }
    free(visited_map);
}

// Annotates the given grid with the nodes in the given graph
void annotate_grid(struct grid *g, struct graph *graph) {
    // This function essentially just DFSs through the given graph

    char **visited_map = malloc(sizeof(char *) * g->height);
    for (int y = 0; y < g->height; y++) {
        visited_map[y] = malloc(sizeof(char) * g->width);
        for (int x = 0; x < g->width; x++) {
            visited_map[y][x] = '0';
        }
    }

    int stack_size = 1;
    // Static 128 size stack for now... but this should really be dynamic
    struct node **stack = malloc(sizeof(struct node *) * 128);
    stack[0] = graph->start;

    while (stack_size > 0) {
        struct node *current = stack[--stack_size];
        if (visited_map[current->y][current->x] == '1') {
            continue;
        }
        visited_map[current->y][current->x] = '1';

        g->map[current->y][current->x] = 'x';

        // Add the children to the stack
        for (int i = 0; i < current->child_count; i++) {
            stack[stack_size++] = current->children[i];
        }
    }

    for (int y = 0; y < g->height; y++) {
        free(visited_map[y]);
    }
    free(visited_map);
}
