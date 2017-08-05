#include "graph.h"

int main(int argc, char **argv) {
    struct grid *g = read_grid(argv[1]);
    printf("Input map:\n");
    print_grid(g);

    struct graph *graph = create_graph(g);
    printf("\nAnnotated map:\n");
    annotate_grid(g, graph);
    print_grid(g);
    return 0;
}

/*
 * Pathfinding algorithm ideas:
 * - DFS
 * - BFS
 * - A-Star
 * - Dijkstra
 * - ???
 */
