#include "VoidTile.hpp"

VoidTile::VoidTile() {
}

TilePtr VoidTile::getNeighbor(unsigned num) {
    fprintf(stderr, "Cannot get neighbors of a void tile!\n");
    exit(1);

    return TilePtr();
}

void VoidTile::setNeighbor(unsigned num, TilePtr tile, unsigned neighborEdge) {
    fprintf(stderr, "Cannot set neighbors of a void tile!\n");
    exit(1);
}
