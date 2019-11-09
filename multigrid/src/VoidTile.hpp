#ifndef __VOID_TILE_HPP__
#define __VOID_TILE_HPP__
#include "Tile.hpp"

class VoidTile : public Tile {
protected:
public:
    VoidTile();
    TilePtr getNeighbor(unsigned num);
    void setNeighbor(unsigned num, TilePtr tile, unsigned neighborEdge);

    bool isVoid() {
        return true;
    }
};
#endif
