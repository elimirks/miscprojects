#ifndef __TILE_HPP__
#define __TILE_HPP__
#include "Tile.hpp"
#include "DrawContext.hpp"
#include <vector>
#include <memory>

using namespace std;

class Tile;
typedef shared_ptr<Tile> TilePtr;

class Tile {
private:
    unsigned short sideCount;
    vector<TilePtr> leftSides, rightSides;
protected:
    Tile();
    Tile(unsigned sideCount);
public:
    virtual bool isVoid() {
        return false;
    }

    virtual TilePtr getNeighbor(unsigned num);
    virtual void setNeighbor(unsigned num, TilePtr tile);

    void draw(DrawContext &context);

    static TilePtr createTile(unsigned sideCount);
};
#endif
