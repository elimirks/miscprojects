#ifndef __TILE_HPP__
#define __TILE_HPP__
#include "Tile.hpp"
#include "DrawContext.hpp"
#include <vector>
#include <queue>
#include <memory>

using namespace std;

class Tile;
typedef shared_ptr<Tile> TilePtr;

class Tile {
private:
    // FIXME: Remove this after creating a static addEdge method
    TilePtr thisPointer;

    // Precomputed geometrical information
    // These should be the same for each category of objects - e.g. squares

    // External angle of the polygon
    double externalAngle;
    // Circumradius of the polygon
    double circumradius;
    // Length between origin and any bisection intersection
    double bisectorLength;

    bool hasPlaced;
    sf::Vector2f origin;
    double principleBisectorAngle;
    unsigned short sideCount;
    vector<TilePtr> neighbors;

    // Helper methods for drawing debug annotations
    void drawOriginAnnotation(DrawContext &context);
    void drawBisectorAnnotation(DrawContext &context);

    bool containsNeighbor(TilePtr neighbor);
    double edgeBisectorAngle(unsigned edgeNum);

    void precomputeGeomtry();
protected:
    Tile();
    Tile(unsigned sideCount);
public:
    virtual bool isVoid() {
        return false;
    }

    virtual TilePtr getNeighbor(unsigned num);
    virtual void setNeighbor(unsigned num, TilePtr tile, unsigned neighborEdge);

    virtual void draw(DrawContext &context);

    /**
     * Places the tile at the given origin.
     * Must be used when no other tiles are connected.
     */
    void place(sf::Vector2f origin);

    /**
     * Destroys the graph of connected tiles
     */
    void destroy();

    static TilePtr createTile(unsigned sideCount);
    static void drawAll(TilePtr tile, DrawContext &context);
};
#endif
