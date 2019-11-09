#include <SFML/Graphics.hpp>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <queue>
#include <set>

using namespace std;

#include "Tile.hpp"
#include "VoidTile.hpp"

static const double SIDE_LEN = 80;

// Taken from https://en.sfml-dev.org/forums/index.php?topic=7313.0
sf::Color hsv(int hue, float sat, float val) {
    hue %= 360;
    while(hue<0) hue += 360;

    if(sat<0.f) sat = 0.f;
    if(sat>1.f) sat = 1.f;

    if(val<0.f) val = 0.f;
    if(val>1.f) val = 1.f;

    int h = hue/60;
    float f = float(hue)/60-h;
    float p = val*(1.f-sat);
    float q = val*(1.f-sat*f);
    float t = val*(1.f-sat*(1-f));

    switch(h) {
    default:
    case 0:
    case 6: return sf::Color(val*255, t*255, p*255);
    case 1: return sf::Color(q*255, val*255, p*255);
    case 2: return sf::Color(p*255, val*255, t*255);
    case 3: return sf::Color(p*255, q*255, val*255);
    case 4: return sf::Color(t*255, p*255, val*255);
    case 5: return sf::Color(val*255, p*255, q*255);
    }
}

TilePtr Tile::createTile(unsigned sideCount) {
    return TilePtr(new Tile(sideCount));
}

Tile::Tile() {
}

Tile::Tile(unsigned sideCount) {
    // Restrict to multiples of 2, and at least 4 neighbors
    if (sideCount % 2 != 0 || sideCount <= 2) {
        fprintf(stderr, "Invalid side count: %d\n", sideCount);
        exit(1);
    }

    this->sideCount = sideCount;
    this->origin = sf::Vector2f(0, 0);
    this->principleBisectorAngle = -20;
    this->hasPlaced = false;

    precomputeGeomtry();

    for (unsigned i = 0; i < sideCount; i++) {
        neighbors.push_back(TilePtr(new VoidTile()));
    }
}

void Tile::precomputeGeomtry() {
    externalAngle = 2.0 * M_PI / ((double)sideCount);
    circumradius = SIDE_LEN / (2.0 * sin(M_PI / ((double)sideCount)));
    bisectorLength = circumradius * cos(externalAngle / 2.0);
}

bool Tile::containsNeighbor(TilePtr neighbor) {
    return std::find(neighbors.begin(), neighbors.end(), neighbor) != neighbors.end();
}

void Tile::place(sf::Vector2f origin) {
    if (hasPlaced) {
        fprintf(stderr, "Cannot reassign tile origin!\n");
        exit(1);
    }

    this->origin = origin;
    this->hasPlaced = true;
}


double Tile::edgeBisectorAngle(unsigned edgeNum) {
    const double externalAngle = 2.0 * M_PI / ((double)sideCount);
    return principleBisectorAngle + edgeNum * externalAngle;
}

TilePtr Tile::getNeighbor(unsigned num) {
    // Restrict to multiples of 2, and non-zero
    if (num >= sideCount) {
        fprintf(stderr, "Invalid side number: %d\n", num);
        exit(1);
    }

    return neighbors[num];
}

void Tile::setNeighbor(unsigned num, TilePtr tile, unsigned neighborEdge) {
    // Restrict to multiples of 2, and non-zero
    if (num >= sideCount) {
        fprintf(stderr, "Invalid side number: %d\n", num);
        exit(1);
    }

    if (neighbors[num].get() != nullptr) {
        fprintf(stderr, "Neighbor already set at %d\n", num);
        exit(1);
    }

    neighbors[num] = tile;
    // TODO: Set proper side index, depending on if there is a principle angle yet
    tile->neighbors[0] = TilePtr(this);
}

void Tile::drawAll(TilePtr tile, DrawContext &context) {
    set<Tile*> rendered;
    queue<TilePtr> tileDrawQueue;
    tileDrawQueue.push(tile);

    // Use BFS to draw
    while (!tileDrawQueue.empty()) {
        TilePtr nextTile = tileDrawQueue.front();
        tileDrawQueue.pop();

        // Already rendered
        if (rendered.count(nextTile.get()) != 0) {
            continue;
        }

        nextTile->draw(context);
        rendered.insert(nextTile.get());

        for (TilePtr neighbor : nextTile->neighbors) {
            tileDrawQueue.push(neighbor);
        }
    }
}

void Tile::draw(DrawContext &context) {
    // Creates a regular polygon with `sideCount` corners
    sf::CircleShape polygon(circumradius, sideCount);
    polygon.rotate(principleBisectorAngle);

    const double x = origin.x + SIDE_LEN;
    const double y = origin.y + SIDE_LEN;
    polygon.setOrigin(x, y);

    // Set fill color based on principle bisector, for aesthetics
    polygon.setFillColor(hsv((180 / M_PI) * principleBisectorAngle, 1, 1));
    polygon.setOutlineColor(sf::Color::White);
    polygon.setOutlineThickness(2);
    context.getWindow()->draw(polygon);
    
    drawBisectorAnnotation(context);
    drawOriginAnnotation(context);
}

void Tile::drawOriginAnnotation(DrawContext &context) {
    const double originPointSize = 3;
    sf::CircleShape originPointShape(originPointSize);
    originPointShape.setFillColor(sf::Color::White);
    originPointShape.setOrigin(originPointSize - origin.x,
                               originPointSize - origin.y);
    context.getWindow()->draw(originPointShape);
}

void Tile::drawBisectorAnnotation(DrawContext &context) {
    const double lineWidth = 2;
    sf::RectangleShape line(sf::Vector2f(bisectorLength, lineWidth));
    line.rotate(principleBisectorAngle);

    context.getWindow()->draw(line);
}


void Tile::destroy() {
    vector<TilePtr> sideCopy = neighbors;
    neighbors.clear();

    for (TilePtr tile : sideCopy) {
        if (tile.get() != nullptr && !tile->isVoid()) {
            tile->destroy();
        }
    }
}
