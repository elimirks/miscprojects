#include <SFML/Graphics.hpp>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <queue>
#include <set>

using namespace std;

#include "Tile.hpp"

static const double SIDE_LEN = 50;

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
    TilePtr tile = TilePtr(new Tile(sideCount));
    tile->thisPointer = tile;
    return tile;
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
    this->principleBisectorAngle = 0;
    this->hasPlaced = false;

    precomputeGeomtry();

    for (unsigned i = 0; i < sideCount; i++) {
        neighbors.push_back(TilePtr(nullptr));
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

    if (!tile->hasPlaced) {
        const double bisectorAngle = edgeBisectorAngle(num);
        const double distance = bisectorLength + tile->bisectorLength;
        const double neighborOriginX = origin.x + distance * cos(bisectorAngle);
        const double neighborOriginY = origin.y + distance * sin(bisectorAngle);
        const double mirroredBisectorAngle = bisectorAngle + M_PI;
        tile->origin = sf::Vector2f(neighborOriginX, neighborOriginY);

        const double neighborPrincipleBisectorAngle = mirroredBisectorAngle -
            (double)neighborEdge * tile->externalAngle;
        tile->principleBisectorAngle = neighborPrincipleBisectorAngle;

        tile->hasPlaced = true;
    }

    neighbors[num] = tile;
    tile->neighbors[neighborEdge] = thisPointer;
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
            if (neighbor.get() != nullptr) {
                tileDrawQueue.push(neighbor);
            }
        }
    }
}

void Tile::draw(DrawContext &context) {
    sf::ConvexShape polygon;
    polygon.setPointCount(sideCount);

    for (unsigned i = 0; i < sideCount; i++) {
        const double currentAngle = principleBisectorAngle +
            ((double)i + 0.5) * externalAngle;
        
        const double x = circumradius * cos(currentAngle);
        const double y = circumradius * sin(currentAngle);
        polygon.setPoint(i, sf::Vector2f(origin.x + x, origin.y + y));
    }

    // Set fill color based on principle bisector, for aesthetics
    polygon.setFillColor(hsv((180 / M_PI) * principleBisectorAngle, 1, 1));
    polygon.setOutlineColor(sf::Color::White);
    polygon.setOutlineThickness(2);
    context.getWindow()->draw(polygon);
    
    drawBisectorAnnotation(context);
    drawOriginAnnotation(context);
}

void Tile::drawOriginAnnotation(DrawContext &context) {
    const double radius = 3;
    sf::CircleShape originPointShape(radius);
    originPointShape.setFillColor(sf::Color::White);
    originPointShape.setPosition(origin.x - radius, origin.y - radius);
    context.getWindow()->draw(originPointShape);
}

void Tile::drawBisectorAnnotation(DrawContext &context) {
    const double bisectX = origin.x + bisectorLength * cos(principleBisectorAngle);
    const double bisectY = origin.y + bisectorLength * sin(principleBisectorAngle);

    sf::Vertex line[] = {
        sf::Vertex(origin),
        sf::Vertex(sf::Vector2f(bisectX, bisectY))
    };

    context.getWindow()->draw(line, 2, sf::Lines);
}

void Tile::destroy() {
    vector<TilePtr> sideCopy = neighbors;
    neighbors.clear();

    for (TilePtr tile : sideCopy) {
        if (tile.get() != nullptr) {
            tile->destroy();
        }
    }
    thisPointer = TilePtr(nullptr);
}
