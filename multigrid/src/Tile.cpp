#include <SFML/Graphics.hpp>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <queue>
#include <set>

using namespace std;

#include "Tile.hpp"
#include "VoidTile.hpp"

static queue<TilePtr> tileDrawQueue;
static queue<DrawContext> contextQueue;

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
    // Restrict to multiples of 2, and at least 4 sides
    if (sideCount % 2 != 0 || sideCount <= 2) {
        fprintf(stderr, "Invalid side count: %d\n", sideCount);
        exit(1);
    }

    this->sideCount = sideCount;

    for (unsigned i = 0; i < sideCount; i++) {
        sides.push_back(TilePtr(new VoidTile()));
    }
}

TilePtr Tile::getNeighbor(unsigned num) {
    // Restrict to multiples of 2, and non-zero
    if (num >= sideCount) {
        fprintf(stderr, "Invalid side number: %d\n", num);
        exit(1);
    }

    return sides[num];
}

void Tile::setNeighbor(unsigned num, TilePtr tile) {
    // Restrict to multiples of 2, and non-zero
    if (num >= sideCount) {
        fprintf(stderr, "Invalid side number: %d\n", num);
        exit(1);
    }

    sides[num] = tile;
    // TODO: Set proper side index, depending on if there is a principle angle yet
    tile->sides[0] = TilePtr(this);
}

void Tile::drawAll(TilePtr tile, DrawContext &context) {
    set<Tile*> rendered;

    tileDrawQueue.push(tile);
    contextQueue.push(context);

    while (!tileDrawQueue.empty()) {
        TilePtr nextTile = tileDrawQueue.front();
        DrawContext nextContext = contextQueue.front();
        tileDrawQueue.pop();
        contextQueue.pop();

        if (rendered.count(nextTile.get()) == 0) {
            nextTile->draw(nextContext);
            rendered.insert(nextTile.get());
        }
    }
}

void Tile::draw(DrawContext &context) {
    static const double SIDE_LEN = 80;
    const double CIRCUMRADIUS = SIDE_LEN / (2.0 * sin(M_PI / ((double)sideCount)));

    // Draw an appropriate regular polygon

    sf::ConvexShape polygon;
    polygon.setPointCount(sideCount);

    const double originPerpBisector = context.getPerpBisector();
    const double directionMultiplier = context.getDirectionMultiplier();

    const double originBisectX = CIRCUMRADIUS * cos(originPerpBisector);
    const double originBisectY = CIRCUMRADIUS * sin(originPerpBisector);
    
    const double xOrigin = context.getCurrentX() + originBisectX;
    const double yOrigin = context.getCurrentY() + originBisectY;

    const double externalAngle = 2.0 * M_PI / ((double)sideCount);

    for (unsigned i = 0; i < sideCount; i++) {
        const double currentAngle = originPerpBisector + ((double)i + 0.5)
            * externalAngle * directionMultiplier;
        
        const double x = xOrigin + CIRCUMRADIUS * cos(currentAngle);
        const double y = yOrigin + CIRCUMRADIUS * sin(currentAngle);
        polygon.setPoint(i, sf::Vector2f(x, y));

        const TilePtr neighbor = sides[i];
        if (!neighbor->isVoid()) {
            const double currentPerpBisector = originPerpBisector
                + ((double)i) * externalAngle * directionMultiplier;
            const double bisectorLength = CIRCUMRADIUS * pow(cos(externalAngle / 2.0), 2);
            const double bisectX = xOrigin + bisectorLength * cos(currentPerpBisector);
            const double bisectY = yOrigin + bisectorLength * sin(currentPerpBisector);
            DrawContext newContext = context.mirroredContextForPosition(currentPerpBisector,
                                                                        bisectX, bisectY);

            tileDrawQueue.push(neighbor);
            contextQueue.push(newContext);
        }
    }

    polygon.setFillColor(hsv((180 / M_PI) * originPerpBisector, 1, 1));
    polygon.setOutlineColor(sf::Color::White);
    polygon.setOutlineThickness(2);

    context.getWindow()->draw(polygon);

    sf::CircleShape originPointShape(3);
    originPointShape.setFillColor(sf::Color::White);
    originPointShape.setOrigin(3 - xOrigin, 3 - yOrigin);
    context.getWindow()->draw(originPointShape);

    printf("%f, %f\n", xOrigin, yOrigin);
}

void Tile::destroy() {
    vector<TilePtr> sideCopy = sides;
    sides.clear();

    for (TilePtr tile : sideCopy) {
        if (tile.get() != nullptr && !tile->isVoid()) {
            tile->destroy();
        }
    }
}
