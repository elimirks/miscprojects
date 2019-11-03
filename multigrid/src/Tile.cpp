#include <SFML/Graphics.hpp>
#include <cmath>
#include <cstdio>
#include <cstdlib>

#include "Tile.hpp"
#include "VoidTile.hpp"

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
    // Restrict to multiples of 2, and non-zero
    if (sideCount % 2 != 0 || sideCount == 0) {
        fprintf(stderr, "Invalid side count: %d\n", sideCount);
        exit(1);
    }

    this->sideCount = sideCount;

    for (unsigned i = 0; i < sideCount; i++) {
        sides.push_back(shared_ptr<Tile>(new VoidTile()));
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

    // TODO: Make cyclic, once the draw method supports it
    sides[num] = tile;
}

void Tile::draw(DrawContext &context) {
    static const double SIDE_LEN = 40;
    const double CIRCUMRADIUS = SIDE_LEN / (2.0 * sin(M_PI / ((double)sideCount)));

    // Draw an appropriate regular polygon

    sf::ConvexShape polygon;
    polygon.setPointCount(sideCount);

    const double angleOffset = context.getCurrentAngle();
    const double xOffset = context.getCurrentX();
    const double yOffset = context.getCurrentY();
    const double direction = context.getDirection();

    // External Angle
    const double edgeAngle = 2.0 * M_PI / ((double)sideCount);

    for (unsigned i = 0; i < sideCount; i++) {
        const double currentAngle = angleOffset + i * edgeAngle * direction;
        const double x = xOffset + CIRCUMRADIUS * cos(currentAngle);
        const double y = yOffset + CIRCUMRADIUS * sin(currentAngle);
        polygon.setPoint(i, sf::Vector2f(x, y));

        TilePtr neighbor = sides[i];
        if (!neighbor->isVoid()) {
            DrawContext newContext = context.mirroredContextForPosition(currentAngle, x, y);
            neighbor->draw(newContext);
        }
    }

    polygon.setFillColor(hsv((180 / M_PI) * angleOffset, 1, 1));
    polygon.setOutlineColor(sf::Color::White);
    polygon.setOutlineThickness(2);

    context.getWindow()->draw(polygon);
}

void Tile::destroy() {
    for (TilePtr tile : sides) {
        if (!tile->isVoid()) {
            tile->destroy();
        }
    }
    sides.clear();
}
