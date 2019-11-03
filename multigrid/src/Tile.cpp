#include <SFML/Graphics.hpp>
#include <cmath>
#include <cstdio>
#include <cstdlib>

#include "Tile.hpp"
#include "VoidTile.hpp"

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

    for (unsigned i = 0; i < sideCount / 2; i++) {
        leftSides.push_back(shared_ptr<Tile>(new VoidTile()));
        rightSides.push_back(shared_ptr<Tile>(new VoidTile()));
    }
}

TilePtr Tile::getNeighbor(unsigned num) {
    // Restrict to multiples of 2, and non-zero
    if (num % 2 != 0 || num == 0) {
        fprintf(stderr, "Invalid side number: %d\n", num);
        exit(1);
    }

    TilePtr ptr;

    // Left side
    if (num < sideCount / 2) {
        return leftSides[num];
    } else {
        return rightSides[num / 2];
    }
}

void Tile::setNeighbor(unsigned num, TilePtr tile) {
    // Restrict to multiples of 2, and non-zero
    if (num % 2 != 0 || num == 0) {
        fprintf(stderr, "Invalid side number: %d\n", num);
        exit(1);
    }

    // Left side
    if (num < sideCount / 2) {
        leftSides[num] = tile;
    } else {
        rightSides[num / 2] = tile;
    }
}

void Tile::draw(DrawContext &context) {
    static const double SIDE_LEN = 40;
    const double CIRCUMRADIUS = SIDE_LEN / (2.0 * sin(M_PI / ((double)sideCount)));

    // Draw an appropriate regular polygon

    sf::ConvexShape polygon;
    polygon.setPointCount(sideCount);

    // External Angle
    const double edgeAngle = 2.0 * M_PI / ((double)sideCount);

    const double xOffset = 100.0;
    const double yOffset = 100.0;

    for (unsigned i = 0; i < sideCount; i++) {
        const double currentAngle = i * edgeAngle;
        const double x = xOffset + CIRCUMRADIUS * cos(currentAngle);
        const double y = yOffset + CIRCUMRADIUS * sin(currentAngle);
        polygon.setPoint(i, sf::Vector2f(x, y));
    }

    polygon.setFillColor(sf::Color::Green);
    polygon.setOutlineColor(sf::Color::White);
    polygon.setOutlineThickness(2);

    context.getWindow()->draw(polygon);
}

void Tile::destroy() {
    for (TilePtr tile : leftSides) {
        if (!tile->isVoid()) {
            tile->destroy();
        }
    }
    for (TilePtr tile : rightSides) {
        if (!tile->isVoid()) {
            tile->destroy();
        }
    }

    leftSides.clear();
    rightSides.clear();
}
