#include <SFML/Graphics.hpp>
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
    static int SIDE_LEN = 10;

    // TODO: Draw based on the number of sides, then iterate through the tile graph

    sf::CircleShape shape(SIDE_LEN);
    shape.setFillColor(sf::Color::Green);
    context.getWindow()->draw(shape);
}
