#include <SFML/Graphics.hpp>
#include "DrawContext.hpp"
#include "Tile.hpp"

int main() {
    const int SIZE = 800;
    const int WIDTH = SIZE;
    const int HEIGHT = SIZE;

    WindowPtr window(new sf::RenderWindow(sf::VideoMode(WIDTH, HEIGHT), "multigrid"));

    sf::View view;
    // Initialize the view to a rectangle located at :
    view.reset(sf::FloatRect(-WIDTH/2, -HEIGHT/2, WIDTH, HEIGHT));
    //view.reset(sf::FloatRect(0, 0, WIDTH, HEIGHT));
    // Apply it
    window->setView(view);

    DrawContext context(window);
    context.setCurrentX(-70);

    // TODO: Load & parse map file

    TilePtr root = Tile::createTile(6);
    root->place(sf::Vector2f(0, 0));
    // Place a tile such that the principle bisectors point to each other
    root->setNeighbor(0, Tile::createTile(4), 0);
    /*
    root->setNeighbor(5, Tile::createTile(8));
    root->setNeighbor(3, Tile::createTile(4));
    root->setNeighbor(1, Tile::createTile(6));
    root->setNeighbor(2, Tile::createTile(6));
    root->getNeighbor(5)->setNeighbor(1, Tile::createTile(4));
    */

    while (window->isOpen()) {
        sf::Event event;
        while (window->pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                window->close();
            } else if (sf::Event::KeyPressed && event.key.code == sf::Keyboard::Escape) {
                window->close();
            }
        }

        window->clear();
        Tile::drawAll(root, context);
        window->display();
    }

    root->destroy();

    return 0;
}
