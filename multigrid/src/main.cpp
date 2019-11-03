#include <SFML/Graphics.hpp>
#include "DrawContext.hpp"
#include "Tile.hpp"

int main()
{
    WindowPtr window(new sf::RenderWindow(sf::VideoMode(200, 200), "SFML works!"));
    DrawContext context(window);

    TilePtr root = Tile::createTile(8);

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

        root->draw(context);

        window->display();
    }

    root->destroy();

    return 0;
}
