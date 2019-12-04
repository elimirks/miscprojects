#include <SFML/Graphics.hpp>

#include "Quaternion.hpp"

#include <cstdio>

const int WIDTH = 400;
const int HEIGHT = 400;

// Draw the real part as the vertical coordinate
// The other components will be RGB values

void updateCanvas(sf::Image &canvas) {
    Quaternion entry(-1, 0, 0, 0.5);
    entry = entry.versor();

    for (int i = 0; i < WIDTH; i++) {
        Quaternion toAdd = Quaternion(0, 0.02, 0, 0);
        entry = entry + toAdd;
        entry = entry.versor();

        Quaternion versor = entry.versor();

        int y = HEIGHT * (0.5 + versor.r() / 2);
        int r = 255 * (versor.i() + 1) / 2;
        int g = 255 * (versor.j() + 1) / 2;
        int b = 255 * (versor.k() + 1) / 2;

        canvas.setPixel(i, y, sf::Color(r, g, b));
    }

    printf("%f, %f, %f, %f\n", entry.r(), entry.i(), entry.j(), entry.k());
}

int main() {
    sf::RenderWindow window(sf::VideoMode(WIDTH, HEIGHT), "multigrid");

    sf::Image canvas;
    canvas.create(WIDTH, HEIGHT, sf::Color::Black);

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                window.close();
            } else if (sf::Event::KeyPressed && event.key.code == sf::Keyboard::Escape) {
                window.close();
            }
        }

        updateCanvas(canvas);

        sf::Texture texture;
        texture.loadFromImage(canvas);

        sf::Sprite sprite;
        sprite.setTexture(texture, true);

        window.clear();
        window.draw(sprite);
        window.display();
    }
}
