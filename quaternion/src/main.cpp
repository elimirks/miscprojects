#include <SFML/Graphics.hpp>

#include "Quaternion.hpp"

#include <cstdio>

const int WIDTH = 400;
const int HEIGHT = 400;


// Sample sequences

auto addSequence = [] (int index, time_t) -> Quaternion {
    Quaternion initial = -1 - 1_k;
    return initial + index * 0.02_i;
};

auto rotateSequence = [] (int index, time_t) -> Quaternion {
    Quaternion initial = -1 + 0.5_k;
    Quaternion rotation = (100 + 3_i).versor();
    return initial * pow(rotation, index);
};


// Draw the real part as the vertical coordinate
// The other components will be RGB values

void updateCanvas(sf::Image &canvas, Quaternion (*function)(int, time_t)) {
    for (int i = 0; i < WIDTH; i++) {
        // FIXME: Pass in the time!
        Quaternion entry = function(i, 0);
        Quaternion versor = entry.versor();

        int y = HEIGHT * (0.5 + versor.r() / 2.0);
        int r = 255 * (versor.i() + 1) / 2;
        int g = 255 * (versor.j() + 1) / 2;
        int b = 255 * (versor.k() + 1) / 2;

        for (int j = y; j < HEIGHT; j++) {
            canvas.setPixel(i, j, sf::Color(r, g, b));
        }
    }
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

        updateCanvas(canvas, rotateSequence);

        sf::Texture texture;
        texture.loadFromImage(canvas);

        sf::Sprite sprite;
        sprite.setTexture(texture, true);

        window.clear();
        window.draw(sprite);
        window.display();
    }
}
