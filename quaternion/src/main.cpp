#include <SFML/Graphics.hpp>

#include "Quaternion.hpp"

#include <cstdio>

const int WIDTH = 400;
const int HEIGHT = 400;


// Sample sequences

auto addSequence = [] (int index, double t) -> Quaternion {
    Quaternion initial = -1 - 1_k;
    return initial + index * 0.02_i;
};

auto rotateSequence = [] (int index, double t) -> Quaternion {
    Quaternion initial = -1 + 0.5_k;
    Quaternion rotation = (100 + 3_i).versor();
    return initial * pow(rotation, index);
};

auto timeAddSequence = [] (int index, double t) -> Quaternion {
    Quaternion initial = sin(t) + 1_k * sin(2 * t);
    return initial + index * 0.02_i;
};

auto timeRotateSequence = [] (int index, double t) -> Quaternion {
    Quaternion initial = cos(t) + 0.5_j * sin(t) + 0.5_k * cos(t);
    Quaternion rotation = (100 + 3_i).versor();
    return initial * pow(rotation, index);
};



// Draw the real part as the vertical coordinate
// The other components will be RGB values

void updateCanvas(sf::Image &canvas, sf::Time time, Quaternion (*function)(int, double)) {
    for (int i = 0; i < WIDTH; i++) {
        // FIXME: Pass in the time!
        Quaternion entry = function(i, time.asSeconds());
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

    sf::Clock clock;

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                window.close();
            } else if (sf::Event::KeyPressed && event.key.code == sf::Keyboard::Escape) {
                window.close();
            }
        }

        sf::Image canvas;
        canvas.create(WIDTH, HEIGHT, sf::Color::Black);

        updateCanvas(canvas, clock.getElapsedTime(), timeAddSequence);

        sf::Texture texture;
        texture.loadFromImage(canvas);

        sf::Sprite sprite;
        sprite.setTexture(texture, true);

        window.clear();
        window.draw(sprite);
        window.display();
    }
}
