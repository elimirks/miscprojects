#include <SFML/Graphics.hpp>

#include "Quaternion.hpp"

#include <cstdio>

const int WIDTH = 2560;
const int HEIGHT = 1440;

// Sample sequences

auto timeSpikeSequence = [] (int index, double t) -> Quaternion {
    printf("");
    //return (1 + (WIDTH/2 - index) * 0.005_j * sin(t));
    return (cos(t) + sin(t) * (WIDTH/2 - index) * 0.005_j);
};

auto timeColorSequence = [] (int index, double t) -> Quaternion {
    printf("");
    Quaternion initial = 1 + sin(t / 3) * 1_i + sin(t / 5) * 1_j + sin(t / 7) * 1_k;
    return initial;
};

auto timeRotateSequence = [] (int index, double t) -> Quaternion {
    printf("");
    Quaternion base = sin(t) * 0.5 + (sin(t/3) * 1_i + cos(t/3) * 1_j);
    Quaternion rotation = (30 + sin(t / 4) * 1_k).versor();
    return base * pow(rotation, WIDTH / 2 - index);
};

auto timeCombinatorSequence = [] (int index, double t) -> Quaternion {
    printf("");

    // Combine with rotates components of quaternion values, time dependent
    /*
    Quaternion first = (pow((1_i - 1_j).versor(), 1 + sin(t / 13)) - 1).versor();
    Quaternion second = (pow((1_j - 1_k).versor(), 1 + sin(t / 19)) - 1).versor();
    Quaternion third =(pow((1_k - 1_i).versor(), 1 + sin(t / 11)) - 1).versor();
    */
    /*
    Quaternion first = (pow(1_i, t / 23) - 1) / 2;
    Quaternion second = (pow(1_j, t / 29) - 1) / 2;
    Quaternion third = (pow(1_k, t / 31) - 1) / 2;
    */

    //return first * timeColorSequence(index, t) + second * timeRotateSequence(index, t) + third * timeSpikeSequence(index, t);

    //return timeColorSequence(index, t).versor() + timeRotateSequence(index, t) + timeSpikeSequence(index, t);
    return (timeSpikeSequence(index, t / 17) + timeRotateSequence(index, t / 13)) *
        pow((timeColorSequence(index, t / 11)).versor(), t / 19);
};


// Draw the real part as the vertical coordinate
// The other components will be RGB values

void updateCanvas(sf::Image &canvas, sf::Time time, Quaternion (*function)(int, double)) {
    for (int i = 0; i < WIDTH; i++) {
        Quaternion entry = function(i, time.asSeconds());
        Quaternion versor = entry.versor();

        int y = std::max(0, (int)(HEIGHT * (0.5 + versor.r() / 2.0)));
        int r = 255 * (versor.i() + 1) / 2;
        int g = 255 * (versor.j() + 1) / 2;
        int b = 255 * (versor.k() + 1) / 2;

        for (int j = y; j < HEIGHT; j++) {
            canvas.setPixel(i, j, sf::Color(r, g, b));
        }

        if (y < 0) {
            exit(1);
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

        updateCanvas(canvas, clock.getElapsedTime(), timeCombinatorSequence);

        sf::Texture texture;
        texture.loadFromImage(canvas);

        sf::Sprite sprite;
        sprite.setTexture(texture, true);

        window.clear();
        window.draw(sprite);
        window.display();
    }
}
