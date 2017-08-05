#include <SFML/Graphics.hpp>
#include <complex>
#include <math.h>

#include "colorManipulationTools.h"

//#define WIDTH 512
//#define HEIGHT 512
#define WIDTH 256
#define HEIGHT 256

#define PI 3.14159

using std::complex;

void successiveRelaxation(complex<double> *grid, int width, int height);
void updateImageFromGrid(sf::Image *img, complex<double> *grid, int width, int height);

int main() {
	complex<double> *grid = new complex<double>[WIDTH * HEIGHT];
	// Set one edge to a const
	int i;
	for (i = 0; i < WIDTH; i++) {
		double v = (double)i;
		v /= WIDTH;
		grid[i] = cos(1 * PI * v) + 1i * sin(1 * PI * v);
		grid[WIDTH * (HEIGHT) - i] = -1.0;
	}
	for (i = 0; i < HEIGHT; i++) {
		double v = (double)i;
		v /= WIDTH;
		grid[(HEIGHT - i) * WIDTH] = +0.7 * (cos(1 * PI * v) + 1i * sin(1 * PI * v));
		//grid[i * WIDTH + WIDTH - 1] = -0.5;
	}

	// Rendering stuff

	sf::RenderWindow window(sf::VideoMode(2 * WIDTH, 2 * HEIGHT), "Laplace Equation Visualizer");

	sf::Image img;
	img.create(WIDTH, HEIGHT, sf::Color::Red);

	while (window.isOpen()) {
		sf::Event event;

		while (window.pollEvent(event)) {
			if (event.type == sf::Event::Closed)
				window.close();
		}

		// Iterate a bunch of times before rendering - speeds things up drastically
		for (int i = 0; i < 10; i++) {
			successiveRelaxation(grid, WIDTH, HEIGHT);
		}

		updateImageFromGrid(&img, grid, WIDTH, HEIGHT);

		sf::Texture texture;
		texture.loadFromImage(img);
		sf::Sprite sprite;
		sprite.setTexture(texture, true);
		sprite.move(WIDTH / 4, HEIGHT / 4);

		window.clear(sf::Color(20, 20, 20));
		window.draw(sprite);
		window.display();
	}

	delete[] grid;

	return 0;
}

// A (hopefully) faster algorithm than Gauss-Seidel
void successiveRelaxation(complex<double> *grid, int width, int height) {
	double s = 1.7;
	int x, y;
	for (y = 1; y < height - 1; y++) {
		for (x = 1; x < width - 1; x++) {
			grid[width * y + x] = grid[width * y + x] * (1.0-s) + (s / 4.0) * (
				grid[width * (y-1) + x] +
				grid[width * (y+1) + x] +
				grid[width * y + (x-1)] +
				grid[width * y + (x+1)]);
		}
	}
}

void updateImageFromGrid(sf::Image *img, complex<double> *grid, int width, int height) {
	int y, x;
  for (y = 0; y < HEIGHT; y++) {
		for (x = 0; x < WIDTH; x++) {
			// Cool visualizations of complex numbers
			HSV hsvCol;
			hsvCol.h = std::arg(grid[width * y + x]) * 180.0 / PI;
			hsvCol.s = 1.0;
			// Hack for now - magnitudes of 1 only work
			hsvCol.v = std::norm(grid[width * y + x]) > 1.0
				? 1.0
				: std::norm(grid[width * y + x]);

			RGB rgbCol = hsv2rgb(hsvCol);

      int red = (int)(rgbCol.r * 256);
      int green = (int)(rgbCol.g * 256);
      int blue = (int)(rgbCol.b * 256);

			img->setPixel(x, y, sf::Color(red, green, blue));
		}
	}
}

