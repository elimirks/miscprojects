#ifndef __DRAW_CONTEXT_HPP__
#define __DRAW_CONTEXT_HPP__
#include <SFML/Graphics.hpp>
#include <memory>

using namespace std;

typedef shared_ptr<sf::RenderWindow> WindowPtr;

class DrawContext {
private:
    WindowPtr window;
public:
    DrawContext(WindowPtr window);
    WindowPtr getWindow();
};
#endif
