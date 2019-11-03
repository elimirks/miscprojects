#ifndef __DRAW_CONTEXT_HPP__
#define __DRAW_CONTEXT_HPP__
#include <SFML/Graphics.hpp>
#include <memory>

using namespace std;

typedef shared_ptr<sf::RenderWindow> WindowPtr;

class DrawContext {
private:
    WindowPtr window;
    double currentAngle = 0.0; // Vector angle of the origin draw edge
    double currentX = 0.0, currentY = 0.0;
    double direction = -1;
public:
    DrawContext(WindowPtr window);
    WindowPtr getWindow();

    void setCurrentAngle(double angle);
    void setCurrentX(double x);
    void setCurrentY(double y);

    double getCurrentAngle();
    double getCurrentX();
    double getCurrentY();

    double getDirection();

    void resetPosition();

    DrawContext mirroredContextForPosition(double angle, double x, double y);
};
#endif
