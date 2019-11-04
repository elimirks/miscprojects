#ifndef __DRAW_CONTEXT_HPP__
#define __DRAW_CONTEXT_HPP__
#include <SFML/Graphics.hpp>
#include <memory>

using namespace std;

typedef shared_ptr<sf::RenderWindow> WindowPtr;

class DrawContext {
private:
    WindowPtr window;
    // Perpendicular bisector of the context, away from the origin
    double perpBisector = 0.0;
    double currentX = 0.0, currentY = 0.0;
    double directionMultiplier = 1;
public:
    DrawContext(WindowPtr window);
    WindowPtr getWindow();

    void setPerpBisector(double angle);
    void setCurrentX(double x);
    void setCurrentY(double y);

    double getPerpBisector();
    double getCurrentX();
    double getCurrentY();

    double getDirectionMultiplier();

    void resetPosition();

    DrawContext mirroredContextForPosition(double angle, double x, double y);
};
#endif
