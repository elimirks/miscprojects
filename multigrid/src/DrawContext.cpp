#include "DrawContext.hpp"

DrawContext::DrawContext(WindowPtr window) {
    this->window = window;
}

WindowPtr DrawContext::getWindow() {
    return this->window;
}

void DrawContext::setCurrentAngle(double angle) {
    this->currentAngle = angle;
}

void DrawContext::setCurrentX(double x) {
    this->currentX = x;
}

void DrawContext::setCurrentY(double y) {
    this->currentY = y;
}

double DrawContext::getCurrentAngle() {
    return this->currentAngle;
}

double DrawContext::getCurrentX() {
    return this->currentX;
}

double DrawContext::getCurrentY() {
    return this->currentY;
}

double DrawContext::getDirection() {
    return direction;
}

void DrawContext::resetPosition() {
    currentY = 0.0;
    currentX = 0.0;
    currentAngle = 0.0;
}

DrawContext DrawContext::mirroredContextForPosition(double angle, double x, double y) {
    DrawContext context(window);
    context.setCurrentAngle(angle);
    context.setCurrentX(x);
    context.setCurrentY(y);
    context.direction = -direction;
    return context;
}
