#include <cmath>

#include "DrawContext.hpp"

DrawContext::DrawContext(WindowPtr window) {
    this->window = window;
}

WindowPtr DrawContext::getWindow() {
    return this->window;
}

void DrawContext::setPerpBisector(double angle) {
    this->perpBisector = angle;
}

void DrawContext::setCurrentX(double x) {
    this->currentX = x;
}

void DrawContext::setCurrentY(double y) {
    this->currentY = y;
}

double DrawContext::getPerpBisector() {
    return this->perpBisector;
}

double DrawContext::getCurrentX() {
    return this->currentX;
}

double DrawContext::getCurrentY() {
    return this->currentY;
}

double DrawContext::getDirectionMultiplier() {
    return directionMultiplier;
}

void DrawContext::resetPosition() {
    currentY = 0.0;
    currentX = 0.0;
    perpBisector = 0.0;
}

DrawContext DrawContext::mirroredContextForPosition(double angle, double x, double y) {
    DrawContext context(window);
    context.setPerpBisector(angle);
    context.setCurrentX(x);
    context.setCurrentY(y);
    context.directionMultiplier = -directionMultiplier;
    return context;
}
