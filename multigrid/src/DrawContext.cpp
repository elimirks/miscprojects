#include "DrawContext.hpp"

DrawContext::DrawContext(WindowPtr window) {
    this->window = window;
}

WindowPtr DrawContext::getWindow() {
    return this->window;
}
