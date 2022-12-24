#include "base.h"

void panic(const char *message) {
    fprintf(stderr, "%s", message);
    exit(1);
}
