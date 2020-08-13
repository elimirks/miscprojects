#include "scanner.h"
#include "../obj/lcvm.yy.c"

void parse() {
    token_t tokenType;

    while ((tokenType = yylex())) {
        printf("%d ", tokenType);
    }
}
