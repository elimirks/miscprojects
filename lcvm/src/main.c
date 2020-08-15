#include <stdio.h>
#include <string.h>

#include "vm.h"

int main() {
    Expression *root = parse();

    printf("Input: \n");
    printExpression(root);
    printf("\n\nResult:\n");
    Expression *evaluated = evaluateExpression(root);
    printExpression(evaluated);
    printf("\n");
}
