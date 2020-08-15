#include <stdio.h>
#include <string.h>

#include "scanner.h"
#include "vm.h"

// Variable x
// Abstraction (\x.M)
// Application (M N)

// Example:
//\f.\x.x

int main() {
    Expression *root = parse();
    printExpression(root);
}
