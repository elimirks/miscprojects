#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "vm.h"

// Inefficient but easy scoping mechanism

typedef struct ScopeListNode {
    char *name;
    Expression *value;

    struct ScopeListNode *next;
} ScopeListNode;

typedef struct ScopeList {
    struct ScopeListNode *head;
} ScopeList;

Expression * eval(Expression *expr, ScopeList scope);

Expression * findDeclaration(char *name, ScopeList scope) {
    for (ScopeListNode *itr = scope.head; itr != NULL; itr = itr->next) {
        if (strncmp(name, itr->name, MAX_NAME_LEN) == 0) {
            return itr->value;
        }
    }

    return NULL;
}

void printExpression(Expression *expr) {
    switch (expr->type) {
    case ExpressionTypeVariable:
        printf("%s", expr->variable->identifier);
        break;
    case ExpressionTypeApplication:
        printf("(");
        printExpression(expr->application->function);
        printf(" ");
        printExpression(expr->application->parameter);
        printf(")");
        break;
    case ExpressionTypeAbstraction:
        printf("\\");
        printf("%s", expr->abstraction->parameter->identifier);
        printf(".");
        printExpression(expr->abstraction->term);
    }
}

ExpressionAbstraction *evalFunctionToCall(Expression *function,
                                          ScopeList scope) {
    switch (function->type) {
    case ExpressionTypeVariable: {
        char *identifier = function->variable->identifier;
        Expression *varExpr = findDeclaration(identifier, scope);

        if (varExpr == NULL) {
            fprintf(stderr, "Variable %s not found\n", identifier);
            exit(1);
        }

        Expression *varValue = eval(varExpr, scope);

        if (varValue->type != ExpressionTypeAbstraction) {
            fprintf(stderr, "Expected abstraction, got something else");
            exit(1);
        }

        return varValue->abstraction;
    }
    case ExpressionTypeAbstraction:
        return function->abstraction;
    case ExpressionTypeApplication: {
        Expression *value = eval(function, scope);
        if (value->type != ExpressionTypeAbstraction) {
            fprintf(stderr, "Expected abstraction, got something else");
            exit(1);
        } else {
            return value->abstraction;
        }
    }
    }
}

Expression * evalApplication(Expression *function,
                             Expression *parameter,
                             ScopeList scope) {
    ExpressionAbstraction *func = evalFunctionToCall(function, scope);

    char *paramId = func->parameter->identifier;
    ScopeListNode *param = malloc(sizeof(ScopeListNode));
    param->name = paramId;
    param->value = parameter;
    param->next = scope.head;

    ScopeList newScope;
    newScope.head = param;

    Expression *result = eval(func->term, newScope);

    // Perform lookup if var returned
    if (result->type == ExpressionTypeVariable) {
        char *id = result->variable->identifier;
        Expression *value = findDeclaration(id, newScope);

        if (value == NULL) {
            fprintf(stderr, "Variable %s not found\n", id);
            exit(1);
        }

        result = value;
    }

    free(param);
    return result;
}

Expression * eval(Expression *expr, ScopeList scope) {
    if (expr->type == ExpressionTypeApplication) {
        return evalApplication(expr->application->function,
                               expr->application->parameter,
                               scope);
    } else {
        return expr;
    }
}

Expression * evaluateExpression(Expression *expr) {
    ScopeList scope;
    scope.head = NULL;
    return eval(expr, scope);
}
