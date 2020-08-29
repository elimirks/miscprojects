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
        printf(" (");
        printExpression(expr->application->parameter);
        printf("))");
        break;
    case ExpressionTypeAbstraction:
        printf("\\");
        printf("%s", expr->abstraction->parameter->identifier);
        printf(".");
        printExpression(expr->abstraction->term);
    }
}

Expression * evalVariable(Expression *expr, ScopeList scope) {
    char *id = expr->variable->identifier;
    Expression *value = findDeclaration(id, scope);
    return value == NULL ? expr : value;
}

Expression * evalApplication(Expression *expr, ScopeList scope) {
    ExpressionApplication *application = expr->application;
    Expression *evaluatedFunction = eval(application->function, scope);
    Expression *evaluatedParameter = eval(application->parameter, scope);

    // Can't evaluate further
    if (evaluatedFunction->type != ExpressionTypeAbstraction) {
        Expression *unevaluatedExpr = malloc(sizeof(Expression));
        unevaluatedExpr->type = ExpressionTypeApplication;
        unevaluatedExpr->application = malloc(sizeof(ExpressionApplication));
        unevaluatedExpr->application->function = evaluatedFunction;
        unevaluatedExpr->application->parameter = evaluatedParameter;
        return unevaluatedExpr;
    }

    char *paramId = evaluatedFunction->abstraction->parameter->identifier;

    ScopeListNode *param = malloc(sizeof(ScopeListNode));
    param->name = paramId;
    param->value = evaluatedParameter;
    param->next = scope.head;

    ScopeList newScope;
    newScope.head = param;

    return eval(evaluatedFunction->abstraction->term, newScope);
}

Expression * evalAbstraction(Expression *expr, ScopeList scope) {
    ExpressionAbstraction *newAbstraction = malloc(sizeof(ExpressionAbstraction));
    newAbstraction->parameter = expr->abstraction->parameter;
    newAbstraction->term = eval(expr->abstraction->term, scope);

    Expression *newExpr = malloc(sizeof(Expression));
    newExpr->type = ExpressionTypeAbstraction;
    newExpr->abstraction = newAbstraction;
    return newExpr;
}

Expression * eval(Expression *expr, ScopeList scope) {
    switch (expr->type) {
    case ExpressionTypeVariable:
        return evalVariable(expr, scope);
    case ExpressionTypeApplication:
        return evalApplication(expr, scope);
    case ExpressionTypeAbstraction: {
        return evalAbstraction(expr, scope);
    }
    }
}

Expression * evaluateExpression(Expression *expr) {
    ScopeList scope;
    scope.head = NULL;
    return eval(expr, scope);
}
