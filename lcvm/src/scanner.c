#include <string.h>

#include "scanner.h"
#include "../obj/lcvm.yy.c"

typedef struct {
    int isToken;

    union {
        TokenType tokenType;
        Expression *expression;
    };
} ParseInfo;

// TODO: Make dynamic
static ParseInfo stack[1024];
static size_t stackTop = 0;

int isApplication(ParseInfo *infos, int size) {
    return size == 2 && !infos[0].isToken && !infos[1].isToken;
}

int isAbstraction(ParseInfo *infos, int size) {
    // TODO: Clean up this mess
    if (size != 4) {
        return 0;
    } else if (!infos[0].isToken || infos[0].tokenType != TokenTypeLambda) {
        return 0;
    } else if (infos[1].isToken || infos[1].expression->type != ExpressionTypeVariable) {
        return 0;
    } else if (!infos[2].isToken || infos[2].tokenType != TokenTypePeriod) {
        return 0;
    } else if (infos[3].isToken) {
        return 0;
    } else {
        return 1;
    }
}

Expression * constructExpressionFromStackTop() {
    int i;
    for (i = stackTop; i >= 0; i--) {
        ParseInfo *info = &stack[i];
        if (info->isToken && info->tokenType == TokenTypeLParen) {
            break;
        }
    }

    if (i < 0) {
        fprintf(stderr, "Parse error on line %d\n", yylineno);
        exit(1);
    }

    ParseInfo *wrapStart = &stack[i + 1];
    int wrappedSize = stackTop - i - 1;

    stackTop = i;

    if (isApplication(wrapStart, wrappedSize)) {
        Expression *expr = malloc(sizeof(Expression));
        expr->type = ExpressionTypeApplication;
        expr->application = malloc(sizeof(ExpressionApplication));
        expr->application->function = stack[i + 1].expression;
        expr->application->parameter = stack[i + 2].expression;
        return expr;
    } else if (isAbstraction(wrapStart, wrappedSize)) {
        Expression *expr = malloc(sizeof(Expression));
        expr->type = ExpressionTypeAbstraction;
        expr->abstraction = malloc(sizeof(ExpressionAbstraction));
        expr->abstraction->parameter = stack[i + 1].expression->variable;
        expr->abstraction->term = stack[i + 2].expression;
        return expr;
    } else {
        fprintf(stderr, "Parse error on line %d\n", yylineno);
        exit(1);
        return NULL;
    }
}

Expression * constructVariable(char *identifier) {
    Expression *expr = malloc(sizeof(Expression));
    expr->type = ExpressionTypeVariable;
    expr->variable = malloc(sizeof(ExpressionVariable));

    size_t textLen = strlen(identifier);
    expr->variable->identifier = malloc(sizeof(char) * (textLen + 1));
    strncpy(expr->variable->identifier, identifier, textLen);
    expr->variable->identifier[textLen] = '\0';

    printf("Constructing variable %s\n", identifier);

    return expr;
}

Expression * parse() {
    Expression *root = NULL;
    TokenType tokenType;

    while ((tokenType = yylex())) {
        printf("%s\n", yytext);

        switch (tokenType) {
        case TokenTypeWhitespace:
            break;
        case TokenTypeRParen: {
            Expression *expr = constructExpressionFromStackTop();

            stack[stackTop++] = (ParseInfo) {
                .isToken = 0,
                .expression = expr
            };
            break;
        }
        case TokenTypeIdentifier:
            stack[stackTop++] = (ParseInfo) {
                .isToken = 0,
                .expression = constructVariable(yytext)
            };
            break;
        default:
            stack[stackTop++] = (ParseInfo) {
                .isToken = 1,
                .tokenType = tokenType
            };
        }
    }

    if (stackTop != 1) {
        fprintf(stderr, "Something terrible has happened\n");
    }

    return root;
}
