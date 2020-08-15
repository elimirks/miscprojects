#ifndef __VM_H__
#define __VM_H__

#define MAX_NAME_LEN (1024)

struct Expression;

typedef enum {
    ExpressionTypeVariable = 1,
    ExpressionTypeAbstraction,
    ExpressionTypeApplication
} ExpressionType;

typedef struct {
    char *identifier;
} ExpressionVariable;

typedef struct {
    ExpressionVariable *parameter;
    struct Expression *term;
} ExpressionAbstraction;

typedef struct {
    struct Expression *function;
    struct Expression *parameter;
} ExpressionApplication;

typedef struct Expression {
    ExpressionType type;

    union {
        ExpressionVariable *variable;
        ExpressionAbstraction *abstraction;
        ExpressionApplication *application;
    };
} Expression;

Expression * parse();

void printExpression(Expression *expr);
Expression *evaluateExpression(Expression *expr);

#endif
