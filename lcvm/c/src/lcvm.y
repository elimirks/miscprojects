%{
#include <string.h>
#include <stdio.h>
#include "../src/vm.h"

void yyerror(char const *);
extern int yylex(void);

Expression *root;

typedef struct DefinitionNode {
    ExpressionVariable *variable;
    Expression *value;

    struct DefinitionNode *next;
} DefinitionNode;

DefinitionNode *definitions = NULL;
%}

%define parse.error verbose

%union {
    Expression *exprVal;
    char *strVal;
}

%token Whitespace
%token Lambda
%token Period
%token LParen
%token RParen
%token Assignment
%token <strVal> Id

%type <exprVal> variable
%type <exprVal> abstraction
%type <exprVal> application
%type <exprVal> expr

%%

program: optwhitespace optdefinitions expr optwhitespace {
    root = $3;
};

optdefinitions: | definitions;
definitions: definition Whitespace
           | definition Whitespace definitions;

definition: variable optwhitespace Assignment optwhitespace expr {
    DefinitionNode *newHead = malloc(sizeof(DefinitionNode));
    newHead->next = definitions;
    newHead->variable = $1->variable;
    newHead->value = $5;
    
    free($1);

    definitions = newHead;
};

optwhitespace: | whitespace;
whitespace: Whitespace | Whitespace whitespace;

lparen: LParen optwhitespace;
rparen: optwhitespace RParen;

expr: variable | abstraction | application
    | lparen expr rparen {
        $$ = $2;
    };

variable: Id {
    Expression *expr = malloc(sizeof(Expression));
    expr->type = ExpressionTypeVariable;
    expr->variable = malloc(sizeof(ExpressionVariable));

    size_t textLen = strlen($1);
    expr->variable->identifier = malloc(sizeof(char) * (textLen + 1));
    strncpy(expr->variable->identifier, $1, textLen);
    expr->variable->identifier[textLen] = '\0';

    $$ = expr;
};

abstraction: Lambda variable Period optwhitespace expr {
    Expression *expr = malloc(sizeof(Expression));
    expr->type = ExpressionTypeAbstraction;
    expr->abstraction = malloc(sizeof(ExpressionAbstraction));
    expr->abstraction->parameter = $2->variable;
    expr->abstraction->term = $5;

    $$ = expr;
};

application: lparen expr Whitespace expr rparen {
    Expression *expr = malloc(sizeof(Expression));
    expr->type = ExpressionTypeApplication;
    expr->application = malloc(sizeof(ExpressionApplication));
    expr->application->function = $2;
    expr->application->parameter = $4;
    
    $$ = expr;
};

%%

#include "../obj/lcvm.yy.c"

Expression * parse() {
    yyin = stdin;
    yyparse();
    
    // Construct bindings for definitions
    while (definitions != NULL) {
        DefinitionNode *top = definitions;
        definitions = definitions->next;
        
        Expression *abstractionExpr = malloc(sizeof(Expression));
        ExpressionAbstraction *abstraction = malloc(sizeof(ExpressionAbstraction));
        abstractionExpr->type = ExpressionTypeAbstraction;
        abstractionExpr->abstraction = abstraction;
        
        Expression *applicationExpr = malloc(sizeof(Expression));
        ExpressionApplication *application = malloc(sizeof(ExpressionApplication));
        applicationExpr->type = ExpressionTypeApplication;
        applicationExpr->application = application;
        
        abstraction->parameter = top->variable;
        abstraction->term = root;
        
        application->function = abstractionExpr;
        application->parameter = top->value;
        
        root = applicationExpr;
        
        free(top);
    }
    
    return root;
}

void yyerror(char const *m) {
  fprintf(stderr, "Scope parse error: %s\n", m);
  exit(1);
}
