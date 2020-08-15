%{
#include <string.h>
#include <stdio.h>
#include "../src/vm.h"

void yyerror(char const *);
extern int yylex(void);

Expression *root;
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
%token <strVal> Id

%type <exprVal> variable
%type <exprVal> abstraction
%type <exprVal> application
%type <exprVal> expr

/* 
 *Grammar rules and actions
 */
%%

program: expr Whitespace {
     root = $1;
};

lparen: LParen | LParen Whitespace;
rparen: RParen | Whitespace RParen;

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

abstraction: Lambda variable Period expr {
    Expression *expr = malloc(sizeof(Expression));
    expr->type = ExpressionTypeAbstraction;
    expr->abstraction = malloc(sizeof(ExpressionAbstraction));
    expr->abstraction->parameter = $2->variable;
    expr->abstraction->term = $4;

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
    return root;
}

void yyerror(char const *m) {
  fprintf(stderr, "Scope parse error: %s\n", m);
  exit(1);
}
