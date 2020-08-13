%{
    #include "../src/vm.h"
    /* LCVM Lexer */
    char *yyfilename; // Set in scanner.c
%}

%option yylineno
%option noyywrap

%{
  /* REGEX Variable Initialization */
%}
whitespace   (;.*[\n]|[ \t\r\n])+
identifier   ([a-zA-Z0-9_])*
lambda       \\

%%

{identifier}   return IDENTIFIER;
{whitespace}   return WHITESPACE;
{lambda}       return LAMBDA;
"."            return PERIOD;
"("            return LPAREN;
")"            return RPAREN;

. {
    const char *yyfilename = "TODO";
    fprintf(stderr, "Invalid token: '%s' on line %d in %s\n",
            yytext, yylineno, yyfilename);
    exit(1);
}
