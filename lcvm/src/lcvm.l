%{
    //#include "../src/scanner.h"
    /* LCVM Lexer */
    char *yyfilename; // Set in scanner.c
%}

%option yylineno
%option noyywrap

%{
  /* REGEX Variable Initialization */
%}
whitespace   (;.*[\n]|[ \t\r\n])+
identifier   ([a-zA-Z0-9_])+
lambda       \\
assignment   :=

%%

{identifier} {
    yylval.strVal = yytext;
    return Id;
};
{whitespace}   return Whitespace;
{lambda}       return Lambda;
{assignment}   return Assignment;
"."            return Period;
"("            return LParen;
")"            return RParen;

. {
    const char *yyfilename = "TODO";
    fprintf(stderr, "Invalid token: '%s' on line %d in %s\n",
            yytext, yylineno, yyfilename);
    exit(1);
}
