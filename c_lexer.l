%{
#include "c_parser.tab.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

    /* Track line numbers for error reporting */
int line_num = 1;
int column = 0;

    /* Function prototypes */
void count_column();
void reset_lexer();
%}

%option noyywrap
%option yylineno

    /* Define states for handling comments */
%x C_COMMENT
%x CPP_COMMENT

%%

"/*"                    { BEGIN(C_COMMENT); }
<C_COMMENT>"*/"         { BEGIN(INITIAL); }
<C_COMMENT>\n           { line_num++; }
<C_COMMENT>.            { /* ignore comment content */ }

"//"                    { BEGIN(CPP_COMMENT); }
<CPP_COMMENT>\n         { line_num++; BEGIN(INITIAL); }
<CPP_COMMENT>.          { /* ignore comment content */ }

"#"[^\n]*               { /* ignore preprocessor directives */ }

    /* Type specifiers */
"int"                   { count_column(); return INT; }
"char"                  { count_column(); return CHAR; }
"short"                 { count_column(); return SHORT; }
"unsigned"              { count_column(); return UNSIGNED; }
"void"                  { count_column(); return VOID; }
"bool"                  { count_column(); return BOOL; }
"struct"                { count_column(); return STRUCT; }
"typedef"               { count_column(); return TYPEDEF; }
"enum"                  { count_column(); return ENUM; }

    /* Control flow keywords */
"if"                    { count_column(); return IF; }
"else"                  { count_column(); return ELSE; }
"switch"                { count_column(); return SWITCH; }
"case"                  { count_column(); return CASE; }
"default"               { count_column(); return DEFAULT; }
"for"                   { count_column(); return FOR; }
"while"                 { count_column(); return WHILE; }
"do"                    { count_column(); return DO; }
"break"                 { count_column(); return BREAK; }
"continue"              { count_column(); return CONTINUE; }
"return"                { count_column(); return RETURN; }

    /* memory operations */
"malloc" { count_column(); return MALLOC; }
"free" { count_column(); return FREE; }
"atomic" { count_column(); return ATOMIC; }
"assert" { count_column(); return ASSERT; }
"d_step" { count_column(); return D_STEP; }
"sizeof" { count_column(); return SIZEOF; }


    /* special operators in Promela */
"::" { count_column(); return DCOLON; }
"->" { count_column(); return ARROW; }


    /* Operators */
"=="                    { count_column(); return EQ; }
"!="                    { count_column(); return NEQ; }
"<="                    { count_column(); return LE; }
">="                    { count_column(); return GE; }
"<"                     { count_column(); return LT; }
">"                     { count_column(); return GT; }
"="                     { count_column(); return ASSIGN; }
"+"                     { count_column(); return PLUS; }
"-"                     { count_column(); return MINUS; }
"*"                     { count_column(); return STAR; }
"/"                     { count_column(); return DIV; }
"%"                     { count_column(); return MOD; }
"&"                     { count_column(); return BITAND; }
"&&"                    { count_column(); return AND; }
"||"                    { count_column(); return OR; }
"!"                     { count_column(); return NOT; }
"~"                     { count_column(); return BITNOT; }
"|"                     { count_column(); return BITOR; }
"^"                     { count_column(); return BITXOR; }
"++"                    { count_column(); return INC; }
"--"                    { count_column(); return DEC; }
"+="                    { count_column(); return PLUSEQ; }
"-="                    { count_column(); return MINUSEQ; }
"*="                    { count_column(); return MULEQ; }
"/="                    { count_column(); return DIVEQ; }
"%="                    { count_column(); return MODEQ; }

    /* Punctuation */
";"                     { count_column(); return SEMI; }
","                     { count_column(); return COMMA; }
":"                     { count_column(); return COLON; }
"("                     { count_column(); return LPAREN; }
")"                     { count_column(); return RPAREN; }
"{"                     { count_column(); return LBRACE; }
"}"                     { count_column(); return RBRACE; }
"["                     { count_column(); return LBRACK; }
"]"                     { count_column(); return RBRACK; }
"."                     { count_column(); return DOT; }
"->"                    { count_column(); return ARROW; }
"?"                     { count_column(); return QMARK; }

    /* Constants and identifiers */
[0-9]+                  { 
                          count_column(); 
                          yylval.num = atoi(yytext); 
                          return NUMBER; 
                        }
[0-9]+\.[0-9]+          {
                          count_column();
                          yylval.fnum = atof(yytext);
                          return FLOAT;
                        }
\"([^\\\"]|\\.)*\"      {
                          count_column();
                          yylval.str = strdup(yytext);
                          return STRING;
                        }
\'([^\\\']|\\.)?\'      {
                          count_column();
                          yylval.str = strdup(yytext);
                          return CHAR;
                        }

[a-zA-Z_][a-zA-Z0-9_]*  { 
                          count_column(); 
                          yylval.str = strdup(yytext); 
                          return ID; 
                        }

    /* Whitespace handling */
[ \t]+                  { count_column(); /* Skip whitespace */ }
\n                      { line_num++; column = 0; }
\r                      { /* Ignore carriage returns */ }

    /* Catch unrecognized characters */
.                       { 
                          fprintf(stderr, "Unrecognized character: %s at line %d\n", 
                                  yytext, line_num); 
                          count_column(); 
                        }

%%

    /* Count columns for error reporting */
void count_column() {
    column += yyleng;
}

    /* Reset lexer state */
void reset_lexer() {
    BEGIN(INITIAL);
    line_num = 1;
    column = 0;
}
