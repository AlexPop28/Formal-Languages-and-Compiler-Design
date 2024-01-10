%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int line_number = 1;
%}

%option noyywrap

IDENTIFIER		[a-z][a-z0-9_]*
DOUBLE_CONST  (INT_CONST|([+-]0))[.][0-9]*
INT_CONST     ([+-]?[1-9][0-9]*)|0
STRING_CONST	\"[a-z0-9 _]*\"

%%

"int"|"str"|"double"|"if"|"else"|"read_int"|"read_str"|"read_double"|"print_int"|"print_str"|"print_double"|"while"|"get"|"set" { printf("Reserved word: %s\n", yytext); }
"+"|"-"|"*"|"/"|"%"|"<="|">="|"=="|"!="|"<"|">"|"="		 { printf("Operator: %s\n", yytext); }
"{"|"}"|"("|")"|"["|"]"|";"|"\""		                   { printf("Separator: %s\n", yytext); }
{IDENTIFIER}		                                       { printf("Identifier: %s\n", yytext); }
{INT_CONST}	                                           { printf("Integer: %s\n", yytext); }
({INT_CONST}|([+-]0))[.][0-9]*                                           { printf("Double: %s\n", yytext); }
{STRING_CONST}	                                       { printf("String: %s\n", yytext); }

[ \t]+	                                               {}
[\n]+	                                                 {line_number++;}

.                                                      { printf("Lexical error; Unexpected token at line %d: %s\n", line_number, yytext); }

%%


int main() {
  yylex();
}
