%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

  int line_number = 1;

  typedef struct {
    char* text;
    int token;
  } TokenMap;

  TokenMap token_map[] = {
    {"int", INT},
    {"str", STR},
    {"double", DOUBLE},
    {"if", IF},
    {"else", ELSE},
    {"read_int", READ_INT},
    {"read_str", READ_STR},
    {"read_double", READ_DOUBLE},
    {"print_int", PRINT_INT},
    {"print_str", PRINT_STR},
    {"print_double", PRINT_DOUBLE},
    {"while", WHILE},
    {"get", GET},
    {"set", SET},


    {"+", ADD},
    {"-", SUB},
    {"*", MUL},
    {"/", DIV},
    {"%", MOD},

    {"<=", LTE},
    {">=", GTE},
    {"==", EQ},
    {"!=", NEQ},
    {"<", LT},
    {">", GT},

    {"=", ASSIGN},

    {"{", OPEN_CURLY},
    {"}", CLOSE_CURLY},
    {"(", OPEN_BRACKET},
    {")", CLOSE_BRACKET},
    {"[", OPEN_SQUARE},
    {"]", CLOSE_SQUARE},

    {";", SEMICOLON},
    {",", COMMA},
  };

  int map_to_token(char* text) {
    int size = sizeof(token_map) / sizeof(TokenMap);
    for (int i = 0; i < size; ++i) {
      if (strcmp(text, token_map[i].text) == 0) {
        return token_map[i].token;
      }
    }

    printf("Token not found: %s\n", text);
    return -1;
  }
%}

%option noyywrap

IDENTIFIER		[a-z][a-z0-9_]*
DOUBLE_CONST  ([+-]?[0-9]+[.][0-9]*|[+-]?[.][0-9]+)
INT_CONST     ([+-]?[1-9][0-9]*|0)
STRING_CONST	\"[a-z0-9 _]*\"

%%

"int"|"str"|"double"|"if"|"else"|"read_int"|"read_str"|"read_double"|"print_int"|"print_str"|"print_double"|"while"|"get"|"set" { printf("Reserved word: %s\n", yytext); return map_to_token(yytext); }
"+"|"-"|"*"|"/"|"%"|"<="|">="|"=="|"!="|"<"|">"|"="		 { printf("Operator: %s\n", yytext); return map_to_token(yytext);}
"{"|"}"|"("|")"|"["|"]"|";"|"\""		                   { printf("Separator: %s\n", yytext); return map_to_token(yytext);}
  {IDENTIFIER}		                                       { printf("Identifier: %s\n", yytext); return IDENTIFIER; }
  {INT_CONST}	                                           { printf("Integer: %s\n", yytext); return INT_CONST; }
  {DOUBLE_CONST}                                         { printf("Double: %s\n", yytext); return DOUBLE_CONST; }
  {STRING_CONST}	                                       { printf("String: %s\n", yytext); return STRING_CONST; }

[ \t]+	                                               {}
[\n]+	                                                 {line_number++;}

.                                                      { printf("Lexical error; Unexpected token at line %d: %s\n", line_number, yytext); }

%%
