%{
	#include <ctype.h>
  #include <stdio.h>

  #define YYDEBUG 1

  int productions[1024];
  int production_index = 0;

  void add_production(int production) {
    productions[production_index++] = production;
  }

  void print_productions() {
    for (int i = 0; i < production_index; i++) {
      printf("%d ", productions[i]);
    }
    printf("\n");
  }
%}

%token INT
%token STR
%token DOUBLE
%token IF
%token ELSE
%token READ_INT
%token READ_STR
%token READ_DOUBLE
%token PRINT_INT
%token PRINT_STR
%token PRINT_DOUBLE
%token WHILE
%token GET
%token SET
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token LTE
%token GTE
%token EQ
%token NEQ
%token LT
%token GT
%token ASSIGN
%token OPEN_CURLY
%token CLOSE_CURLY
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token OPEN_SQUARE
%token CLOSE_SQUARE
%token SEMICOLON
%token COMMA
%token IDENTIFIER
%token DOUBLE_CONSTANT
%token INT_CONSTANT
%token STRING_CONST

%start program

%%

program : statement_list
        ;

statement_list : statement {add_production(1);}
               | statement_list statement {add_production(2);}
               ;

statement : variable_declaration SEMICOLON { add_production(3); }
          | function_call SEMICOLON { add_production(4); }
          | assignment SEMICOLON { add_production(5); }
          | if { add_production(6); }
          | while { add_production(7); }
          ;

type : INT { add_production(8); }
     | STR { add_production(9); }
     | DOUBLE { add_production(11); }
     ;

variable_declaration : type IDENTIFIER { add_production(11); }
                     | type IDENTIFIER OPEN_SQUARE INT_CONSTANT CLOSE_SQUARE { add_production(12); }
                     ;

function_call : get_call { add_production(13); }
              | set_call { add_production(14); }
              | read_call { add_production(15); }
              | print_call { add_production(16); }

get_call : GET OPEN_BRACKET IDENTIFIER COMMA int_expression CLOSE_BRACKET { add_production(17); }
         ;
set_call : SET OPEN_BRACKET IDENTIFIER COMMA int_expression COMMA expression CLOSE_BRACKET { add_production(18); }
         ;
read_function : READ_INT { add_production(19); }
              | READ_STR { add_production(20); }
              | READ_DOUBLE { add_production(21); }
              ;
read_call : read_function OPEN_BRACKET CLOSE_BRACKET { add_production(22); }
          ;
print_function : PRINT_INT { add_production(23); }
               | PRINT_STR { add_production(24); }
               | PRINT_DOUBLE { add_production(25); }
               ;
print_call : print_function OPEN_BRACKET expression CLOSE_BRACKET { add_production(26); }
           ;

assignment : IDENTIFIER ASSIGN expression { add_production(27); }
           ;

if : IF OPEN_BRACKET bool_expression CLOSE_BRACKET OPEN_CURLY statement_list CLOSE_CURLY { add_production(28); }
   | IF OPEN_BRACKET bool_expression CLOSE_BRACKET OPEN_CURLY statement_list CLOSE_CURLY ELSE OPEN_CURLY statement_list CLOSE_CURLY { add_production(29); }
   ;

while : WHILE OPEN_BRACKET bool_expression CLOSE_BRACKET OPEN_CURLY statement_list CLOSE_CURLY { add_production(30); }
      ;

constant : INT_CONSTANT { add_production(31); }
         | STRING_CONST { add_production(32); }
         | DOUBLE_CONSTANT { add_production(33); }
         ;

bool_operator : EQ { add_production(34); }
              | NEQ { add_production(35); }
              | LT { add_production(36); }
              | GT { add_production(37); }
              | LTE { add_production(38); }
              | GTE { add_production(39); }
              ;

constant_or_identifier : constant { add_production(40); }
                       | IDENTIFIER { add_production(41); }
                       ;

bool_expression : constant_or_identifier bool_operator constant_or_identifier { add_production(42); }
                ;

expression : int_expression { add_production(43); }
           | str_expression { add_production(44); }
           | double_expression { add_production(45); }
           ;

int_expression : int_term { add_production(46); }
              | int_term ADD int_expression { add_production(47); }
              | int_term SUB int_expression { add_production(48); }
              ;

int_term : int_factor { add_production(49); }
         | int_factor MUL int_term { add_production(50); }
         | int_factor DIV int_term { add_production(51); }
         | int_factor MOD int_term { add_production(52); }
         ;

int_factor : INT_CONSTANT { add_production(53); }
           | IDENTIFIER { add_production(54); }
           | OPEN_BRACKET int_expression CLOSE_BRACKET { add_production(55); }
           ;

double_expression : double_term { add_production(56); }
                 | double_term ADD double_expression { add_production(57); }
                 | double_term SUB double_expression { add_production(58); }
                 ;

double_term : double_factor { add_production(59); }
           | double_factor MUL double_term { add_production(60); }
           | double_factor DIV double_term { add_production(61); }
           ;

double_factor : DOUBLE_CONSTANT { add_production(62); }
             | IDENTIFIER { add_production(63); }
             | OPEN_BRACKET double_expression CLOSE_BRACKET { add_production(64); }
             ;

str_expression : str_term { add_production(65); }
              | str_term ADD str_expression { add_production(66); }
              ;

str_term : str_constant { add_production(67); }
         | IDENTIFIER { add_production(68); }
         ;

str_constant : STRING_CONST { add_production(69); }
             ;

%%

yyerror(char *s)
{
    printf("%s\n", s);
}

extern FILE *yyin;

int main(int argc, char** argv)
{
  if (argc > 1)
  {
    yyin = fopen(argv[1], "r");
    if (!yyin)
    {
      printf("'%s': Could not open specified file\n", argv[1]);
      return 1;
    }
  }

  if (argc > 2 && strcmp(argv[2], "-d") == 0)
  {
    printf("Debug mode on\n");
    yydebug = 1;
  }

  printf("Starting parsing...\n");

  if (yyparse() == 0)
  {
    printf("Parsing successful!\n");
    print_productions();
    return 0;
  }

  printf("Parsing failed!\n");
  return 0;
}
