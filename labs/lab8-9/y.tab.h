/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     INT = 258,
     STR = 259,
     DOUBLE = 260,
     IF = 261,
     ELSE = 262,
     READ_INT = 263,
     READ_STR = 264,
     READ_DOUBLE = 265,
     PRINT_INT = 266,
     PRINT_STR = 267,
     PRINT_DOUBLE = 268,
     WHILE = 269,
     GET = 270,
     SET = 271,
     ADD = 272,
     SUB = 273,
     MUL = 274,
     DIV = 275,
     MOD = 276,
     LTE = 277,
     GTE = 278,
     EQ = 279,
     NEQ = 280,
     LT = 281,
     GT = 282,
     ASSIGN = 283,
     OPEN_CURLY = 284,
     CLOSE_CURLY = 285,
     OPEN_BRACKET = 286,
     CLOSE_BRACKET = 287,
     OPEN_SQUARE = 288,
     CLOSE_SQUARE = 289,
     SEMICOLON = 290,
     COMMA = 291,
     IDENTIFIER = 292,
     DOUBLE_CONSTANT = 293,
     INT_CONSTANT = 294,
     STRING_CONST = 295
   };
#endif
/* Tokens.  */
#define INT 258
#define STR 259
#define DOUBLE 260
#define IF 261
#define ELSE 262
#define READ_INT 263
#define READ_STR 264
#define READ_DOUBLE 265
#define PRINT_INT 266
#define PRINT_STR 267
#define PRINT_DOUBLE 268
#define WHILE 269
#define GET 270
#define SET 271
#define ADD 272
#define SUB 273
#define MUL 274
#define DIV 275
#define MOD 276
#define LTE 277
#define GTE 278
#define EQ 279
#define NEQ 280
#define LT 281
#define GT 282
#define ASSIGN 283
#define OPEN_CURLY 284
#define CLOSE_CURLY 285
#define OPEN_BRACKET 286
#define CLOSE_BRACKET 287
#define OPEN_SQUARE 288
#define CLOSE_SQUARE 289
#define SEMICOLON 290
#define COMMA 291
#define IDENTIFIER 292
#define DOUBLE_CONSTANT 293
#define INT_CONSTANT 294
#define STRING_CONST 295




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

