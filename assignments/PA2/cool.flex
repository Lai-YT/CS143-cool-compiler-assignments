/*
 *  The scanner definition for COOL.
 */

%option noyywrap

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 *  to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
#include <stdbool.h>
#include <stddef.h> /* size_t */
#include <stdlib.h> /* atoi */
#include <string.h> /* strncpy */

/*
 * For nested comments.
 */

int comment_level = 0;

/*
 * For string constants.
 */

void ResetStrBuf() {
  string_buf_ptr = string_buf;
  *string_buf_ptr = '\0';
}

bool StrWillRunOutOfRange(size_t length_to_append) {
  return string_buf_ptr + length_to_append
      > string_buf + MAX_STR_CONST - 1;
}

%}

%x INLINE_COMMENT NESTED_COMMENT STRING TOO_LONG_STRING

/*
 * Define names for regular expressions here.
 */

DIGIT  [0-9]

/* XXX: "character class expressions" aren't working correctly */
TYPEID  [A-Z][a-zA-Z0-9_]*
OBJECTID  [a-zA-Z][a-zA-Z0-9_]*

DARROW  =>
ASSIGN  <-
LE  <=
SINGLE_OP  [-+*\/:~<=(){};.,@]

%%

 /*
  *  Nested comments
  */
--  BEGIN(INLINE_COMMENT);
"(*"  comment_level = 1; BEGIN(NESTED_COMMENT);
"*)" {
  yylval.error_msg = "Unmatched *)";
  return ERROR;
}
<NESTED_COMMENT>"(*"  comment_level++;
<NESTED_COMMENT>"*)"  if (!--comment_level) { BEGIN(INITIAL); }
<NESTED_COMMENT>\n  curr_lineno++;
<INLINE_COMMENT>\n  curr_lineno++; BEGIN(INITIAL);
<INLINE_COMMENT,NESTED_COMMENT>.  ; /* eat up */
<INLINE_COMMENT,NESTED_COMMENT><<EOF>> {
  /* change back to INITIAL so the next scan will trigger the yyterminate()
    instead of being stuck in the infinite loop */
  BEGIN(INITIAL);
  yylval.error_msg = "EOF in comment";
  return ERROR;
}

\n  curr_lineno++;
[ \f\r\t\v]+  ; /* eat up */

 /*
  *  The multiple-character operators.
  */
{DARROW}  return DARROW;
{ASSIGN}  return ASSIGN;
{LE}  return LE;

 /*
  * The single-character operators.
  */
{SINGLE_OP}  return yytext[0];

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
(?i:class)  return CLASS;
(?i:else)  return ELSE;
(?i:fi)  return FI;
(?i:if)  return IF;
(?i:in)  return IN;
(?i:inherits)  return INHERITS;
(?i:isvoid)  return ISVOID;
(?i:let)  return LET;
(?i:loop)  return LOOP;
(?i:pool)  return POOL;
(?i:then)  return THEN;
(?i:while)  return WHILE;
(?i:case)  return CASE;
(?i:esac)  return ESAC;
(?i:new)  return NEW;
(?i:of)  return OF;
(?i:not)  return NOT;

 /*
  * Booleans.
  */
t(?i:rue)   cool_yylval.boolean = true;  return BOOL_CONST;
f(?i:alse)  cool_yylval.boolean = false; return BOOL_CONST;

 /*
  * Integers.
  */
{DIGIT}+ {
  cool_yylval.symbol = inttable.add_int(atoi(yytext));
  return INT_CONST;
}

 /*
  * Identifiers.
  */
{TYPEID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}
{OBJECTID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for
  *  \n \t \b \f, the result is c.
  *
  */
\"  ResetStrBuf(); BEGIN(STRING);
<STRING>\\(.|\n) {
  if (StrWillRunOutOfRange(1)) {
    BEGIN(TOO_LONG_STRING);
    break;
  }
  switch (yytext[1]) {
    case 'b':  *string_buf_ptr++ = '\b'; break;
    case 't':  *string_buf_ptr++ = '\t'; break;
    case 'n':  *string_buf_ptr++ = '\n'; break;
    case 'f':  *string_buf_ptr++ = '\f'; break;
    case '\"': *string_buf_ptr++ = '"';  break;
    case '\\': *string_buf_ptr++ = '\\'; break;
    case '\n': *string_buf_ptr++ = '\n'; curr_lineno++; break;
    default:   *string_buf_ptr++ = yytext[1]; break;
  }
}
<TOO_LONG_STRING>\\.  ; /* no more recording */
<TOO_LONG_STRING>\\\n  curr_lineno++;
<STRING>[^\\"\n]+ {
  if (StrWillRunOutOfRange(yyleng)) {
    BEGIN(TOO_LONG_STRING);
    break;
  }
  strncpy(string_buf_ptr, yytext, yyleng);
  string_buf_ptr += yyleng;
}
<TOO_LONG_STRING>[^\\"\n]+  ; /* eat up */
<STRING>\" {
  BEGIN(INITIAL);
  *string_buf_ptr = '\0';
  cool_yylval.symbol = idtable.add_string(string_buf);
  return STR_CONST;
}
<TOO_LONG_STRING>\" {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "String constant too long";
  return ERROR;
}
<STRING,TOO_LONG_STRING>\n {
  BEGIN(INITIAL);
  curr_lineno++;
  cool_yylval.error_msg = "Unterminated string constant";
  return ERROR;
}
 /* XXX: there's an error call "String contains null character" but I don't know how to produce it */

. {
  yylval.error_msg = yytext;
  return ERROR;
}
%%
