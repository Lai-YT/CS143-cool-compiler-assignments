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
#include <assert.h>
#include <ctype.h> /* tolower */
#include <stdbool.h>
#include <stddef.h> /* size_t */
#include <stdlib.h> /* atoi */

typedef struct {
  char *keyword;
  int code;
} KeywordAndCode;

KeywordAndCode keywords[] = {
  {"class", CLASS}, {"else", ELSE}, {"fi", FI}, {"if", IF}, {"in", IN},
  {"inherits", INHERITS}, {"isvoid", ISVOID}, {"let", LET}, {"loop", LOOP},
  {"pool", POOL}, {"then", THEN}, {"while", WHILE}, {"case", CASE}, {"esac", ESAC},
  {"new", NEW}, {"of", OF}, {"not", NOT},
};

void ToLowerStr(char *s) {
  for (int i = 0; s[i]; i++) {
    s[i] = tolower(s[i]);
  }
}

void ShouldNotReachHere() {
  assert(0);
}

bool in_one_line_comment = false;
int nested_comment_level = 0;

%}

%x COMMENT
/*
 * Define names for regular expressions here.
 */
DIGIT [0-9]

KEYWORD (?i:class|else|fi|if|in|inherits|isvoid|let|loop|pool|then|while|case|esac|new|of|not)
BOOL  [Tt]rue|[Ff]alse

TYPE_ID Object|Bool|Int|String|SELF_TYPE|self
OBJECT_ID [:alpha:][[:alnum:]_]*

DARROW  =>
ASSIGN  <-
/* - and / has to be escaped */
SINGLE_OP  [-+*\/:~<>=(){};.,]

%%

 /*
  *  Nested comments
  */
--  {
  if (!in_one_line_comment) {
    in_one_line_comment = true;
    BEGIN(COMMENT);
  }
}
<INITIAL,COMMENT>"(*"  {
  if (!in_one_line_comment) {
    nested_comment_level++;
    BEGIN(COMMENT);
  }
}
<COMMENT>"*)" {
  if (!in_one_line_comment
      && --nested_comment_level == 0) {
    BEGIN(INITIAL);
  }
}
<COMMENT>\n {
  curr_lineno++;
  if (in_one_line_comment) {
    in_one_line_comment = false;
    BEGIN(INITIAL);
  }
}
<COMMENT>.  ; /* eat up */
 /* TODO: EOF in comment */

\n  curr_lineno++;
[ \f\r\t\v]+  ; /* eat up */

 /*
  *  The multiple-character operators.
  */
{DARROW}  { return (DARROW); }
{ASSIGN}  { return (ASSIGN); }

 /*
  * The single-character operators.
  */
{SINGLE_OP} {
  cool_yylval.symbol = idtable.add_string(yytext);
  assert(!yytext[1]);
  return yytext[0];
}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{KEYWORD} {
  cool_yylval.symbol = idtable.add_string(yytext);

  ToLowerStr(yytext);
  for (size_t i = 0; i < sizeof(keywords)/sizeof(KeywordAndCode); i++) {
    if (strcmp(yytext, keywords[i].keyword) == 0) {
      return keywords[i].code;
    }
  }
  ShouldNotReachHere();
}
{BOOL} {
  idtable.add_string(yytext);

  ToLowerStr(yytext);
  if (strcmp(yytext, "false") == 0) {
    cool_yylval.boolean = 0;
  } else if (strcmp(yytext, "true") == 0) {
    cool_yylval.boolean = 1;
  }
  ShouldNotReachHere();
}
{DIGIT}+  {
  cool_yylval.symbol = inttable.add_int(atoi(yytext));
  return INT_CONST;
}

 /*
  * Identifiers.
  */
{TYPE_ID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}
 /* TODO: OBJECTID after CLASS should be a TYPEID */
{OBJECT_ID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for
  *  \n \t \b \f, the result is c.
  *
  */
 /* TODO: string constants */

%%
