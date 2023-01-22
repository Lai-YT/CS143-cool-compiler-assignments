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

void to_lower_s(char *s) {
  for (int i = 0; s[i]; i++) {
    s[i] = tolower(s[i]);
  }
}

%}

/*
 * Define names for regular expressions here.
 */
DIGIT [0-9]

KEYWORD (?i:class|else|fi|if|in|inherits|isvoid|let|loop|pool|then|while|case|esac|new|of|not)
BOOL  [Tt]rue|[Ff]alse

TYPE_ID Object|Bool|Int|String|SELF_TYPE|self
OBJECT_ID [[:alnum:]_]+

DARROW  =>
ASSIGN  <-
/* - and / has to be escaped */
SINGLEOP  [-+*\/:~<>=(){};]

%%

 /*
  *  Nested comments
  */
 /* TODO: nested comments */

\n  curr_lineno++;
[ \f\r\t\v]+ ;  /* eat up */

 /*
  *  The multiple-character operators.
  */
{DARROW}  { return (DARROW); }
{ASSIGN}  { return (ASSIGN); }

 /*
  * The single-character operators.
  */
{SINGLEOP} {
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

  to_lower_s(yytext);
  for (size_t i = 0; i < sizeof(keywords)/sizeof(KeywordAndCode); i++) {
    if (strcmp(yytext, keywords[i].keyword) == 0) {
      return keywords[i].code;
    }
  }
  assert(0);
}
{BOOL} {
  idtable.add_string(yytext);

  to_lower_s(yytext);
  if (strcmp(yytext, "false") == 0) {
    cool_yylval.boolean = 0;
  } else if (strcmp(yytext, "true") == 0) {
    cool_yylval.boolean = 1;
  }
  assert(0);
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
 /* TODO: OBJECT_ID after CLASS should be a TYPE_ID */
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
