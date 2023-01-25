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
#include <string.h> /* strncpy */

/*
 * For keywords.
 */

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
DIGIT [0-9]

KEYWORD (?i:class|else|fi|if|in|inherits|isvoid|let|loop|pool|then|while|case|esac|new|of|not)
BOOL  t(?i:rue)|f(?i:alse)

/* XXX: "character class expressions" aren't working correctly */
TYPEID [A-Z][a-zA-Z0-9_]*
OBJECTID [a-zA-Z][a-zA-Z0-9_]*

DARROW  =>
ASSIGN  <-
LE  <=
/* - and / has to be escaped */
SINGLE_OP  [-+*\/:~<=(){};.,@]

%%

 /*
  *  Nested comments
  */
--  BEGIN(INLINE_COMMENT);
"(*"  comment_level = 1; BEGIN(NESTED_COMMENT);
"*)"  {
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
{DARROW}  return (DARROW);
{ASSIGN}  return (ASSIGN);
{LE}  return (LE);

 /*
  * The single-character operators.
  */
{SINGLE_OP} {
  assert(!yytext[1]);
  return yytext[0];
}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{KEYWORD} {
  ToLowerStr(yytext);
  for (size_t i = 0; i < sizeof(keywords)/sizeof(KeywordAndCode); i++) {
    if (strcmp(yytext, keywords[i].keyword) == 0) {
      return keywords[i].code;
    }
  }
  ShouldNotReachHere();
}
{BOOL} {
  ToLowerStr(yytext);
  if (strcmp(yytext, "false") == 0) {
    cool_yylval.boolean = false;
    return BOOL_CONST;
  } else if (strcmp(yytext, "true") == 0) {
    cool_yylval.boolean = true;
    return BOOL_CONST;
  }
  ShouldNotReachHere();
}

 /*
  * Integers.
  */
{DIGIT}+  {
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
<TOO_LONG_STRING>\\\n curr_lineno++;
<STRING>[^\\"\n]+ {
  if (StrWillRunOutOfRange(yyleng)) {
    BEGIN(TOO_LONG_STRING);
    break;
  }
  strncpy(string_buf_ptr, yytext, yyleng);
  string_buf_ptr += yyleng;
}
<TOO_LONG_STRING>[^\\"\n]+  ;
<STRING>\"  {
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
<STRING,TOO_LONG_STRING>\n  {
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
