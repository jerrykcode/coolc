/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>
#include <string>
using std::string;

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

int last;

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>
ASSIGN          <-

/* key words */
IF              (if)
THEN            (then)
ELSE            (else)
FI              (FI)
WHILE           (while)
LOOP            (loop)
POOL            (pool)
LET             (let)
IN              (in)
CASE            (case)
ESAC            (esac)
OF              (of)
LE              (le)
NOT             (not)
ISVOID          (void)

CLASS           (class)
INHERITS        (inherits)

DIGIT           [0-9]


/* const */
STR_CONST       ["]([^"\n]|[\\\n])*["]
UNTERM_STR      ["]([^"\n]|[\\\n])*
INT_CONST       {DIGIT}+
BOOL_TRUE       (true)
BOOL_FALSE      (false)
BOOL_CONST      {BOOL_TRUE}|{BOOL_FALSE}


/* TYPEID name of a class */
LOWER_LETTER    [a-z]
UPPER_LETTER    [A-Z]
LETTER          [a-zA-Z]

SELF_TYPE       (SELF_TYPE)
TYPEID          {UPPER_LETTER}({LETTER}|{DIGIT})*
OBJECTID        {LETTER}({LETTER}|{DIGIT}|_)*

/* comments */
COMMENT        [(][*][^*]*[*]+([^*)][^*]*[*]+)*[)]
UNTERM_COMMENT [(][*] 

/* single characters */
SINGLE_CHAR    [+]|[/]|[-]|[*]|[=]|[<]|[.]|[~]|[,]|[;]|[:]|[(]|[)]|[@]|[{]|[}]

/* empty space */
EMPTY_SPACE    [ ]|[\t]|[\n]

%%

 /*
  *  Nested comments
  */
 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (last = DARROW); }
{ASSIGN}        { return (last = ASSIGN); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{IF}            { return (last = IF); }
{THEN}          { return (last = THEN); }
{ELSE}          { return (last = ELSE); }
{FI}            { return (last = FI); }
{WHILE}         { return (last = WHILE); }
{LOOP}          { return (last = LOOP); }
{POOL}          { return (last = POOL); }
{LET}           { return (last = LET); }
{IN}            { return (last = IN); }
{CASE}          { return (last = CASE); }
{ESAC}          { return (last = ESAC); }
{OF}            { return (last = OF); }
{LE}            { return (last = LE); }
{NOT}           { return (last = NOT); }
{ISVOID}        { return (last = ISVOID); }

{STR_CONST}     {
                    string strtext = "";
                    unsigned int len = strlen(yytext);
                    for (unsigned int i = 1; i < len - 1; i++) {//i in [1, len - 2]
                        if (yytext[i] == '\\') continue;
                        if (yytext[i] == '\n') continue;
                        strtext += yytext[i];
                    }
                    char * ctext = new char[strtext.length()];
                    strcpy(ctext, strtext.c_str());
                    cool_yylval.symbol = new Entry(ctext, yyleng, 0);
                    free(ctext);
                    return (last = STR_CONST);
                }
{UNTERM_STR}    { fatal_error("Unterminated string"); }


{INT_CONST}     { 

                    cool_yylval.symbol = new Entry(yytext, yyleng, 0);
                    return (last = INT_CONST); 
                }


{BOOL_CONST}    {
                    cool_yylval.symbol = new Entry(yytext, yyleng, 0);
                    return (last = BOOL_CONST);
                }


{CLASS}         { return (last = CLASS); }
{INHERITS}      { return (last = INHERITS); }

{TYPEID}        { 
                        cool_yylval.symbol = new Entry(yytext, yyleng, 0); 
                        return (last = TYPEID); 
                }
{SELF_TYPE}     {
                    cool_yylval.symbol = new Entry(yytext, yyleng, 0);
                    return (last = TYPEID);
                }


{OBJECTID}      {
                    cool_yylval.symbol = new Entry(yytext, yyleng, 0);
                    return (last = OBJECTID);
                }


{COMMENT}        { }
{UNTERM_COMMENT} { fatal_error("Unterminated comment"); }

{SINGLE_CHAR}    { 
                     char c = yytext[0];
                     return (last = c);
                 }

{EMPTY_SPACE}    { }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */


%%
