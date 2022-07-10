/*
*  cool.y
*              Parser definition for the COOL language.
*
*/
%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
  for the location of tokens */
    
    extern int node_lineno;          /* set before constructing a tree node
    to whatever you want the line number
    for the tree node to be */
      
      
      #define YYLLOC_DEFAULT(Current, Rhs, N)         \
      Current = Rhs[1];                             \
      node_lineno = Current;
    
    
    #define SET_NODELOC(Current)  \
    node_lineno = Current;
    
    /* IMPORTANT NOTE ON LINE NUMBERS
    *********************************
    * The above definitions and macros cause every terminal in your grammar to 
    * have the line number supplied by the lexer. The only task you have to
    * implement for line numbers to work correctly, is to use SET_NODELOC()
    * before constructing any constructs from non-terminals in your grammar.
    * Example: Consider you are matching on the following very restrictive 
    * (fictional) construct that matches a plus between two integer constants. 
    * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
    
    plus_consts	: INT_CONST '+' INT_CONST 
    
    * where INT_CONST is a terminal for an integer constant. Now, a correct
    * action for this rule that attaches the correct line number to plus_const
    * would look like the following:
    
    plus_consts	: INT_CONST '+' INT_CONST 
    {
      // Set the line number of the current non-terminal:
      // ***********************************************
      // You can access the line numbers of the i'th item with @i, just
      // like you acess the value of the i'th exporession with $i.
      //
      // Here, we choose the line number of the last INT_CONST (@3) as the
      // line number of the resulting expression (@$). You are free to pick
      // any reasonable line as the line number of non-terminals. If you 
      // omit the statement @$=..., bison has default rules for deciding which 
      // line number to use. Check the manual for details if you are interested.
      @$ = @3;
      
      
      // Observe that we call SET_NODELOC(@3); this will set the global variable
      // node_lineno to @3. Since the constructor call "plus" uses the value of 
      // this global, the plus node will now have the correct line number.
      SET_NODELOC(@3);
      
      // construct the result node:
      $$ = plus(int_const($1), int_const($3));
    }
    
    */
    
    
    
    void yyerror(char *s);        /*  defined below; called for each parse error */
    extern int yylex();           /*  the entry point to the lexer  */
    
    /************************************************************************/
    /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
    
    Program ast_root;	      /* the result of the parse  */
    Classes parse_results;        /* for use in semantic analysis */
    int omerrs = 0;               /* number of errors in lexing and parsing */
    %}
    
    /* A union of all the types that can be the result of parsing actions. */
    %union {
      Boolean boolean;
      Symbol symbol;
      Program program;
      Class_ class_;
      Classes classes;
      Feature feature;
      Features features;
      Formal formal;
      Formals formals;
      Case case_;
      Cases cases;
      Expression expression;
      Expressions expressions;
      char *error_msg;
    }
    
    /* 
    Declare the terminals; a few have types for associated lexemes.
    The token ERROR is never used in the parser; thus, it is a parse
    error when the lexer returns it.
    
    The integer following token declaration is the numeric constant used
    to represent that token internally.  Typically, Bison generates these
    on its own, but we give explicit numbers to prevent version parity
    problems (bison 1.25 and earlier start at 258, later versions -- at
    257)
    */
    %token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
    %token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
    %token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
    %token <symbol>  STR_CONST 275 INT_CONST 276 
    %token <boolean> BOOL_CONST 277
    %token <symbol>  TYPEID 278 OBJECTID 279 
    %token ASSIGN 280 NOT 281 LE 282 ERROR 283
    
    /*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
    /**************************************************************************/

    %left '=' '<'  LE
    %left '+' '-'
    %left '*' '/'
    
    /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */
    
    /* Declare types for the grammar's non-terminals. */
    %type <program> program
    %type <classes> class_list
    %type <class_> class
    
    %type <features> dummy_feature_list
    %type <feature> feature
    %type <feature> method
    %type <feature> attr
    
    %type <formals> formals
    %type <formals> formals_without_last
    %type <formal> formal
    
    %type <expressions> expressions
    %type <expressions> block_expressions
    %type <expressions> dispatch_expressions

    %type <expression> expression
    %type <expression> no_expr
    %type <expression> assign
    %type <expression> dispatch
    %type <expression> static_dispatch
    %type <expression> cond
    %type <expression> loop
    %type <expression> block
    %type <expression> let
    %type <expression> next_let
    %type <expression> typcase
    %type <expression> lt
    %type <expression> eq
    %type <expression> leq
    %type <expression> plus
    %type <expression> sub
    %type <expression> mul
    %type <expression> divide
    %type <expression> neg
    %type <expression> int_const
    %type <expression> bool_const
    %type <expression> str_const
    %type <expression> new
    %type <expression> isvoid
    %type <expression> object
    %type <expression> comp 

    %type <cases> cases
    %type <case_> branch
    
    /* Precedence declarations go here. */
    
    
    %%
    /* 
    Save the root of the abstract syntax tree in a global variable.
    */
    program	: class_list	{ @$ = @1; ast_root = program($1); }
    ;
    
    class_list
    : class			/* single class */
    { $$ = single_Classes($1);
    parse_results = $$; }
    | class_list class	/* several classes */
    { $$ = append_Classes($1,single_Classes($2)); 
    parse_results = $$; }
    ;
    
    /* If no parent is specified, the class inherits from the Object class. */
    class	: CLASS TYPEID '{' dummy_feature_list '}' ';'
    { $$ = class_($2,idtable.add_string("Object"),$4,
    stringtable.add_string(curr_filename)); }
    | CLASS TYPEID INHERITS TYPEID '{' dummy_feature_list '}' ';'
    { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
    | CLASS error '{'dummy_feature_list '}' ';'
    | CLASS error INHERITS TYPEID '{' dummy_feature_list '}' ';'
    ;
    
    /* Feature list may be empty, but no empty features in list. */
    dummy_feature_list:		/* empty */
    { $$ = nil_Features(); }
    | feature               /* single feature */
    { $$ = single_Features($1); }
    | dummy_feature_list feature
    { $$ = append_Features($1, single_Features($2)); }
    ;

    feature: method
    { $$ = $1; }
    | attr
    { $$ = $1; }
    ;

    method: OBJECTID '('formals')' ':' TYPEID '{' expression '}' ';'
    { $$ = method($1, $3, $6, $8); }
    ;

    attr: OBJECTID ':' TYPEID ';'
    { $$ = attr($1, $3, no_expr()); }
    | OBJECTID ':' TYPEID ASSIGN expression ';'
    { $$ = attr($1, $3, $5); }
    ;
   
    formals:        /* empty */
    { $$ = nil_Formals(); }
    | formal
    { $$ = single_Formals($1); }
    | formals_without_last ',' formal
    { $$ = append_Formals($1, single_Formals($3)); }
    ;
    formals_without_last: formal
    { $$ = single_Formals($1); }
    | formals_without_last ',' formal
    { $$ = append_Formals($1, single_Formals($3)); }
    ;

    formal: OBJECTID ':' TYPEID
    { $$ = formal($1, $3); }
    ;

    expressions:    /* empty */
    { $$ = nil_Expressions(); }
    | block_expressions /* expressions for block */
    { $$ = $1; }
    | dispatch_expressions   /* expressions for dispatch(arguments when calling a method) */
    { $$ = $1; }
    ;

    block_expressions: expression ';'
    { $$ = single_Expressions($1); }
    | block_expressions expression ';'
    { $$ = append_Expressions($1, single_Expressions($2)); }
    ;

    dispatch_expressions: expression
    { $$ = single_Expressions($1); }
    | dispatch_expressions ',' expression
    { $$ = append_Expressions($1, single_Expressions($3)); }
    ;

    expression: no_expr
    { $$ = $1; }
    | assign
    { $$ = $1; }
    | dispatch
    { $$ = $1; }
    | static_dispatch
    { $$ = $1; }
    | cond
    { $$ = $1; }
    | loop
    { $$ = $1; }
    | block
    { $$ = $1; }
    | let
    { $$ = $1; }
    | typcase
    { $$ = $1; }
    | plus
    { $$ = $1; }
    | sub
    { $$ = $1; }
    | mul
    { $$ = $1; }
    | divide
    { $$ = $1; }
    | neg
    { $$ = $1; }
    | lt
    { $$ = $1; }
    | eq
    { $$ = $1; }
    | leq
    { $$ = $1; }
    | int_const
    { $$ = $1; }
    | bool_const
    { $$ = $1; }
    | str_const
    { $$ = $1; }
    | new
    { $$ = $1; }
    | isvoid
    { $$ = $1; }
    | object
    { $$ = $1; }
    | comp 
    { $$ = $1; }
    | '(' expression ')'   /* parentheses */
    { $$ = $2; }
    ;

    no_expr:    /* no expression, empty */
    { $$ = no_expr(); }
    ;

    assign: OBJECTID ASSIGN expression
    { $$ = assign($1, $3); }
    ;

    dispatch: expression '.' OBJECTID '(' expressions ')'
    { $$ = dispatch($1, $3, $5); }
    | OBJECTID '(' expressions ')'
    { int len = strlen("self");
      char *SELF = (char *)malloc((len + 1)*sizeof(char));
      strcpy(SELF, "self"); SELF[len] = '\0';
      $$ = dispatch(object(new Entry(SELF, len, OBJECTID)), $1, $3); }
    ;

    static_dispatch: expression '@' TYPEID '.'OBJECTID '(' expressions ')'
    { $$ = static_dispatch($1, $3, $5, $7); }
    ;

    cond: IF expression THEN expression FI
    { $$ = cond($2, $4, no_expr()); }
    | IF expression THEN expression ELSE expression FI
    { $$ = cond($2, $4, $6); }
    ;

    loop: WHILE expression LOOP expression POOL
    { $$ = loop($2, $4); }
    ;

    block: '{' expressions '}'
    { $$ = block($2); }
    ;

    let: LET next_let
    { $$ = $2; }
    ;

    next_let: OBJECTID ':' TYPEID ASSIGN expression IN expression
    { $$ = let($1, $3, $5, $7); }
    | OBJECTID ':' TYPEID IN expression
    { $$ = let($1, $3, no_expr(), $5); }
    | OBJECTID ':' TYPEID ASSIGN expression ',' next_let
    { $$ = let($1, $3, $5, $7); }
    | OBJECTID ':' TYPEID ',' next_let
    { $$ = let($1, $3, no_expr(), $5); }
    ;

    typcase: CASE expression OF cases ESAC
    { $$ = typcase($2, $4); }
    ;

    cases: branch
    { $$ = single_Cases($1); }
    | cases branch
    { $$ = append_Cases($1, single_Cases($2)); }
    ;

    branch: OBJECTID ':' TYPEID DARROW expression ';'
    { $$ = branch($1, $3, $5); }
    ;

    lt: expression '<' expression
    { $$ = lt($1, $3); }
    ;

    eq: expression '=' expression
    { $$ = eq($1, $3); }
    ;

    leq: expression LE expression
    { $$ = leq($1, $3); }
    ;

    plus: expression '+' expression
    { $$ = plus($1, $3); }
    ;

    sub: expression '-' expression
    { $$ = sub($1, $3); }
    ;

    mul: expression '*' expression
    { $$ = mul($1, $3); }
    ;

    divide: expression '/' expression
    { $$ = divide($1, $3); }
    ;

    neg: '~' expression
    { $$ = neg($2); }
    ;

    int_const: INT_CONST
    { $$ = int_const($1); }
    ;

    bool_const: BOOL_CONST
    { $$ = bool_const($1); }

    str_const: STR_CONST
    { $$ = string_const($1); }
    ; 

    new: NEW TYPEID
    { $$ = new_($2); }
    ;

    isvoid: ISVOID expression
    { $$ = isvoid($2); }
    ;

    object: OBJECTID
    { $$ = object($1); }
    ;

    comp: NOT expression
    { $$ = comp($2); }
    ;

    /* end of grammar */
    %%
    
    /* This function is called automatically when Bison detects a parse error. */
    void yyerror(char *s)
    {
      extern int curr_lineno;
      
      cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
      << s << " at or near ";
      print_cool_token(yychar);
      cerr << endl;
      omerrs++;
      
      if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
    }
    
    
