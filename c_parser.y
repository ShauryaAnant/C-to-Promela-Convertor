%define parse.error verbose
%debug

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* Global variables and state tracking */
static int indent_level = 0;
static int temp_var_count = 0;
static int channel_count = 0;
static char code_buffer[8192] = "";
static int has_main = 0;
static int has_pointers = 0;

/* Function prototypes */
int yylex(void);
void yyerror(const char *s);
char* create_temp_var();
char* create_channel_name();
void append_code(const char *fmt, ...);
void clear_code_buffer();
char* indent_string(const char *str);
extern int line_num;
extern FILE *yyin, *yyout;

/* Helper macros */
#define EMIT(fmt, ...) fprintf(yyout, fmt, ##__VA_ARGS__)
%}

/* Semantic value union */
%union {
    int num;
    double fnum;
    char *str;
    struct type_info {
        char *type_name;
        int is_pointer;
        int array_size;
    } type;
    struct expr_info {
        char *code;
        char *type;
        int is_lvalue;
    } expr;
}

/* Token declarations */
%token <str> ID
%token <num> NUMBER
%token <fnum> FLOAT
%token <str> STRING CHAR RELOP

/* C keywords */
%token INT VOID SHORT UNSIGNED BOOL
%token IF ELSE SWITCH CASE DEFAULT
%token FOR WHILE DO BREAK CONTINUE RETURN
%token STRUCT TYPEDEF ENUM

/* Operators */
%token PLUS MINUS STAR DIV MOD
%token ASSIGN PLUSEQ MINUSEQ MULEQ DIVEQ MODEQ
%token EQ NEQ LT GT LE GE
%token AND OR NOT
%token BITAND BITOR BITXOR BITNOT
%token INC DEC
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMI COMMA COLON DOT ARROW QMARK
%token FREE MALLOC
%token ATOMIC ASSERT D_STEP
%token DCOLON ARROW
%token SIZEOF



/* Operator precedence (lowest to highest) */
%right ASSIGN PLUSEQ MINUSEQ MULEQ DIVEQ MODEQ
%right QMARK COLON
%left OR
%left AND
%left BITOR
%left BITXOR
%left BITAND
%left EQ NEQ
%left LT GT LE GE
%left PLUS MINUS
%left STAR DIV MOD
%right UNARY
%left INC DEC
%left DOT ARROW LBRACK

/* Nonterminal types */
%type <str> program global_items global_item
%type <type> type_spec
%type <str> func_decl param_list param compound_stmt
%type <str> stmt_list stmt decl_stmt expr_stmt if_stmt
%type <str> switch_stmt case_list case_item default_part
%type <str> while_stmt do_stmt for_stmt
%type <str> break_stmt continue_stmt return_stmt
%type <str> struct_decl struct_body struct_field
%type <str> declarator_list declarator
%type <expr> expr assignment_expr conditional_expr
%type <expr> logical_or_expr logical_and_expr equality_expr
%type <expr> relational_expr additive_expr multiplicative_expr
%type <expr> unary_expr postfix_expr primary_expr
%type <str> arg_list args


%%

/* Top-level program structure */
program:
    {
        /* Emit memory management arrays at start */
        EMIT("/* Memory management for pointers */\n");
        EMIT("typedef node {\n  int next;\n  int value;\n};\n");
        EMIT("node node_mem[9];\n");
        EMIT("int node_valid[9];\n\n");
        has_pointers = 1;
    }
    global_items {
        if(has_main) {
            EMIT("\ninit {\n");
            EMIT("    chan ret_main = [0] of { int };\n");
            EMIT("    run main(ret_main);\n");
            EMIT("    ret_main ? _;\n");
            EMIT("}\n");
        }
        has_pointers += 1;
    }
    ;


global_items:
    /* empty */
    | global_items global_item
    ;

global_item:
    /* Function definition */
    type_spec ID LPAREN param_list RPAREN compound_stmt {
        if (strcmp($2, "main") == 0) {
            has_main = 1;
            EMIT("proctype %s(chan ret_chan) {\n", $2);
        } else {
            EMIT("proctype %s(chan ret_chan; %s) {\n", $2, $4 ? $4 : "");
        }
        EMIT("%s", $6);
        EMIT("end:\n");
        EMIT("  printf(\"End of %s\")\n", $2);
        EMIT("}\n\n");
    }
    | type_spec declarator_list SEMI {
        /* Global variable declaration */
        char *vars = strdup($2);
        char *token = strtok(vars, ",");
        
        while (token) {
            /* Clean up whitespace */
            char *clean_token = token;
            while (*clean_token == ' ' || *clean_token == '\t') clean_token++;
            
            EMIT("%s %s;\n", $1.type_name, clean_token);
            token = strtok(NULL, ",");
        }
        free(vars);
    }
    | struct_decl SEMI {
        EMIT("%s", $1);
    }
    | TYPEDEF struct_decl SEMI {
        EMIT("typedef %s", $2);
    }
    ;

/* Type specifiers */
type_spec:
    INT { 
        $$.type_name = strdup("int"); 
        $$.is_pointer = 0;
        $$.array_size = 0;
    }
    | VOID { 
        $$.type_name = strdup("bit"); 
        $$.is_pointer = 0;
        $$.array_size = 0;
    }
    | CHAR { 
        $$.type_name = strdup("byte"); 
        $$.is_pointer = 0;
        $$.array_size = 0;
    }
    | SHORT { 
        $$.type_name = strdup("short"); 
        $$.is_pointer = 0;
        $$.array_size = 0;
    }
    | UNSIGNED { 
        $$.type_name = strdup("unsigned"); 
        $$.is_pointer = 0;
        $$.array_size = 0;
    }
    | BOOL { 
        $$.type_name = strdup("bool"); 
        $$.is_pointer = 0;
        $$.array_size = 0;
    }
    | STRUCT ID { 
        $$.type_name = strdup($2); 
        $$.is_pointer = 0;
        $$.array_size = 0;
    }
    | type_spec STAR { 
        has_pointers = 1;
        $$.type_name = strdup("int");  /* Pointers are integers in Promela */
        $$.is_pointer = 1;
        $$.array_size = 0;
    }
    ;

/* Structure declarations */
struct_decl:
    STRUCT ID LBRACE struct_body RBRACE {
        char *result = malloc(strlen($2) + strlen($4) + 32);
        if(!has_pointers) sprintf(result, "typedef %s {\n%s}", $2, $4);  /* Remove semicolon after brace */
        $$ = result;
    }
    ;

struct_body:
    /* empty */ { $$ = strdup(""); }
    | struct_body struct_field {
        char *result = malloc(strlen($1) + strlen($2) + 2);
        sprintf(result, "%s%s", $1, $2);
        $$ = result;
    }
    ;

struct_field:
    type_spec declarator_list SEMI {
        char *result = malloc(strlen($1.type_name) + strlen($2) + 8);
        sprintf(result, "  %s %s;\n", $1.type_name, $2);
        $$ = result;
    }
    ;

/* Variable declarators */
declarator_list:
    declarator { $$ = $1; }
    | declarator_list COMMA declarator {
        char *result = malloc(strlen($1) + strlen($3) + 2);
        sprintf(result, "%s,%s", $1, $3);
        $$ = result;
    }
    ;

declarator:
    ID { $$ = strdup($1); }
    | ID LBRACK NUMBER RBRACK {
        char *result = malloc(strlen($1) + 12);
        sprintf(result, "%s[%d]", $1, $3);
        $$ = result;
    }
    | ID ASSIGN expr {
        char *result = malloc(strlen($1) + strlen($3.code) + 4);
        sprintf(result, "%s=%s", $1, $3.code);
        $$ = result;
    }
    ;

/* Function parameters */
param_list:
    /* empty */ { $$ = NULL; }
    | param { $$ = $1; }
    | param_list COMMA param {
        char *result = malloc(strlen($1) + strlen($3) + 3);
        sprintf(result, "%s; %s", $1, $3);
        $$ = result;
    }
    ;

param:
    type_spec ID {
        char *result = malloc(strlen($1.type_name) + strlen($2) + 2);
        sprintf(result, "%s %s", $1.type_name, $2);
        $$ = result;
    }
    ;

/* Compound statement (block) */
compound_stmt:
    LBRACE stmt_list RBRACE {
        char *indented = indent_string($2);
        $$ = indented;
    }
    ;

/* List of statements */
stmt_list:
    /* empty */ { $$ = strdup(""); }
    | stmt_list stmt {
        char *result = malloc(strlen($1) + strlen($2) + 1);
        sprintf(result, "%s%s", $1, $2);
        $$ = result;
    }
    ;

/* Individual statements */
stmt:
    decl_stmt { $$ = $1; }
    | expr_stmt { $$ = $1; }
    | compound_stmt { $$ = $1; }
    | if_stmt { $$ = $1; }
    | switch_stmt { $$ = $1; }
    | while_stmt { $$ = $1; }
    | do_stmt { $$ = $1; }
    | for_stmt { $$ = $1; }
    | break_stmt { $$ = $1; }
    | continue_stmt { $$ = $1; }
    | return_stmt { $$ = $1; }
    | SEMI { $$ = strdup(""); }
    | ATOMIC LBRACE stmt_list RBRACE {
        char *result = malloc(strlen($3) + 20);
        sprintf(result, " atomic {\n%s };\n", $3);
        $$ = result;
    }
    | D_STEP LBRACE stmt_list RBRACE {
        char *result = malloc(strlen($3) + 20);
        sprintf(result, " d_step {\n%s };\n", $3);
        $$ = result;
    }
    ;

/* Declaration statement */
decl_stmt:
    type_spec declarator_list SEMI {
        char *vars = strdup($2);
        char *token = strtok(vars, ",");
        char *result = strdup("");
        char *temp;
        
        while (token) {
            char *clean_token = token;
            while (*clean_token == ' ' || *clean_token == '\t') clean_token++;
            
            char *equals = strchr(clean_token, '=');
            if (equals) {
                *equals = '\0';
                char *var_name = clean_token;
                char *init_value = equals + 1;
                
                /* Trim whitespace */
                while (*var_name == ' ' || *var_name == '\t') var_name++;
                char *end = var_name + strlen(var_name) - 1;
                while (end > var_name && (*end == ' ' || *end == '\t')) end--;
                *(end + 1) = '\0';
                
                /* Create declaration and assignment */
                temp = result;
                result = malloc(strlen(temp) + strlen($1.type_name) + strlen(var_name) + strlen(init_value) + 16);
                sprintf(result, "%s  %s %s;\n  %s = %s;\n", temp, $1.type_name, var_name, var_name, init_value);
                free(temp);
            } else {
                /* Simple declaration */
                temp = result;
                result = malloc(strlen(temp) + strlen($1.type_name) + strlen(clean_token) + 8);
                sprintf(result, "%s  %s %s;\n", temp, $1.type_name, clean_token);
                free(temp);
            }
            
            token = strtok(NULL, ",");
        }
        
        free(vars);
        $$ = result;
    }
    ;

/* Expression statement */
expr_stmt:
    expr SEMI {
        /* Handle normal expressions */
        char *result = malloc(strlen($1.code) + strlen(code_buffer) + 8);
        sprintf(result, "%s  %s;\n", code_buffer, $1.code);
        clear_code_buffer();
        $$ = result;
    }
    | FREE LPAREN expr RPAREN SEMI {
        has_pointers = 1;
        char *result = malloc(strlen($3.code) + 100);
        sprintf(result, " d_step {\n"
                       "   node_valid[%s] = 0;\n"
                       "   node_mem[%s].next = 0;\n"
                       "   node_mem[%s].value = 0\n"
                       " };\n",
                $3.code, $3.code, $3.code);
        $$ = result;
    }
    ;


/* If statement */
if_stmt:
    IF LPAREN expr RPAREN stmt {
        char *result = malloc(strlen($3.code) + strlen($5) + 64);
        sprintf(result, "  if\n  :: (%s) ->\n%s  fi;\n", $3.code, $5);
        $$ = result;
    }
    | IF LPAREN expr RPAREN stmt ELSE stmt {
        char *result = malloc(strlen($3.code) + strlen($5) + strlen($7) + 96);
        sprintf(result, "  if\n  :: (%s) ->\n%s  :: else ->\n%s  fi;\n", $3.code, $5, $7);
        $$ = result;
    }
    ;

/* Switch statement */
switch_stmt:
    SWITCH LPAREN expr RPAREN LBRACE case_list default_part RBRACE {
        char *result = malloc(strlen($3.code) + strlen($6) + strlen($7) + 32);
        sprintf(result, "  /* switch on %s */\n  if\n%s%s  fi;\n", $3.code, $6, $7);
        $$ = result;
    }
    ;

case_list:
    /* empty */ { $$ = strdup(""); }
    | case_list case_item {
        char *result = malloc(strlen($1) + strlen($2) + 1);
        sprintf(result, "%s%s", $1, $2);
        $$ = result;
    }
    ;

case_item:
    CASE NUMBER COLON stmt_list {
        char *indented = indent_string($4);
        char *result = malloc(strlen(indented) + 32);
        sprintf(result, "  :: (switch_expr == %d) ->\n%s", $2, indented);
        free(indented);
        $$ = result;
    }
    ;

default_part:
    /* empty */ { $$ = strdup(""); }
    | DEFAULT COLON stmt_list {
        char *indented = indent_string($3);
        char *result = malloc(strlen(indented) + 16);
        sprintf(result, "  :: else ->\n%s", indented);
        free(indented);
        $$ = result;
    }
    ;

/* While statement */
while_stmt:
    WHILE LPAREN expr RPAREN stmt {
        char *result = malloc(strlen($3.code) + strlen($5) + 64);
        sprintf(result, "  do\n  :: (%s) ->\n%s  :: else -> break;\n  od;\n", $3.code, $5);
        $$ = result;
    }
    ;

/* Do-while statement */
do_stmt:
    DO stmt WHILE LPAREN expr RPAREN SEMI {
        char *result = malloc(strlen($2) + strlen($5.code) + 64);
        sprintf(result, "  do\n  :: true ->\n%s    if\n    :: !(%s) -> break;\n    :: else -> skip;\n    fi;\n  od;\n", $2, $5.code);
        $$ = result;
    }
    ;

/* For statement */
for_stmt:
    FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt {
        /* Format: for(init; cond; incr) body */
        char *result = malloc(strlen($3.code) + strlen($5.code) + strlen($7.code) + strlen($9) + 128);
        sprintf(result, "  /* for loop */\n  %s;\n  do\n  :: (%s) ->\n%s    %s;\n  :: else -> break;\n  od;\n", 
                $3.code, $5.code, $9, $7.code);
        $$ = result;
    }
    | FOR LPAREN decl_stmt expr SEMI expr RPAREN stmt {
        /* Format: for(decl; cond; incr) body */
        char *result = malloc(strlen($3) + strlen($4.code) + strlen($6.code) + strlen($8) + 128);
        sprintf(result, "  /* for loop with declaration */\n%s  do\n  :: (%s) ->\n%s    %s;\n  :: else -> break;\n  od;\n", 
                $3, $4.code, $8, $6.code);
        $$ = result;
    }
    ;

/* Break statement */
break_stmt:
    BREAK SEMI {
        $$ = strdup("  break;\n");
    }
    ;

/* Continue statement */
continue_stmt:
    CONTINUE SEMI {
        $$ = strdup(" continue;\n"); 
    }
    ;

/* Return statement */
return_stmt:
    RETURN expr SEMI {
        char *result = malloc(strlen($2.code) + strlen(code_buffer) + 64);
        sprintf(result, "%s  ret_chan ! %s;\n  goto end;\n", code_buffer, $2.code);
        clear_code_buffer();
        $$ = result;
    }
    | RETURN SEMI {
        $$ = strdup("  ret_chan ! 0;\n  goto end;\n");
    }
    ;

/* Expression handling */
expr:
    assignment_expr {
        $$.code = $1.code;
        $$.type = $1.type;
        $$.is_lvalue = $1.is_lvalue;
    }
    ;

assignment_expr:
    conditional_expr {
        $$.code = $1.code;
        $$.type = $1.type;
        $$.is_lvalue = $1.is_lvalue;
    }
    | unary_expr ASSIGN assignment_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 4);
        sprintf(result, "%s = %s", $1.code, $3.code);
        $$.code = result;
        $$.type = $1.type;
        $$.is_lvalue = 0;
    }
    | unary_expr PLUSEQ assignment_expr {
        char *result = malloc(strlen($1.code) * 2 + strlen($3.code) + 8);
        sprintf(result, "%s = %s + %s", $1.code, $1.code, $3.code);
        $$.code = result;
        $$.type = $1.type;
        $$.is_lvalue = 0;
    }
    | unary_expr MINUSEQ assignment_expr {
        char *result = malloc(strlen($1.code) * 2 + strlen($3.code) + 8);
        sprintf(result, "%s = %s - %s", $1.code, $1.code, $3.code);
        $$.code = result;
        $$.type = $1.type;
        $$.is_lvalue = 0;
    }
    ;

conditional_expr:
    logical_or_expr {
        $$.code = $1.code;
        $$.type = $1.type;
        $$.is_lvalue = $1.is_lvalue;
    }
    | logical_or_expr QMARK expr COLON conditional_expr {
        /* Create a temporary variable for ternary result */
        char *temp = create_temp_var();
        
        /* Generate code for ternary operator */
        char *code = malloc(strlen($1.code) + strlen($3.code) + strlen($5.code) + strlen(temp) + 64);
        sprintf(code, "if\n:: (%s) -> %s = %s;\n:: else -> %s = %s;\nfi", 
                $1.code, temp, $3.code, temp, $5.code);
        
        /* Store the code in the buffer and return the temp var */
        append_code("%s", code);
        free(code);
        
        $$.code = strdup(temp);
        $$.type = $3.type;
        $$.is_lvalue = 0;
    }
    ;

logical_or_expr:
    logical_and_expr {
        $$.code = $1.code;
        $$.type = $1.type;
        $$.is_lvalue = $1.is_lvalue;
    }
    | logical_or_expr OR logical_and_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s || %s", $1.code, $3.code);
        $$.code = result;
        $$.type = strdup("bool");
        $$.is_lvalue = 0;
    }
    ;

logical_and_expr:
    equality_expr {
        $$.code = $1.code;
        $$.type = $1.type;
        $$.is_lvalue = $1.is_lvalue;
    }
    | logical_and_expr AND equality_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s && %s", $1.code, $3.code);
        $$.code = result;
        $$.type = strdup("bool");
        $$.is_lvalue = 0;
    }
    ;

equality_expr:
    relational_expr {
        $$.code = $1.code;
        $$.type = $1.type;
        $$.is_lvalue = $1.is_lvalue;
    }
    | equality_expr EQ relational_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s == %s", $1.code, $3.code);
        $$.code = result;
        $$.type = strdup("bool");
        $$.is_lvalue = 0;
    }
    | equality_expr NEQ relational_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s != %s", $1.code, $3.code);
        $$.code = result;
        $$.type = strdup("bool");
        $$.is_lvalue = 0;
    }
    ;

relational_expr:
    additive_expr {
        $$.code = $1.code;
        $$.type = $1.type;
        $$.is_lvalue = $1.is_lvalue;
    }
    | relational_expr LT additive_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s < %s", $1.code, $3.code);
        $$.code = result;
        $$.type = strdup("bool");
        $$.is_lvalue = 0;
    }
    | relational_expr GT additive_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s > %s", $1.code, $3.code);
        $$.code = result;
        $$.type = strdup("bool");
        $$.is_lvalue = 0;
    }
    | relational_expr LE additive_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s <= %s", $1.code, $3.code);
        $$.code = result;
        $$.type = strdup("bool");
        $$.is_lvalue = 0;
    }
    | relational_expr GE additive_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s >= %s", $1.code, $3.code);
        $$.code = result;
        $$.type = strdup("bool");
        $$.is_lvalue = 0;
    }
    ;

additive_expr:
    multiplicative_expr {
        $$.code = $1.code;
        $$.type = $1.type;
        $$.is_lvalue = $1.is_lvalue;
    }
    | additive_expr PLUS multiplicative_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s + %s", $1.code, $3.code);
        $$.code = result;
        $$.type = $1.type;
        $$.is_lvalue = 0;
    }
    | additive_expr MINUS multiplicative_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s - %s", $1.code, $3.code);
        $$.code = result;
        $$.type = $1.type;
        $$.is_lvalue = 0;
    }
    ;

multiplicative_expr:
    unary_expr {
        $$.code = $1.code;
        $$.type = $1.type;
        $$.is_lvalue = $1.is_lvalue;
    }
    | multiplicative_expr STAR unary_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s * %s", $1.code, $3.code);
        $$.code = result;
        $$.type = $1.type;
        $$.is_lvalue = 0;
    }
    | multiplicative_expr DIV unary_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s / %s", $1.code, $3.code);
        $$.code = result;
        $$.type = $1.type;
        $$.is_lvalue = 0;
    }
    | multiplicative_expr MOD unary_expr {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s %% %s", $1.code, $3.code);
        $$.code = result;
        $$.type = $1.type;
        $$.is_lvalue = 0;
    }
    ;

unary_expr:
    postfix_expr {
        $$.code = $1.code;
        $$.type = $1.type;
        $$.is_lvalue = $1.is_lvalue;
    }
    | INC unary_expr {
        char *result = malloc(strlen($2.code) + 16);
        sprintf(result, "%s = %s + 1", $2.code, $2.code);
        append_code("%s;\n", result);
        $$.code = $2.code;
        $$.type = $2.type;
        $$.is_lvalue = 1;
    }
    | DEC unary_expr {
        char *result = malloc(strlen($2.code) + 16);
        sprintf(result, "%s = %s - 1", $2.code, $2.code);
        append_code("%s;\n", result);
        $$.code = $2.code;
        $$.type = $2.type;
        $$.is_lvalue = 1;
    }
    | PLUS unary_expr {
        $$.code = $2.code;
        $$.type = $2.type;
        $$.is_lvalue = 0;
    }
    | MINUS unary_expr {
        char *result = malloc(strlen($2.code) + 4);
        sprintf(result, "-%s", $2.code);
        $$.code = result;
        $$.type = $2.type;
        $$.is_lvalue = 0;
    }
    | NOT unary_expr {
        char *result = malloc(strlen($2.code) + 4);
        sprintf(result, "!%s", $2.code);
        $$.code = result;
        $$.type = strdup("bool");
        $$.is_lvalue = 0;
    }
    | STAR unary_expr {
        has_pointers = 1;
        char *result = malloc(strlen($2.code) + 20);
        sprintf(result, "node_mem[%s].value", $2.code);
        $$.code = result;
        $$.type = strdup("int");
        $$.is_lvalue = 1;
    }
    ;

postfix_expr:
    primary_expr {
        $$.code = $1.code;
        $$.type = $1.type;
        $$.is_lvalue = $1.is_lvalue;
    }
    | postfix_expr LBRACK expr RBRACK {
        char *result = malloc(strlen($1.code) + strlen($3.code) + 8);
        sprintf(result, "%s[%s]", $1.code, $3.code);
        $$.code = result;
        $$.type = $1.type;
        $$.is_lvalue = 1;
    }
    | postfix_expr INC {
        char *temp = create_temp_var();
        char *inc_code = malloc(strlen($1.code) * 2 + 32);
        sprintf(inc_code, "%s = %s;\n%s = %s + 1", temp, $1.code, $1.code, $1.code);
        append_code("%s;\n", inc_code);
        free(inc_code);
        $$.code = strdup(temp);
        $$.type = $1.type;
        $$.is_lvalue = 0;
    }
    | postfix_expr DEC {
        char *temp = create_temp_var();
        char *dec_code = malloc(strlen($1.code) * 2 + 32);
        sprintf(dec_code, "%s = %s;\n%s = %s - 1", temp, $1.code, $1.code, $1.code);
        append_code("%s;\n", dec_code);
        free(dec_code);
        $$.code = strdup(temp);
        $$.type = $1.type;
        $$.is_lvalue = 0;
    }
    | ID LPAREN arg_list RPAREN {
        /* Function call */
        char *chan = create_channel_name();
        char *temp = create_temp_var();
        
        char *call_code = malloc(strlen($1) + strlen(chan) + strlen($3 ? $3 : "") + 128);
        if ($3 && strlen($3) > 0) {
            sprintf(call_code, "chan %s = [0] of { int };\nrun %s(%s, %s);\n%s %s;\n%s ? %s", 
                    chan, $1, chan, $3, "int", temp, chan, temp);
        } else {
            sprintf(call_code, "chan %s = [0] of { int };\nrun %s(%s);\n%s %s;\n%s ? %s", 
                    chan, $1, chan, "int", temp, chan, temp);
        }
        
        append_code("%s;\n", call_code);
        free(call_code);
        
        $$.code = strdup(temp);
        $$.type = strdup("int");
        $$.is_lvalue = 0;
    }
    | postfix_expr DOT ID {
        char *result = malloc(strlen($1.code) + strlen($3) + 4);
        sprintf(result, "%s.%s", $1.code, $3);
        $$.code = result;
        $$.type = strdup("int");
        $$.is_lvalue = 1;
    }
    | postfix_expr ARROW ID {
        char *result = malloc(strlen($1.code) + strlen($3) + 4);
        sprintf(result, "%s.%s", $1.code, $3);
        $$.code = result;
        $$.type = strdup("int");
        $$.is_lvalue = 1;
    }
    ;

primary_expr:
    ID {
        $$.code = strdup($1);
        $$.type = strdup("int");
        $$.is_lvalue = 1;
    }
    | NUMBER {
        char *num_str = malloc(16);
        sprintf(num_str, "%d", $1);
        $$.code = num_str;
        $$.type = strdup("int");
        $$.is_lvalue = 0;
    }
    | FLOAT {
        char *float_str = malloc(32);
        sprintf(float_str, "%f", $1);
        $$.code = float_str;
        $$.type = strdup("float");
        $$.is_lvalue = 0;
    }
    | STRING {
        $$.code = strdup($1);
        $$.type = strdup("string");
        $$.is_lvalue = 0;
    }
    | CHAR {
        $$.code = strdup($1);
        $$.type = strdup("byte");
        $$.is_lvalue = 0;
    }
    | LPAREN expr RPAREN {
        char *result = malloc(strlen($2.code) + 8);
        sprintf(result, "(%s)", $2.code);
        $$.code = result;
        $$.type = $2.type;
        $$.is_lvalue = $2.is_lvalue;
    }
    | SIZEOF LPAREN type_spec RPAREN {
        $$.code = strdup("1"); // Size is abstracted in Promela
        $$.type = strdup("int");
        $$.is_lvalue = 0;
    }
    | SIZEOF LPAREN STRUCT ID RPAREN {
        $$.code = strdup("1"); // Size is abstracted in Promela
        $$.type = strdup("int");
        $$.is_lvalue = 0;
    }
    | MALLOC LPAREN expr RPAREN {
        has_pointers = 1;
        char *result = malloc(512);
        sprintf(result, "({ int malloc_node_c = 1;\n"
                       "   atomic {\n"
                       "     malloc_node_c = 1;\n"
                       "     do\n"
                       "     :: (malloc_node_c >= 9) -> break\n"
                       "     :: else ->\n"
                       "       if\n"
                       "       :: (node_valid[malloc_node_c] == 0) ->\n"
                       "          node_valid[malloc_node_c] = 1;\n"
                       "          break\n"
                       "       :: else -> malloc_node_c++\n"
                       "       fi\n"
                       "     od;\n"
                       "     assert (malloc_node_c < 9);\n"
                       "     tmp = malloc_node_c\n"
                       "   };\n"
                       "   malloc_node_c })");
        $$.code = result;
        $$.type = strdup("int");
        $$.is_lvalue = 0;
    }
    ;
/* Function call arguments */
arg_list:
    /* empty */ { $$ = NULL; }
    | args { $$ = $1; }
    ;

args:
    expr {
        $$ = strdup($1.code);
    }
    | args COMMA expr {
        char *result = malloc(strlen($1) + strlen($3.code) + 4);
        sprintf(result, "%s, %s", $1, $3.code);
        $$ = result;
    }
    ;

%%

/* Helper functions */
char* create_temp_var() {
    char *temp = malloc(16);
    sprintf(temp, "_t%d", temp_var_count++);
    return temp;
}

char* create_channel_name() {
    char *chan = malloc(16);
    sprintf(chan, "_chan%d", channel_count++);
    return chan;
}

void append_code(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);  // Second argument must be the last named parameter
    char buffer[1024];
    vsnprintf(buffer, sizeof(buffer), fmt, args);
    va_end(args);
    
    strcat(code_buffer, buffer);
}

void clear_code_buffer() {
    code_buffer[0] = '\0';
}

char* indent_string(const char *str) {
    if (!str || !*str) return strdup("");
    
    char *result = malloc(strlen(str) * 2 + 1);
    result[0] = '\0';
    
    char *copy = strdup(str);
    char *line = strtok(copy, "\n");
    
    while (line) {
        if (strlen(line) > 0) {
            strcat(result, "  ");
            strcat(result, line);
        }
        strcat(result, "\n");
        line = strtok(NULL, "\n");
    }
    
    free(copy);
    return result;
}

void yyerror(const char *s) {
    fprintf(stderr, "Error at line %d: %s\n", line_num, s);
}

int main(int argc, char **argv) {
    argc=3;
    if (argc < 2) {
        fprintf(stderr, "Usage: %s input.c [output.pml]\n", argv[0]);
        return 1;
    }
    
    yyin = fopen(argv[1], "r");
    if (!yyin) {
        perror("Cannot open input file");
        return 1;
    }
    
    if (argc >= 3) {
        yyout = fopen(argv[2], "w");
        if (!yyout) {
            perror("Cannot open output file");
            fclose(yyin);
            return 1;
        }
    } else {
        yyout = stdout;
    }
    
    /* Parse the input */
    yyparse();
    
    /* Clean up */
    if (yyin != stdin) fclose(yyin);
    if (yyout != stdout) fclose(yyout);
    
    return 0;
}
