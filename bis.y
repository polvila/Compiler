
%{
    
    /********************
     Declaraciones en C
     **********************/
    
    #include "functions.h"
    extern int yylex(void);
    extern char *yytext;
    extern FILE *yyin;
    extern int yylineno;
    void yyerror(char *s);
    int count = 0;
    int line = 1;
    int isCalcOff;
    char ** taula;
    extern char *strdup(const char *s);
    FILE *fLog;
    FILE *fp;
    
    // declare a vector
    Vector vector;
    
%}

%union{
    char *ident;
    int enter;
    char *cadena;
    double real;
    struct structTipusValor tipusValor;
}


%token PLUS
%token MINUS
%token TIMES
%token POW
%token SLASH
%token MOD
%token LPAREN
%token RPAREN
%token SEMICOLON
%token COMMA
%token PERIOD
%token BECOMES
%token EQL
%token NEQ
%token LSS
%token GTR
%token LEQ
%token GEQ

%token CALCON
%token CALCOFF

%token IF
%token THEN
%token ELSE
%token FI
%token ELSIF
%token WHILE
%token DO
%token DONE
%token REPEAT
%token UNTIL
%token FOR
%token IN
%token POINTS

%token <ident> ID
%token <enter> TOK_ENTER
%token <cadena> TOK_STRING
%token <real> TOK_REAL
%token TOK_TRUE;
%token TOK_FALSE;

%token TOK_AND;
%token TOK_OR;
%token TOK_NOT;

%token NL

%type <tipusValor> exp
%type <tipusValor> M
%type <tipusValor> N
%type <tipusValor> T
%type <tipusValor> Q
%type <tipusValor> R
%type <tipusValor> C
%type <tipusValor> P
%type <tipusValor> stmtseq
%type <tipusValor> statement
%type <tipusValor> expRel
%type <tipusValor> expBool
%type <tipusValor> expBoolNot
%type <tipusValor> expBoolAnd
%type <tipusValor> boolean
%type <tipusValor> lineaP
%type <tipusValor> elsif

%%

mode: calcOn linea
| calcOff lineaP {
    char str[100];
    sprintf(str, "%d: HALT", line);
    line++;
    vector_append(&vector, str);
}
;

lineaP:            {}
| lineaP statement { completa(&vector, $2.lls, line); }
;

calcOn: CALCON  { isCalcOff = 0; }  //false
;
calcOff: CALCOFF { isCalcOff = 1; }  //true
;

statement:  ID BECOMES expBool NL {
    switch($3.tipus){
        case enter: //enter
        printf("%s is an int\n", $1);
        break;
        case real: //real
        printf("%s is a real\n", $1);
        break;
        case cadena: //string
        printf("%s is a string\n", $1);
        break;
        case boolea:
        if($3.valBoolea == 0){ //false
            printf("%s is a bool\n", $1);
        }else{      //true
            printf("%s is a bool\n", $1);
        }
        break;
    }
    afegir_variable($1,&$3);
    
    struct structTipusValor tv;
    if(agafar_variable($1, &tv)!=SYMTAB_NOT_FOUND){
        
        if($3.tipus == enter || $3.tipus == real || $3.tipus == cadena){
            char str[250];
            sprintf(str, "%d: %s := %s\n", line, $1, $3.lloc);
            line++;
            vector_append(&vector, str);
        }else{ // $3.tipus == boolea
            char str[250];
            sprintf(str, "%d: %s := true\n", line, $1);
            completa(&vector, $3.llc, line);
            line++;
            vector_append(&vector, str);
            int line2 = line + 2;
            sprintf(str, "%d: GOTO %d\n", line, line2);
            line++;
            vector_append(&vector, str);
            sprintf(str, "%d: %s := false\n", line, $1);
            completa(&vector, $3.llf, line);
            line++;
            vector_append(&vector, str);
        }
        
    }else{
        yyerror("Lexical error: Id don't inicialized");
        exit(-1);
    }
    
}
| expBool NL          {
    switch($1.tipus){
        case enter: //enter
        printf("Value of type int\n");
        break;
        case real: //real
        printf("Value of type real\n");
        break;
        case cadena: //string
        printf("Value of type cadena\n");
        break;
        case boolea:
        if($1.valBoolea == 0){ //false
            printf("Value of type boolea\n");
        }else{      //true
            printf("Value of type boolea\n");
        }
        break;
    }
    if($1.tipus == boolea){
        completa(&vector, $1.llc, line);
        char str[250];
        sprintf(str, "%d: PARAM true\n", line);
        line++;
        vector_append(&vector, str);
        sprintf(str, "%d: CALL PUT, 1\n", line);
        line++;
        vector_append(&vector, str);
        sprintf(str, "%d: GOTO %d\n", line, line+3);
        line++;
        vector_append(&vector, str);
        completa(&vector, $1.llf, line);
        sprintf(str, "%d: PARAM false\n", line);
        line++;
        vector_append(&vector, str);
        sprintf(str, "%d: CALL PUT, 1\n", line);
        line++;
        vector_append(&vector, str);
    }else{
        char* t = incr_count(&count);
        char str[250];
        sprintf(str, "%d: %s = %s\n", line, t, $1.lloc);
        line++;
        vector_append(&vector, str);
        sprintf(str, "%d: PARAM %s\n", line, t);
        line++;
        vector_append(&vector, str);
        sprintf(str, "%d: CALL PUT, 1\n", line);
        line++;
        vector_append(&vector, str);
    }
    
    
}
| IF LPAREN expBool RPAREN THEN M NL stmtseq FI NL {
    if($3.tipus == boolea){
        completa(&vector, $3.llc, $6.quad);
        $$.lls = mergeLists($3.llf, $8.lls);
    }else{
        yyerror("Semantic error: It needs a boolean expression");
        exit(-1);
    }
}
| IF LPAREN expBool RPAREN THEN M NL stmtseq ELSE N M stmtseq FI NL {
    if($3.tipus == boolea){
        completa(&vector, $3.llc, $6.quad);
        completa(&vector, $3.llf, $11.quad);
        Node* listAux = mergeLists($12.lls, $10.lls);
        $$.lls = mergeLists($8.lls, listAux);
    }else{
        yyerror("Semantic error: It needs a boolean expression");
        exit(-1);
    }
}
| IF LPAREN expBool RPAREN THEN M NL stmtseq N M elsif M ELSE stmtseq FI NL {
    if($3.tipus == boolea){
        completa(&vector, $3.llc, $6.quad);
        completa(&vector, $3.llf, $10.quad);
        
        Node* listAux = mergeLists($14.lls, $11.lls);
        Node* listAux2 = mergeLists(listAux, $9.lls);
        $$.lls = mergeLists(listAux2, $8.lls);
    }else{
        yyerror("Semantic error: It needs a boolean expression");
        exit(-1);
    }
}
| WHILE M LPAREN expBool RPAREN DO M NL stmtseq DONE NL {
    if($4.tipus == boolea){
        $$.lls = $4.llf;
        completa(&vector, $4.llc, $7.quad);
        completa(&vector, $9.lls, $2.quad);
        char str[250];
        sprintf(str, "%d: GOTO %d\n", line, $2.quad);
        line++;
        vector_append(&vector, str);
    }else{
        yyerror("Semantic error: It needs a boolean expression");
        exit(-1);
    }
}
| REPEAT M NL stmtseq UNTIL M LPAREN expBool RPAREN NL {
    if($8.tipus == boolea){
        $$.lls = $8.llc;
        completa(&vector, $8.llf, $2.quad);
        completa(&vector, $4.lls, $6.quad);
    }else{
        yyerror("Semantic error: It needs a boolean expression");
        exit(-1);
    }
}
| P DO stmtseq DONE NL {
    char str[250];
    completa(&vector, $3.lls, line);
    sprintf(str, "%d: %s := %s ADDI 1\n", line, $1.lloc, $1.lloc);
    line++;
    vector_append(&vector, str);
    sprintf(str, "%d: GOTO %d\n", line, $1.test);
    line++;
    vector_append(&vector, str);
    $$.lls = $1.lls;
    
}
| NL { }
;

P: C POINTS exp RPAREN {
    if($3.tipus == enter){
        char str[250];
        $$.lloc = $1.lloc;
        $$.test = line;
        int line2 = line+2;
        sprintf(str, "%d: IF %s LEI %s GOTO %d\n", line, $1.lloc, $3.lloc, line2);
        line++;
        vector_append(&vector, str);
        $$.lls = creaLlista(line);
        sprintf(str, "%d: GOTO ", line);
        line++;
        vector_append(&vector, str);
    }else{
        yyerror("Semantic error: The ranges have to be formed by integer aritmetic expressions");
        exit(-1);
    }

}

C: FOR LPAREN ID IN exp {
    struct structTipusValor tv;
    if(agafar_variable($3, &tv)==SYMTAB_NOT_FOUND){
        if($5.tipus == enter){
            char str[250];
            sprintf(str, "%d: %s := %s\n", line, $3, $5.lloc);
            line++;
            vector_append(&vector, str);
            $$.lloc = $3;
        }else{
            yyerror("Semantic error: The ranges have to be formed by integer aritmetic expressions");
            exit(-1);
        }
    }else{
        yyerror("Semantic error: The identifier has to be a new identifier, don't previously used");
        exit(-1);
    }
}

elsif: elsif ELSIF LPAREN expBool RPAREN THEN M stmtseq N M {
    completa(&vector, $4.llc, $7.quad);
    completa(&vector, $4.llf, $10.quad);
    Node* listAux = mergeLists($9.lls, $8.lls);
    $$.lls = mergeLists(listAux, $1.lls);
}
| ELSIF LPAREN expBool RPAREN THEN M stmtseq N M {
    completa(&vector, $3.llc, $6.quad);
    completa(&vector, $3.llf, $9.quad);
    $$.lls = mergeLists($8.lls, $7.lls);

}


linea:
| linea sentencia
;

sentencia:  ID BECOMES expBool NL {
    switch($3.tipus){
        case enter: //enter
        printf("%s takes by value %d, an int\n", $1, $3.valEnter);
        break;
        case real: //real
        printf("%s takes by value %f, a real\n", $1, $3.valReal);
        break;
        case cadena: //string
        printf("%s takes by value %s, a string\n", $1, $3.valCadena);
        break;
        case boolea:
        if($3.valBoolea == 0){ //false
            printf("%s takes by value false, a boolean\n", $1);
        }else{      //true
            printf("%s takes by value true, a boolean\n", $1);
        }
        break;
    }
    afegir_variable($1,&$3);
}
| expBool NL          {
    switch($1.tipus){
        case enter: //enter
        printf("The value is: %d, and the type is int\n", $1.valEnter);
        break;
        case real: //real
        printf("The value is: %f, and the type is real\n", $1.valReal);
        break;
        case cadena: //string
        printf("The value is: %s, and the type is string\n", $1.valCadena);
        break;
        case boolea:
        if($1.valBoolea == 0){ //false
            printf("The value is: false, and the type is boolean\n");
        }else{      //true
            printf("The value is: true, and the type is boolean\n");
        }
        break;
    }
    
}
| NL
;

stmtseq: stmtseq M statement {
                        completa(&vector, $1.lls, $2.quad);
                        $$.lls = $3.lls;
}
| statement { $$ = $1; }
;

expBool: expBool TOK_OR M expBoolAnd {
                    if($1.tipus == boolea && $4.tipus == boolea){
                        if($1.valBoolea == 1 || $4.valBoolea == 1){
                            $$.valBoolea = 1;
                        }else if($1.valBoolea == 0 && $4.valBoolea == 0){
                            $$.valBoolea = 0;
                        }
                        $$.tipus = boolea;
                        if(isCalcOff == 1){
                            $$.llc = mergeLists($1.llc, $4.llc);
                            $$.llf = $4.llf;
                            completa(&vector, $1.llf, $3.quad);
                        }
                    }else{
                        yyerror("Semantic error: It can't do OR with aritmetic expressions");
                        exit(-1);
                    }
}
| expBoolAnd {
    $$ = $1;
}

expBoolAnd: expBoolAnd TOK_AND M expBoolNot {
                    if($1.tipus == boolea && $4.tipus == boolea){
                        if($1.valBoolea == 0 || $4.valBoolea == 0){
                            $$.valBoolea = 0;
                        }else if($1.valBoolea == 1 && $4.valBoolea == 1){
                            $$.valBoolea = 1;
                        }
                        $$.tipus = boolea;
                        if(isCalcOff == 1){
                            $$.llc = $4.llc;
                            $$.llf = mergeLists($1.llf, $4.llf);
                            completa(&vector, $1.llc, $3.quad);
                        }
                    }else{
                        yyerror("Semantic error: It can't do AND with aritmetic expressions");
                        exit(-1);
                    }
                }
| expBoolNot {
    $$ = $1;
}

expBoolNot: TOK_NOT boolean {
                    if($2.tipus == boolea){
                        if($2.valBoolea == 0){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                        $$.tipus = boolea;
                        if(isCalcOff == 1){
                            $$.llc = $2.llf;
                            $$.llf = $2.llc;
                        }
                    }else{
                        yyerror("Semantic error: It can't do NOT with aritmetic expressions");
                        exit(-1);
                    }
                }
| boolean {
    $$ = $1;
}

boolean: TOK_TRUE {
    $$.valBoolea = 1;
    $$.tipus = boolea;
    if(isCalcOff == 1){
        $$.llc = creaLlista(line);
        //$$.llf = NULL;
        char str[250];
        sprintf(str, "%d: GOTO ", line);
        line++;
        vector_append(&vector, str);
    }
}
| TOK_FALSE {
    $$.valBoolea = 0;
    $$.tipus = boolea;
    if(isCalcOff == 1){
        //$$.lloc = NULL;
        $$.llf = creaLlista(line);
        char str[250];
        sprintf(str, "%d: GOTO ", line);
        line++;
        vector_append(&vector, str);
    }
}
| expRel {
    $$ = $1;
}

M:   {$$.quad = line;}

N:   {
        $$.lls = creaLlista(line);
        char str[250];
        sprintf(str, "%d: GOTO ", line);
        line++;
        vector_append(&vector, str);
}

expRel: exp EQL exp {
                    if($1.tipus == enter && $3.tipus == enter){
                        if($1.valEnter == $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == real){
                        if($1.valEnter == $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare an int with a string");
                        exit(-1);
                    }else if ($1.tipus == real && $3.tipus == enter){
                        if($1.valReal == $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == real){
                        if($1.valReal == $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare an real with a stringv");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == enter){
                        yyerror("Semantic error: It can't compare a string with an int");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == real){
                        yyerror("Semantic error: It can't compare a string with a real");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == cadena){
                        if(strcmp($1.valCadena,$3.valCadena) == 0){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }
                    if(isCalcOff == 1){
                        $$.llc = creaLlista(line);
                        $$.llf = creaLlista(line+1);
                        char str[250];
                        sprintf(str, "%d: IF %s EQL %s GOTO ", line, $1.lloc, $3.lloc);
                        line++;
                        vector_append(&vector, str);
                        char str2[250];
                        sprintf(str2, "%d: GOTO ", line);
                        line++;
                        vector_append(&vector, str2);
                        
                    }
                    $$.tipus = boolea;
                }
| exp NEQ exp   {
                    if($1.tipus == enter && $3.tipus == enter){
                        if($1.valEnter != $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == real){
                        if($1.valEnter != $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare an int with a string");
                        exit(-1);
                    }else if ($1.tipus == real && $3.tipus == enter){
                        if($1.valReal != $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == real){
                        if($1.valReal != $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare a real with a string");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == enter){
                        yyerror("Semantic error: It can't compare a string with an int");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == real){
                        yyerror("Semantic error: It can't compare a string with a real");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == cadena){
                        if(strcmp($1.valCadena,$3.valCadena) != 0){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }
                    if(isCalcOff == 1){
                        $$.llc = creaLlista(line);
                        $$.llf = creaLlista(line+1);
                        char str[250];
                        sprintf(str, "%d: IF %s NEQ %s GOTO ", line, $1.lloc, $3.lloc);
                        line++;
                        vector_append(&vector, str);
                        sprintf(str, "%d: GOTO ", line);
                        line++;
                        vector_append(&vector, str);
                    }
                    $$.tipus = boolea;
                }
| exp LSS exp   {
                    if($1.tipus == enter && $3.tipus == enter){
                        if($1.valEnter < $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == real){
                        if($1.valEnter < $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare an int with a string");
                        exit(-1);
                    }else if ($1.tipus == real && $3.tipus == enter){
                        if($1.valReal < $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == real){
                        if($1.valReal < $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare a real with a string");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == enter){
                        yyerror("Semantic error: It can't compare a string with an int");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == real){
                        yyerror("Semantic error: It can't compare a string with a real");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare both strings majors or minors");
                        exit(-1);
                    }
                    if(isCalcOff == 1){
                        $$.llc = creaLlista(line);
                        $$.llf = creaLlista(line+1);
                        char str[250];
                        sprintf(str, "%d: IF %s LSS %s GOTO ", line, $1.lloc, $3.lloc);
                        vector_append(&vector, str);
                        line++;
                        sprintf(str, "%d: GOTO ", line);
                        line++;
                        vector_append(&vector, str);
                    }
                    $$.tipus = boolea;
                }
| exp GTR exp   {
                    if($1.tipus == enter && $3.tipus == enter){
                        if($1.valEnter > $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == real){
                        if($1.valEnter > $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare an int with a string");
                        exit(-1);
                    }else if ($1.tipus == real && $3.tipus == enter){
                        if($1.valReal > $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == real){
                        if($1.valReal > $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare a real with a string");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == enter){
                        yyerror("Semantic error: It can't compare a string with an int");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == real){
                        yyerror("Semantic error: It can't compare a string with a real";
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare both strings majors or minors");
                        exit(-1);
                    }
                    if(isCalcOff == 1){
                        $$.llc = creaLlista(line);
                        $$.llf = creaLlista(line+1);
                        char str[250];
                        sprintf(str, "%d: IF %s GTR %s GOTO ", line, $1.lloc, $3.lloc);
                        line++;
                        vector_append(&vector, str);
                        sprintf(str, "%d: GOTO ", line);
                        line++;
                        vector_append(&vector, str);
                    }
                    $$.tipus = boolea;
                }
| exp LEQ exp   {
                    if($1.tipus == enter && $3.tipus == enter){
                        if($1.valEnter <= $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == real){
                        if($1.valEnter <= $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare an int with a string");
                        exit(-1);
                    }else if ($1.tipus == real && $3.tipus == enter){
                        if($1.valReal <= $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == real){
                        if($1.valReal <= $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare a real with a string");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == enter){
                        yyerror("Semantic error: It can't compare a string with an int");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == real){
                        yyerror("Semantic error: It can't compare a string with a real");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare both strings majors or minors");
                        exit(-1);
                    }
                    if(isCalcOff == 1){
                        $$.llc = creaLlista(line);
                        $$.llf = creaLlista(line+1);
                        char str[250];
                        sprintf(str, "%d: IF %s LEQ %s GOTO ", line, $1.lloc, $3.lloc);
                        vector_append(&vector, str);
                        line++;
                        sprintf(str, "%d: GOTO ", line);
                        line++;
                        vector_append(&vector, str);
                    }
                    $$.tipus = boolea;
                }
| exp GEQ exp   {
                    if($1.tipus == enter && $3.tipus == enter){
                        if($1.valEnter >= $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == real){
                        if($1.valEnter >= $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == enter && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare an int with a string");
                        exit(-1);
                    }else if ($1.tipus == real && $3.tipus == enter){
                        if($1.valReal >= $3.valEnter){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == real){
                        if($1.valReal >= $3.valReal){
                            $$.valBoolea = 1;
                        }else{
                            $$.valBoolea = 0;
                        }
                    }else if ($1.tipus == real && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare a real with a string");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == enter){
                        yyerror("Semantic error: It can't compare a string with an int");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == real){
                        yyerror("Semantic error: It can't compare a string with a real");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == cadena){
                        yyerror("Semantic error: It can't compare both strings majors or minors");
                        exit(-1);
                    }
                    if(isCalcOff == 1){
                        $$.llc = creaLlista(line);
                        $$.llf = creaLlista(line+1);
                        char str[250];
                        sprintf(str, "%d: IF %s GEQ %s GOTO ", line, $1.lloc, $3.lloc);
                        line++;
                        vector_append(&vector, str);
                        sprintf(str, "%d: GOTO ", line);
                        line++;
                        vector_append(&vector, str);
                    }
                    $$.tipus = boolea;
                }
| exp           {
                        $$ = $1;
                }


exp: exp PLUS T {   char var1[100], var2[100];
                    sprintf(var1, "%s", $1.lloc);
                    sprintf(var2, "%s", $3.lloc);
    
                    if($1.tipus == enter && $3.tipus == enter){
                        $$.valEnter = $1.valEnter + $3.valEnter;
                        $$.tipus = enter;
                    }else if ($1.tipus == enter && $3.tipus == real){
                        $$.valReal = $1.valEnter + $3.valReal;
                        $$.tipus = real;
                        if(isCalcOff == 1){
                            char* t = incr_count(&count);
                            char str[200];
                            sprintf(str, "%d: %s := I2F %s\n", line, t, var1);
                            line++;
                            vector_append(&vector, str);
                            strcpy(var1, t);
                        }
                    }else if ($1.tipus == enter && $3.tipus == cadena){
                        char *str = (char *)malloc((sizeof(char)*floor(log10(abs($1.valEnter+1)))+4)+(sizeof(char)*strlen($3.valCadena)));
                        sprintf(str, "%d%s", $1.valEnter, $3.valCadena);
                        $$.valCadena = str;
                        $$.tipus = cadena;
                        if(isCalcOff == 1){
                            char* t = incr_count(&count);
                            char str[200];
                            sprintf(str, "%d: %s := I2C %s\n", line, t, var1);
                            line++;
                            vector_append(&vector, str);
                            strcpy(var1, t);
                        }
                    }else if ($1.tipus == real && $3.tipus == enter){
                        $$.valReal = $1.valReal + $3.valEnter;
                        $$.tipus = real;
                        if(isCalcOff == 1){
                            char* t = incr_count(&count);
                            char str[200];
                            sprintf(str, "%d: %s := I2F %s\n", line, t, var2);
                            line++;
                            vector_append(&vector, str);
                            strcpy(var2, t);
                        }
                    }else if ($1.tipus == real && $3.tipus == real){
                        $$.valReal = $1.valReal + $3.valReal;
                        $$.tipus = real;
                    }else if ($1.tipus == real && $3.tipus == cadena){
                        char *str = (char *)malloc((sizeof(char)*sizeof(float))+(sizeof(char)*strlen($3.valCadena)));
                        sprintf(str, "%f%s", $1.valReal, $3.valCadena);
                        $$.valCadena = str;
                        $$.tipus = cadena;
                        if(isCalcOff == 1){
                            yyerror("Semantic error: It can't do adds with reals and strings");
                            exit(-1);
                        }
                    }else if ($1.tipus == cadena && $3.tipus == enter){
                        char *str = (char *)malloc((sizeof(char)*strlen($1.valCadena))+(sizeof(char)*floor(log10(abs($3.valEnter+1)))+4));
                        sprintf(str, "%s%d", $1.valCadena, $3.valEnter);
                        $$.valCadena = str;
                        $$.tipus = cadena;
                        if(isCalcOff == 1){
                            char* t = incr_count(&count);
                            char str[200];
                            sprintf(str, "%d: %s := I2C %s\n", line, t, var2);
                            line++;
                            vector_append(&vector, str);
                            strcpy(var2, t);
                        }
                    }else if ($1.tipus == cadena && $3.tipus == real){
                        char *str = (char *)malloc((sizeof(char)*strlen($1.valCadena))+(sizeof(char)*sizeof(float)));
                        sprintf(str, "%s%f", $1.valCadena, $3.valReal);
                        $$.valCadena = str;
                        $$.tipus = cadena;
                        if(isCalcOff == 1){
                            yyerror("Semantic error: It can't do adds with strings and reals");
                            exit(-1);
                        }
                    }else if ($1.tipus == cadena && $3.tipus == cadena){
                        char *str = (char *)malloc((sizeof(char)*strlen($1.valCadena))+(sizeof(char)*strlen($3.valCadena)));
                        sprintf(str, "%s%s", $1.valCadena, $3.valCadena);
                        $$.valCadena = str;
                        $$.tipus = cadena;
                    }else{
                        yyerror("Semantic error: It can't do adds with booleans");
                        exit(-1);
                    }
                    if(isCalcOff == 1){
                        char* t = incr_count(&count);
                        char dest[250], src[50];
                        sprintf(dest, "%d: %s := %s ", line, t, var1);
                        line++;
                        if($1.tipus == enter && $3.tipus == enter){
                            sprintf(src, "ADDI %s\n", var2);
                        }else if($1.tipus == cadena || $3.tipus == cadena){
                            sprintf(src, "ADDC %s\n", var2);
                        }else{
                            sprintf(src, "ADDF %s\n", var2);
                        }
                        strcat(dest, src);
                        vector_append(&vector, dest);
                        $$.lloc = t;
                    }
                }
| exp MINUS T   {
                    char var1[100], var2[100];
                    sprintf(var1, "%s", $1.lloc);
                    sprintf(var2, "%s", $3.lloc);
                    
                    if($1.tipus == enter && $3.tipus == enter){
                        $$.valEnter = $1.valEnter - $3.valEnter;
                        $$.tipus = enter;
                    }else if ($1.tipus == enter && $3.tipus == real){
                        $$.valReal = $1.valEnter - $3.valReal;
                        $$.tipus = real;
                        if(isCalcOff == 1){
                            char* t = incr_count(&count);
                            char str[200];
                            sprintf(str, "%d: %s := I2F %s\n", line, t, var1);
                            line++;
                            vector_append(&vector, str);
                            strcpy(var1, t);
                        }
                    }else if ($1.tipus == enter && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do rest with ints and strings");
                        exit(-1);
                    }else if ($1.tipus == real && $3.tipus == enter){
                        $$.valReal = $1.valReal - $3.valEnter;
                        $$.tipus = real;
                        if(isCalcOff == 1){
                            char* t = incr_count(&count);
                            char str[200];
                            sprintf(str, "%d: %s := I2F %s\n", line, t, var2);
                            line++;
                            vector_append(&vector, str);
                            strcpy(var2, t);
                        }
                    }else if ($1.tipus == real && $3.tipus == real){
                        $$.valReal = $1.valReal - $3.valReal;
                        $$.tipus = real;
                    }else if ($1.tipus == real && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do rest with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == enter){
                        yyerror("Semantic error: It can't do rest with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == real){
                        yyerror("Semantic error: It can't do rest with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do rest with a strings");
                        exit(-1);
                    }else{
                        yyerror("Semantic error: It can't do rest with a booleans");
                        exit(-1);
                    }
                    if(isCalcOff == 1){
                        char* t = incr_count(&count);
                        char dest[250], src[50];
                        sprintf(dest, "%d: %s := %s ", line, t, var1);
                        line++;
                        if($1.tipus == enter && $3.tipus == enter){
                            strcpy(src, "SUBI");
                        }else{
                            strcpy(src, "SUBF");
                        }
                        strcat(dest, src);
                        sprintf(src, " %s\n", var2);
                        strcat(dest, src);
                        vector_append(&vector, dest);
                        $$.lloc = t;
                    }
                }
| PLUS T        {
                    switch($2.tipus){
                        case enter: //enter
                        $$.valEnter = $2.valEnter;
                        $$.tipus = enter;
                        break;
                        case real: //real
                        $$.valReal = $2.valReal;
                        $$.tipus = real;
                        break;
                        case cadena: //string
                        yyerror("Semantic error: A string can't has a positive value");
                        exit(-1);
                        break;
                        case boolea:
                        yyerror("Semantic error: It can't do positive booleans");
                        exit(-1);
                        break;
                    }
                    if(isCalcOff == 1){
                        char* t = incr_count(&count);
                        char dest[250], src[50];
                        sprintf(dest, "%d: %s := ", line, t);
                        line++;
                        sprintf(src, "%s\n", $2.lloc);
                        strcat(dest, src);
                        vector_append(&vector, dest);
                        $$.lloc = t;
                    }
                }
| MINUS T       {
                    switch($2.tipus){
                        case enter: //enter
                        $$.valEnter = -$2.valEnter;
                        $$.tipus = enter;
                        break;
                        case real: //real
                        $$.valReal = -$2.valReal;
                        $$.tipus = real;
                        break;
                        case cadena: //string
                        yyerror("Semantic error: A string can't has a negative value");
                        exit(-1);
                        break;
                        case boolea:
                        yyerror("Semantic error: It can't do negative booleans");
                        exit(-1);
                        break;
                    }
                    if(isCalcOff == 1){
                        char* t = incr_count(&count);
                        char dest[250], src[50];
                        sprintf(dest, "%d: %s := ", line, t);
                        line++;
                        if($2.tipus == enter){
                            strcpy(src, "CHSI");
                        }else{
                            strcpy(src, "CHSF");
                        }
                        strcat(dest, src);
                        sprintf(src, " %s\n", $2.lloc);
                        strcat(dest, src);
                        vector_append(&vector, dest);
                        $$.lloc = t;
                    }
                }
| T             {
                    $$ = $1;
                }
;



T : T TIMES R   {
                        char var1[100], var2[100];
                        sprintf(var1, "%s", $1.lloc);
                        sprintf(var2, "%s", $3.lloc);
    
                        if($1.tipus == enter && $3.tipus == enter){
                            $$.valEnter = $1.valEnter * $3.valEnter;
                            $$.tipus = enter;
                        }else if ($1.tipus == enter && $3.tipus == real){
                            $$.valReal = $1.valEnter * $3.valReal;
                            $$.tipus = real;
                            if(isCalcOff == 1){
                                char* t = incr_count(&count);
                                char str[200];
                                sprintf(str, "%d: %s := I2F %s\n", line, t, var1);
                                line++;
                                vector_append(&vector, str);
                                strcpy(var1, t);
                            }
                        }else if ($1.tipus == enter && $3.tipus == cadena){
                            yyerror("Semantic error: It can't do mult with a strings");
                            exit(-1);
                        }else if ($1.tipus == real && $3.tipus == enter){
                            $$.valReal = $1.valReal * $3.valEnter;
                            $$.tipus = real;
                            if(isCalcOff == 1){
                                char* t = incr_count(&count);
                                char str[200];
                                sprintf(str, "%d: %s := I2F %s\n", line, t, var2);
                                line++;
                                vector_append(&vector, str);
                                strcpy(var2, t);
                            }
                        }else if ($1.tipus == real && $3.tipus == real){
                            $$.valReal = $1.valReal * $3.valReal;
                            $$.tipus = real;
                        }else if ($1.tipus == real && $3.tipus == cadena){
                            yyerror("Semantic error: It can't do mult with a strings");
                            exit(-1);
                        }else if ($1.tipus == cadena && $3.tipus == enter){
                            yyerror("Semantic error: It can't do mult with a strings");
                            exit(-1);
                        }else if ($1.tipus == cadena && $3.tipus == real){
                            yyerror("Semantic error: It can't do mult with a strings");
                            exit(-1);
                        }else if ($1.tipus == cadena && $3.tipus == cadena){
                            yyerror("Semantic error: It can't do mult with a strings");
                            exit(-1);
                        }else{
                            yyerror("Semantic error: It can't do mult with a booleans");
                            exit(-1);
                        }
                        if(isCalcOff == 1){
                            char* t = incr_count(&count);
                            char dest[250], src[50];
                            sprintf(dest, "%d: %s := %s ", line, t, var1);
                            line++;
                            if($1.tipus == enter && $3.tipus == enter){
                                strcpy(src, "MULI");
                            }else{
                                strcpy(src, "MULF");
                            }
                            strcat(dest, src);
                            sprintf(src, " %s\n", var2);
                            strcat(dest, src);
                            vector_append(&vector, dest);
                            $$.lloc = t;
                        }
                    }
| T SLASH R     {
                    char var1[100], var2[100];
                    sprintf(var1, "%s", $1.lloc);
                    sprintf(var2, "%s", $3.lloc);
    
                    if($1.tipus == enter && $3.tipus == enter){
                        $$.valEnter = (double)$1.valEnter / $3.valEnter;
                        $$.tipus = enter;
                    }else if ($1.tipus == enter && $3.tipus == real){
                        $$.valReal = (double)$1.valEnter / $3.valReal;
                        $$.tipus = real;
                        if(isCalcOff == 1){
                            char* t = incr_count(&count);
                            char str[200];
                            sprintf(str, "%d: %s := I2F %s\n", line, t, var1);
                            line++;
                            vector_append(&vector, str);
                            strcpy(var1, t);
                        }
                    }else if ($1.tipus == enter && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do division with a strings");
                        exit(-1);
                    }else if ($1.tipus == real && $3.tipus == enter){
                        $$.valReal = (double)$1.valReal / $3.valEnter;
                        $$.tipus = real;
                        if(isCalcOff == 1){
                            char* t = incr_count(&count);
                            char str[200];
                            sprintf(str, "%d: %s := I2F %s\n", line, t, var2);
                            line++;
                            vector_append(&vector, str);
                            strcpy(var2, t);
                        }
                    }else if ($1.tipus == real && $3.tipus == real){
                        $$.valReal = $1.valReal / $3.valReal;
                        $$.tipus = real;
                    }else if ($1.tipus == real && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do division with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == enter){
                        yyerror("Semantic error: It can't do division with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == real){
                        yyerror("Semantic error: It can't do division with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do division with a strings");
                        exit(-1);
                    }else{
                        yyerror("Semantic error: It can't do division with a booleans");
                        exit(-1);
                    }
                    if(isCalcOff == 1){
                        char* t = incr_count(&count);
                        char dest[250], src[50];
                        sprintf(dest, "%d: %s := %s ", line, t, var1);
                        line++;
                        if($1.tipus == enter && $3.tipus == enter){
                            strcpy(src, "DIVI");
                        }else{
                            strcpy(src, "DIVF");
                        }
                        strcat(dest, src);
                        sprintf(src, " %s\n", var2);
                        strcat(dest, src);
                        vector_append(&vector, dest);
                        $$.lloc = t;
                    }
                }
| T MOD R       {
                    char var1[100], var2[100];
                    sprintf(var1, "%s", $1.lloc);
                    sprintf(var2, "%s", $3.lloc);
    
                    if($1.tipus == enter && $3.tipus == enter){
                        $$.valEnter = $1.valEnter % $3.valEnter;
                        $$.tipus = enter;
                    }else if ($1.tipus == enter && $3.tipus == real){
                        $$.valReal = fmod($1.valEnter,$3.valReal);
                        $$.tipus = real;
                        if(isCalcOff == 1){
                            char* t = incr_count(&count);
                            char str[200];
                            sprintf(str, "%d: %s := I2F %s\n", line, t, var1);
                            line++;
                            vector_append(&vector, str);
                            strcpy(var1, t);
                        }
                    }else if ($1.tipus == enter && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do mod with a strings");
                        exit(-1);
                    }else if ($1.tipus == real && $3.tipus == enter){
                        $$.valReal = fmod($1.valReal, $3.valEnter);
                        $$.tipus = real;
                        if(isCalcOff == 1){
                            char* t = incr_count(&count);
                            char str[200];
                            sprintf(str, "%d: %s := I2F %s\n", line, t, var2);
                            line++;
                            vector_append(&vector, str);
                            strcpy(var2, t);
                        }
                    }else if ($1.tipus == real && $3.tipus == real){
                        $$.valReal = fmod($1.valReal,$3.valReal);
                        $$.tipus = real;
                    }else if ($1.tipus == real && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do mod with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == enter){
                        yyerror("Semantic error: It can't do mod with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == real){
                        yyerror("Semantic error: It can't do mod with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do mod with a strings");
                        exit(-1);
                    }else{
                        yyerror("Semantic error: It can't do mod with a booleans");
                        exit(-1);
                    }
                    if(isCalcOff == 1){
                        char* t = incr_count(&count);
                        char dest[250], src[50];
                        sprintf(dest, "%d: %s := %s ", line, t, var1);
                        line++;
                        if($1.tipus == enter && $3.tipus == enter){
                            strcpy(src, "MODI");
                        }else{
                            strcpy(src, "MODF");
                        }
                        strcat(dest, src);
                        sprintf(src, " %s\n", var2);
                        strcat(dest, src);
                        vector_append(&vector, dest);
                        $$.lloc = t;
                    }
                }
| R             {
                    $$ = $1;
                }
;

R : R POW Q       {
    
                    if($1.tipus == enter && $3.tipus == enter){
                        $$.valEnter = pow($1.valEnter,$3.valEnter);
                        $$.tipus = enter;
                    }else if ($1.tipus == enter && $3.tipus == real){
                        $$.valReal = pow($1.valEnter,$3.valReal);
                        $$.tipus = real;
                    }else if ($1.tipus == enter && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do pow with a strings");
                        exit(-1);
                    }else if ($1.tipus == real && $3.tipus == enter){
                        $$.valReal = pow($1.valReal, $3.valEnter);
                        $$.tipus = real;
                    }else if ($1.tipus == real && $3.tipus == real){
                        $$.valReal = pow($1.valReal,$3.valReal);
                        $$.tipus = real;
                    }else if ($1.tipus == real && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do pow with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == enter){
                        yyerror("Semantic error: It can't do pow with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == real){
                        yyerror("Semantic error: It can't do pow with a strings");
                        exit(-1);
                    }else if ($1.tipus == cadena && $3.tipus == cadena){
                        yyerror("Semantic error: It can't do pow with a strings");
                        exit(-1);
                    }else{
                        yyerror("Semantic error: It can't do pow with a booleans");
                        exit(-1);
                    }
                    if(isCalcOff == 1){
                        if($3.tipus == real){
                            yyerror("Semantic error: It can't do pow with a reals");
                            exit(-1);
                        }else{
                            char* t = incr_count(&count);
                            char* taux = incr_count(&count);
                            char str[250];
                            sprintf(str, "%d: %s := 1\n", line, t);
                            line++;
                            vector_append(&vector, str);
                            sprintf(str, "%d: %s := 1\n", line, taux);
                            line++;
                            vector_append(&vector, str);
                            sprintf(str, "%d: IF %s LEI %s GOTO %d\n", line, taux, $3.lloc, line+2);
                            line++;
                            vector_append(&vector, str);
                            sprintf(str, "%d: GOTO %d\n", line, line+4);
                            line++;
                            vector_append(&vector, str);
                            if($1.tipus == real){
                                sprintf(str, "%d: %s := %s MULF %s\n", line, t, t, $1.lloc);
                            }else{
                                sprintf(str, "%d: %s := %s MULI %s\n", line, t, t, $1.lloc);
                            }
                            line++;
                            vector_append(&vector, str);
                            sprintf(str, "%d: %s := %s ADDI 1\n", line, taux, taux);
                            line++;
                            vector_append(&vector, str);
                            sprintf(str, "%d: GOTO %d\n", line, line-4);
                            line++;
                            vector_append(&vector, str);
                            $$.lloc = t;
                        }
                        
                    }
                }
| Q             {
                    $$ = $1;
                }
;




Q : LPAREN expBool RPAREN   {
                            $$ = $2;
                        }
| TOK_ENTER     {
                    $$.valEnter = $1;
                    $$.tipus = enter;
                    char *str = (char *)malloc(sizeof(char)*floor(log10(abs($$.valEnter+1)))+4);
                    sprintf(str, "%d", $$.valEnter);
                    $$.lloc = str;
                }
| TOK_REAL      {
                    $$.valReal = $1;
                    $$.tipus = real;
                    char *str = (char *)malloc(sizeof(char)*sizeof(float));
                    sprintf(str, "%f", $$.valReal);
                    $$.lloc = str;
                }
| TOK_STRING    {
                    $$.valCadena = $1;
                    $$.tipus = cadena;
                    $$.lloc = $1;
                }
| ID            {
                    struct structTipusValor tv;
                    if(agafar_variable($1, &tv)!=SYMTAB_NOT_FOUND){
                        $$.tipus = tv.tipus;
                        $$.lloc = $1;
                        switch(tv.tipus){
                            case enter: //enter
                                $$.valEnter = tv.valEnter;
                                break;
                            case real: //real
                                $$.valReal = tv.valReal;
                                break;
                            case cadena: //string
                                $$.valCadena = tv.valCadena;
                                break;
                            case boolea: //boolea
                                $$.valBoolea = tv.valBoolea;
                                break;
                        }
                    }else{
                        yyerror("Lexical error: Id don't initialized");
                        exit(-1);
                    }
                }
;



%%

void yyerror(char *s)
{
    int line = yylineno;
    printf("%s\tLine: %d\n",s, line);
}

int main(int argc,char **argv)
{
    //initialize a new vector
    vector_init(&vector);
    if (argc>1)
    yyin=fopen(argv[1],"rt");
    else
    yyin=stdin;
    fLog = fopen("Log.txt", "w");
    yyparse();
    fp = fopen("Output.txt", "w");
    print_vector(&vector, fp);
    vector_free(&vector);
    fclose(fp);
    fclose(fLog);
    fclose(yyin);
    
    return 0;
}