%option yylineno
%{
    
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <math.h>
    #include "functions.h"
    #include "bis.tab.h"
%}

digit         [0-9]
letter        [a-zA-Z]

%%
"+"                  { return PLUS;       }
"-"                  { return MINUS;      }
"*"                  { return TIMES;      }
"**"                 { return POW;        }
"/"                  { return SLASH;      }
"mod"                { return MOD;        }
"("                  { return LPAREN;     }
")"                  { return RPAREN;     }
";"                  { return SEMICOLON;  }
","                  { return COMMA;      }
"."                  { return PERIOD;     }
":="                 { return BECOMES;    }
"="                  { return EQL;        }
"<>"                 { return NEQ;        }
"<"                  { return LSS;        }
">"                  { return GTR;        }
"<="                 { return LEQ;        }
">="                 { return GEQ;        }


"if"                 { return IF;      }
"then"               { return THEN;    }
"else"               { return ELSE;    }
"fi"                 { return FI;      }
"elsif"              { return ELSIF;   }
"while"              { return WHILE;   }
"do"                 { return DO;      }
"done"               { return DONE;    }
"repeat"             { return REPEAT;  }
"until"              { return UNTIL;   }
"for"                { return FOR;     }
"in"                 { return IN;      }
".."                 { return POINTS;  }


"true"                      {
                                return TOK_TRUE;
                            }

"false"                     {
                                return TOK_FALSE;
                            }

"and"                       {
                                return TOK_AND;
                            }

"or"                        {
                                return TOK_OR;
                            }

"not"                       {
                                return TOK_NOT;
                            }

{letter}({letter}|{digit})* {
                                yylval.cadena = strdup(yytext);
                                return ID;
                                
                            }

{digit}+                    {
                                yylval.enter = atoi(yytext);
                                return TOK_ENTER;
                            }

"\""(\\.|[^\"\""])*"\""     {
                                yytext = strndup(yytext + 1, strlen(yytext) - 2);  //per treure dobles comes dels strings
                                yylval.cadena = strdup(yytext);
                                return TOK_STRING;
                            }

{digit}+"."{digit}+         {
                                yylval.real = atof(yytext);
                                return TOK_REAL;
                            }

\/\/.*                      //comment

\/\*([^*]|(\*+[^*/]))*\*+\/ /* comment */

[\n]                        {
                                return NL;
                            }

<<EOF>>                     {
                                static int once = 0;
                                if(once){ // 0 -> false, !0 -> true
                                    return 0;
                                }else{
                                    once++;
                                    return NL;
                                }
                            }

"calc on"                   {
                                return CALCON;
                            }

"calc off"                  {
                                return CALCOFF;
                            }




[ \t\r]                   /* skip whitespace */

.                           {   printf("Lexical error: %s\tLine: %d\n", yytext, yylineno ); exit(-1);}
%%