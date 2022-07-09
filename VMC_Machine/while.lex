
structure Tokens = Tokens

fun toInt s = valOf (Int.fromString s);
type pos = int;
type svalue = Tokens.svalue;
type ('a, 'b) token = ('a, 'b) Tokens.token;
type lexresult = (svalue, pos) token;
val pos = ref 1;
val col = ref 1;
type lexarg = string;
type arg = lexarg;
val eof = fn fileName => let val x = HashTable.clear SymbolTable.htable;val l = !pos; val r = !col in (SymbolTable.index:=1;pos:=1;col:=1;Tokens.EOF(l,r)) end;
val error = fn(str, pos, col) => TextIO.output(TextIO.stdOut, "Check token "^(str)^ " at line "^ Int.toString(pos)^"\n");

%%
%header (functor WHILELexFun(structure Tokens: WHILEEXP_TOKENS));
%full
%count
digit = [0-9];
letter = [A-Za-z];
ws = [\ \t];

%arg (fileName: string);

%%
\n => (pos:=(!pos)+1;col:=1; continue());
{ws}+     => (col:=(!(col)+ size yytext);continue());
"program" => (col:=(!(col)+ size yytext);Tokens.PROG(!pos,!col));
"::"      => (col:=(!(col)+ size yytext);Tokens.DOUBLECOLON(!pos,!col));
"var"     => (col:=(!(col)+ size yytext);Tokens.VAR(!pos,!col));
"int"     => (col:=(!(col)+ size yytext);Tokens.INT(!pos,!col));
"bool"    => (col:=(!(col)+ size yytext);Tokens.BOOL(!pos,!col));
","   => (col:=(!(col)+ size yytext);Tokens.COMMA(!pos,!col));
"{"   => (col:=(!(col)+ size yytext);Tokens.LBRACE(!pos,!col));
"}"   => (col:=(!(col)+ size yytext);Tokens.RBRACE(!pos,!col));
";"   => (col:=(!(col)+ size yytext);Tokens.SEQ(!pos,!col));
":="      => (col:=(!(col)+ size yytext);Tokens.SET(!pos,!col));
"read"    => (col:=(!(col)+ size yytext);Tokens.READ(!pos,!col));
"write"   => (col:=(!(col)+ size yytext);Tokens.WRITE(!pos,!col));
"if"      => (col:=(!(col)+ size yytext);Tokens.IF(!pos,!col));
"then"    => (col:=(!(col)+ size yytext);Tokens.THEN(!pos,!col));
"else"    => (col:=(!(col)+ size yytext);Tokens.ELSE(!pos,!col));
"endif"   => (col:=(!(col)+ size yytext);Tokens.ENDIF(!pos,!col));
"while"   => (col:=(!(col)+ size yytext);Tokens.WHILE(!pos,!col));
"do"      => (col:=(!(col)+ size yytext);Tokens.DO(!pos,!col));
"endwh"   => (col:=(!(col)+ size yytext);Tokens.ENDWH(!pos,!col));
"("   => (col:=(!(col)+ size yytext);Tokens.LPAREN(!pos,!col));
")"   => (col:=(!(col)+ size yytext);Tokens.RPAREN(!pos,!col));
"||"      => (col:=(!(col)+ size yytext);Tokens.OR(!pos,!col));
"&&"      => (col:=(!(col)+ size yytext);Tokens.AND(!pos,!col));
"tt"      => (col:=(!(col)+ size yytext);Tokens.TT(!pos,!col));
"ff"      => (col:=(!(col)+ size yytext);Tokens.FF(!pos,!col));
"!"   => (col:=(!(col)+ size yytext);Tokens.NOT(!pos,!col));
"<"   => (col:=(!(col)+ size yytext);Tokens.LT(!pos,!col));
"<="      => (col:=(!(col)+ size yytext);Tokens.LEQ(!pos,!col));
"="   => (col:=(!(col)+ size yytext);Tokens.EQ(!pos,!col));
">"   => (col:=(!(col)+ size yytext);Tokens.GT(!pos,!col));
">="      => (col:=(!(col)+ size yytext);Tokens.GEQ(!pos,!col));
"<>"      => (col:=(!(col)+ size yytext);Tokens.NEQ(!pos,!col));
"+"   => (col:=(!(col)+ size yytext);Tokens.PLUS(!pos,!col));
"-"   => (col:=(!(col)+ size yytext);Tokens.MINUS(!pos,!col));
"*"   => (col:=(!(col)+ size yytext);Tokens.TIMES(!pos,!col));
"/"   => (col:=(!(col)+ size yytext);Tokens.DIV(!pos,!col));
"%"   => (col:=(!(col)+ size yytext);Tokens.MOD(!pos,!col));
":"   => (col:=(!(col)+ size yytext);Tokens.COLON(!pos,!col));
"~"	  => (col:=(!(col)+ size yytext);Tokens.TILDE(!pos,!col));
{digit}+ => (col:=(!(col)+ size yytext);Tokens.NUMERAL(toInt yytext,!pos,!col));
{letter} ({letter}|{digit})*    => (col:=(!(col)+ size yytext);Tokens.IDENTIFIER (yytext,!pos,!col));
. 	  => (error(yytext, !pos, !col); col := !col + size yytext ;continue());
