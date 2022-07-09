functor WHILEEXPLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : WHILEEXP_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes;
val lst = ref ["#"];
val temp = ref BOOLEAN;
val temp2 = ref SymbolTable.BOOLEAN;
fun getType(x:string) = case HashTable.find SymbolTable.htable x of SOME (ind,typ) =>  
							if typ = SymbolTable.BOOLEAN then (temp:=BOOLEAN) else (temp:=INTEGER)
							| NONE => (print ("Variable:"^x^" is being used before declaration\n"); HashTable.clear SymbolTable.htable; lst:=[]; raise SymbolTable.IdentifierNotDeclared);



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\036\000\002\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\029\000\031\000\034\000\030\000\042\000\029\000\000\000\
\\001\000\001\000\059\000\000\000\
\\001\000\003\000\080\000\018\000\057\000\019\000\056\000\023\000\055\000\
\\024\000\054\000\025\000\053\000\026\000\052\000\027\000\051\000\
\\028\000\050\000\029\000\049\000\030\000\048\000\031\000\047\000\
\\032\000\046\000\033\000\045\000\000\000\
\\001\000\004\000\010\000\000\000\
\\001\000\004\000\010\000\007\000\009\000\000\000\
\\001\000\005\000\021\000\012\000\020\000\015\000\019\000\040\000\018\000\
\\041\000\017\000\042\000\016\000\000\000\
\\001\000\005\000\026\000\000\000\
\\001\000\006\000\025\000\000\000\
\\001\000\006\000\065\000\000\000\
\\001\000\006\000\066\000\000\000\
\\001\000\008\000\024\000\038\000\023\000\000\000\
\\001\000\009\000\005\000\000\000\
\\001\000\010\000\042\000\011\000\041\000\000\000\
\\001\000\013\000\084\000\000\000\
\\001\000\014\000\086\000\000\000\
\\001\000\016\000\062\000\018\000\057\000\019\000\056\000\023\000\055\000\
\\024\000\054\000\025\000\053\000\026\000\052\000\027\000\051\000\
\\028\000\050\000\029\000\049\000\030\000\048\000\031\000\047\000\
\\032\000\046\000\033\000\045\000\000\000\
\\001\000\017\000\083\000\000\000\
\\001\000\018\000\057\000\019\000\056\000\023\000\055\000\024\000\054\000\
\\025\000\053\000\026\000\052\000\027\000\051\000\028\000\050\000\
\\029\000\049\000\030\000\048\000\031\000\047\000\032\000\046\000\
\\033\000\045\000\039\000\063\000\000\000\
\\001\000\035\000\027\000\000\000\
\\001\000\036\000\000\000\000\000\
\\001\000\037\000\003\000\000\000\
\\001\000\042\000\004\000\000\000\
\\001\000\042\000\013\000\000\000\
\\001\000\042\000\037\000\000\000\
\\001\000\042\000\040\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\007\000\009\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\012\000\020\000\015\000\019\000\040\000\018\000\041\000\017\000\
\\042\000\016\000\000\000\
\\099\000\000\000\
\\100\000\018\000\057\000\019\000\056\000\023\000\055\000\024\000\054\000\
\\025\000\053\000\026\000\052\000\027\000\051\000\028\000\050\000\
\\029\000\049\000\030\000\048\000\031\000\047\000\032\000\046\000\
\\033\000\045\000\000\000\
\\101\000\000\000\
\\102\000\018\000\057\000\019\000\056\000\023\000\055\000\024\000\054\000\
\\025\000\053\000\026\000\052\000\027\000\051\000\028\000\050\000\
\\029\000\049\000\030\000\048\000\031\000\047\000\032\000\046\000\
\\033\000\045\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\031\000\047\000\032\000\046\000\033\000\045\000\000\000\
\\106\000\031\000\047\000\032\000\046\000\033\000\045\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\029\000\049\000\030\000\048\000\031\000\047\000\032\000\046\000\
\\033\000\045\000\000\000\
\\116\000\029\000\049\000\030\000\048\000\031\000\047\000\032\000\046\000\
\\033\000\045\000\000\000\
\\117\000\029\000\049\000\030\000\048\000\031\000\047\000\032\000\046\000\
\\033\000\045\000\000\000\
\\118\000\029\000\049\000\030\000\048\000\031\000\047\000\032\000\046\000\
\\033\000\045\000\000\000\
\\119\000\029\000\049\000\030\000\048\000\031\000\047\000\032\000\046\000\
\\033\000\045\000\000\000\
\\120\000\029\000\049\000\030\000\048\000\031\000\047\000\032\000\046\000\
\\033\000\045\000\000\000\
\\121\000\019\000\056\000\023\000\055\000\024\000\054\000\025\000\053\000\
\\026\000\052\000\027\000\051\000\028\000\050\000\029\000\049\000\
\\030\000\048\000\031\000\047\000\032\000\046\000\033\000\045\000\000\000\
\\122\000\023\000\055\000\024\000\054\000\025\000\053\000\026\000\052\000\
\\027\000\051\000\028\000\050\000\029\000\049\000\030\000\048\000\
\\031\000\047\000\032\000\046\000\033\000\045\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\"
val actionRowNumbers =
"\020\000\021\000\011\000\004\000\
\\026\000\027\000\003\000\022\000\
\\005\000\028\000\025\000\010\000\
\\007\000\006\000\018\000\000\000\
\\023\000\000\000\000\000\033\000\
\\029\000\024\000\012\000\035\000\
\\034\000\000\000\039\000\049\000\
\\000\000\001\000\000\000\051\000\
\\050\000\000\000\047\000\038\000\
\\015\000\017\000\010\000\008\000\
\\009\000\036\000\037\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\062\000\048\000\060\000\002\000\
\\003\000\003\000\032\000\031\000\
\\030\000\044\000\046\000\045\000\
\\043\000\042\000\057\000\056\000\
\\055\000\054\000\053\000\052\000\
\\059\000\058\000\061\000\016\000\
\\013\000\041\000\003\000\014\000\
\\040\000\019\000"
val gotoT =
"\
\\001\000\085\000\000\000\
\\000\000\
\\000\000\
\\002\000\006\000\003\000\005\000\005\000\004\000\000\000\
\\000\000\
\\002\000\009\000\003\000\005\000\000\000\
\\005\000\010\000\000\000\
\\000\000\
\\006\000\013\000\007\000\012\000\000\000\
\\000\000\
\\000\000\
\\004\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\026\000\000\000\
\\000\000\
\\008\000\036\000\000\000\
\\008\000\037\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\041\000\007\000\012\000\000\000\
\\000\000\
\\008\000\042\000\000\000\
\\000\000\
\\000\000\
\\008\000\056\000\000\000\
\\000\000\
\\008\000\058\000\000\000\
\\000\000\
\\000\000\
\\008\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\062\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\065\000\000\000\
\\008\000\066\000\000\000\
\\008\000\067\000\000\000\
\\008\000\068\000\000\000\
\\008\000\069\000\000\000\
\\008\000\070\000\000\000\
\\008\000\071\000\000\000\
\\008\000\072\000\000\000\
\\008\000\073\000\000\000\
\\008\000\074\000\000\000\
\\008\000\075\000\000\000\
\\008\000\076\000\000\000\
\\008\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\079\000\000\000\
\\005\000\080\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\083\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 86
val numrules = 38
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | IDENTIFIER of unit ->  (string) | NUMERAL of unit ->  (int)
 | EXPRESSION of unit ->  (EXP) | COMMAND of unit ->  (CMD)
 | COMMANDLIST of unit ->  (CMD) | COMMANDSEQ of unit ->  (CMD)
 | VARLIST of unit ->  (VARIABLELIST) | DECLARATION of unit ->  (DEC)
 | DECLARATIONSEQ of unit ->  (DEC) | PROGRAM of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 35) => true | _ => false
val showTerminal =
fn (T 0) => "NUMERAL"
  | (T 1) => "LPAREN"
  | (T 2) => "RPAREN"
  | (T 3) => "LBRACE"
  | (T 4) => "RBRACE"
  | (T 5) => "SEQ"
  | (T 6) => "VAR"
  | (T 7) => "COLON"
  | (T 8) => "DOUBLECOLON"
  | (T 9) => "INT"
  | (T 10) => "BOOL"
  | (T 11) => "IF"
  | (T 12) => "ELSE"
  | (T 13) => "ENDIF"
  | (T 14) => "WHILE"
  | (T 15) => "DO"
  | (T 16) => "ENDWH"
  | (T 17) => "OR"
  | (T 18) => "AND"
  | (T 19) => "TT"
  | (T 20) => "FF"
  | (T 21) => "NOT"
  | (T 22) => "LT"
  | (T 23) => "LEQ"
  | (T 24) => "EQ"
  | (T 25) => "GT"
  | (T 26) => "GEQ"
  | (T 27) => "NEQ"
  | (T 28) => "PLUS"
  | (T 29) => "MINUS"
  | (T 30) => "DIV"
  | (T 31) => "MOD"
  | (T 32) => "TIMES"
  | (T 33) => "TILDE"
  | (T 34) => "SET"
  | (T 35) => "EOF"
  | (T 36) => "PROG"
  | (T 37) => "COMMA"
  | (T 38) => "THEN"
  | (T 39) => "READ"
  | (T 40) => "WRITE"
  | (T 41) => "IDENTIFIER"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34)
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, _, COMMANDSEQ1right
)) :: ( _, ( MlyValue.DECLARATIONSEQ DECLARATIONSEQ1, _, _)) :: _ :: (
 _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, PROG1left,
 _)) :: rest671)) => let val  result = MlyValue.PROGRAM (fn _ => let
 val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (DECLARATIONSEQ as DECLARATIONSEQ1) = DECLARATIONSEQ1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (PROG(IDENTIFIER, DECLARATIONSEQ,COMMANDSEQ))
end)
 in ( LrTable.NT 0, ( result, PROG1left, COMMANDSEQ1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, _, COMMANDSEQ1right)
) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, 
PROG1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM (fn
 _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (PROG(IDENTIFIER,EMPTYDEC,COMMANDSEQ))
end)
 in ( LrTable.NT 0, ( result, PROG1left, COMMANDSEQ1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.DECLARATION DECLARATION1, DECLARATION1left, 
DECLARATION1right)) :: rest671)) => let val  result = 
MlyValue.DECLARATIONSEQ (fn _ => let val  (DECLARATION as DECLARATION1
) = DECLARATION1 ()
 in (DECLARATION)
end)
 in ( LrTable.NT 1, ( result, DECLARATION1left, DECLARATION1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.DECLARATIONSEQ DECLARATIONSEQ1, _, 
DECLARATIONSEQ1right)) :: ( _, ( MlyValue.DECLARATION DECLARATION1, 
DECLARATION1left, _)) :: rest671)) => let val  result = 
MlyValue.DECLARATIONSEQ (fn _ => let val  (DECLARATION as DECLARATION1
) = DECLARATION1 ()
 val  (DECLARATIONSEQ as DECLARATIONSEQ1) = DECLARATIONSEQ1 ()
 in (DSEQ (DECLARATION, DECLARATIONSEQ))
end)
 in ( LrTable.NT 1, ( result, DECLARATION1left, DECLARATIONSEQ1right),
 rest671)
end
|  ( 4, ( ( _, ( MlyValue.VARLIST VARLIST1, _, VARLIST1right)) :: ( _,
 ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, VAR1left, _))
 :: rest671)) => let val  result = MlyValue.DECLARATION (fn _ => let
 val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (VARLIST as VARLIST1) = VARLIST1 ()
 in (
lst:=IDENTIFIER::(!lst);SymbolTable.insertListInTable(!lst,!temp2);lst:=[];DEC(IDENTIFIER,VARLIST)
)
end)
 in ( LrTable.NT 2, ( result, VAR1left, VARLIST1right), rest671)
end
|  ( 5, ( ( _, ( _, _, SEQ1right)) :: _ :: ( _, ( _, COLON1left, _))
 :: rest671)) => let val  result = MlyValue.VARLIST (fn _ => (
lst:=[];temp2:=SymbolTable.INTEGER;INTLIST))
 in ( LrTable.NT 3, ( result, COLON1left, SEQ1right), rest671)
end
|  ( 6, ( ( _, ( _, _, SEQ1right)) :: _ :: ( _, ( _, COLON1left, _))
 :: rest671)) => let val  result = MlyValue.VARLIST (fn _ => (
lst:=[];temp2:=SymbolTable.BOOLEAN;BOOLLIST))
 in ( LrTable.NT 3, ( result, COLON1left, SEQ1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.VARLIST VARLIST1, _, VARLIST1right)) :: ( _,
 ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, COMMA1left, _)
) :: rest671)) => let val  result = MlyValue.VARLIST (fn _ => let val 
 (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (VARLIST as VARLIST1) = VARLIST1 ()
 in (lst:=IDENTIFIER::(!lst);COMMA(IDENTIFIER,VARLIST))
end)
 in ( LrTable.NT 3, ( result, COMMA1left, VARLIST1right), rest671)
end
|  ( 8, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( _, LBRACE1left, _)) ::
 rest671)) => let val  result = MlyValue.COMMANDSEQ (fn _ => (EMPTYCMD
))
 in ( LrTable.NT 4, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.COMMANDLIST 
COMMANDLIST1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.COMMANDSEQ (fn _ => let val  (COMMANDLIST as 
COMMANDLIST1) = COMMANDLIST1 ()
 in (COMMANDLIST)
end)
 in ( LrTable.NT 4, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 10, ( ( _, ( _, _, SEQ1right)) :: ( _, ( MlyValue.COMMAND 
COMMAND1, COMMAND1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMANDLIST (fn _ => let val  (COMMAND as COMMAND1) = 
COMMAND1 ()
 in (COMMAND)
end)
 in ( LrTable.NT 5, ( result, COMMAND1left, SEQ1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.COMMANDLIST COMMANDLIST1, _, 
COMMANDLIST1right)) :: _ :: ( _, ( MlyValue.COMMAND COMMAND1, 
COMMAND1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMANDLIST (fn _ => let val  (COMMAND as COMMAND1) = 
COMMAND1 ()
 val  (COMMANDLIST as COMMANDLIST1) = COMMANDLIST1 ()
 in (CSEQ(COMMAND,COMMANDLIST))
end)
 in ( LrTable.NT 5, ( result, COMMAND1left, COMMANDLIST1right), 
rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right
)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)
) :: rest671)) => let val  result = MlyValue.COMMAND (fn _ => let val 
 (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (EXPRESSION as EXPRESSION1) = EXPRESSION1 ()
 in (getType(IDENTIFIER);SET(IDENTIFIER,EXPRESSION,!temp))
end)
 in ( LrTable.NT 6, ( result, IDENTIFIER1left, EXPRESSION1right), 
rest671)
end
|  ( 13, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, IDENTIFIER1right
)) :: ( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMAND (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in (getType(IDENTIFIER);READ(IDENTIFIER,!temp))
end)
 in ( LrTable.NT 6, ( result, READ1left, IDENTIFIER1right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right
)) :: ( _, ( _, WRITE1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMAND (fn _ => let val  (EXPRESSION as EXPRESSION1) = 
EXPRESSION1 ()
 in (WRITE(EXPRESSION))
end)
 in ( LrTable.NT 6, ( result, WRITE1left, EXPRESSION1right), rest671)

end
|  ( 15, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ2, _, _)) :: _ :: ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, _,
 _)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, _)) :: ( _, (
 _, IF1left, _)) :: rest671)) => let val  result = MlyValue.COMMAND
 (fn _ => let val  (EXPRESSION as EXPRESSION1) = EXPRESSION1 ()
 val  COMMANDSEQ1 = COMMANDSEQ1 ()
 val  COMMANDSEQ2 = COMMANDSEQ2 ()
 in (ITE(EXPRESSION,COMMANDSEQ1,COMMANDSEQ2))
end)
 in ( LrTable.NT 6, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 16, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ1, _, _)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, _,
 _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMAND (fn _ => let val  (EXPRESSION as EXPRESSION1) = 
EXPRESSION1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (WH(EXPRESSION,COMMANDSEQ))
end)
 in ( LrTable.NT 6, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (PLUS(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (MINUS(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (TIMES(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (DIV(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (MOD(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.NUMERAL NUMERAL1, NUMERAL1left, 
NUMERAL1right)) :: rest671)) => let val  result = MlyValue.EXPRESSION
 (fn _ => let val  (NUMERAL as NUMERAL1) = NUMERAL1 ()
 in (INT(NUMERAL))
end)
 in ( LrTable.NT 7, ( result, NUMERAL1left, NUMERAL1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.NUMERAL NUMERAL1, _, NUMERAL1right)) :: ( _
, ( _, PLUS1left, _)) :: rest671)) => let val  result = 
MlyValue.EXPRESSION (fn _ => let val  (NUMERAL as NUMERAL1) = NUMERAL1
 ()
 in (UNPLUS(NUMERAL))
end)
 in ( LrTable.NT 7, ( result, PLUS1left, NUMERAL1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = 
MlyValue.EXPRESSION (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in (getType(IDENTIFIER);VARIABLE(IDENTIFIER,!temp))
end)
 in ( LrTable.NT 7, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 25, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.EXPRESSION (fn _ => (TT))
 in ( LrTable.NT 7, ( result, TT1left, TT1right), rest671)
end
|  ( 26, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.EXPRESSION (fn _ => (FF))
 in ( LrTable.NT 7, ( result, FF1left, FF1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (LT(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (LEQ(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (EQ(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (GT(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (GEQ(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (NEQ(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 33, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (OR(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (AND(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 7, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.EXPRESSION (fn _ => let val  (EXPRESSION as EXPRESSION1) = 
EXPRESSION1 ()
 in (NOT(EXPRESSION))
end)
 in ( LrTable.NT 7, ( result, NOT1left, EXPRESSION1right), rest671)

end
|  ( 36, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPRESSION 
EXPRESSION1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.EXPRESSION (fn _ => let val  (EXPRESSION as 
EXPRESSION1) = EXPRESSION1 ()
 in (EXPRESSION)
end)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right
)) :: ( _, ( _, TILDE1left, _)) :: rest671)) => let val  result = 
MlyValue.EXPRESSION (fn _ => let val  (EXPRESSION as EXPRESSION1) = 
EXPRESSION1 ()
 in (TILDE(EXPRESSION))
end)
 in ( LrTable.NT 7, ( result, TILDE1left, EXPRESSION1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : WHILEEXP_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun NUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.NUMERAL (fn () => i),p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun SEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun DOUBLECOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TILDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun SET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun PROG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.IDENTIFIER (fn () => i),p1,p2))
end
end
