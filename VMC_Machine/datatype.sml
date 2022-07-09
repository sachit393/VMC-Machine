structure DataTypes =
struct
exception BoolExpectedIntOperand of string;
exception IntExpectedBoolOperand of string;
exception BoolExpectedIntIdentifier of string;
exception IntExpectedBoolIdentifier of string;
exception BoolExpectedIntValue of string;
exception IntExpectedBoolValue of string;
datatype AST = PROG of string*DEC*CMD 
        and  CMD = EMPTYCMD | CSEQ of CMD*CMD | SET of string*EXP*TYPE | ITE of EXP*CMD*CMD | WH of EXP*CMD | READ of string*TYPE | WRITE of EXP 
        and EXP = PLUS of EXP*EXP | MINUS of EXP*EXP | TIMES of EXP*EXP | DIV of EXP*EXP | MOD of EXP*EXP  | OR of EXP*EXP | AND of EXP*EXP | NOT of EXP  | LT of EXP*EXP | LEQ of EXP*EXP | EQ of EXP*EXP | GT of EXP*EXP | GEQ of EXP*EXP | NEQ of EXP*EXP | TILDE of EXP | VARIABLE of string*TYPE | TT | FF | INT  of int | UNPLUS of int
        and DEC = EMPTYDEC | DSEQ of DEC*DEC | DEC of string*VARIABLELIST
        and TYPE = INTEGER|BOOLEAN 
        and VARIABLELIST = INTLIST | BOOLLIST | COMMA of string*VARIABLELIST

fun typeCheckProg(PROG(name,dseq,cseq)) = typeCheckCSEQ(cseq)
and    typeCheckCSEQ(EMPTYCMD) = ()
     |typeCheckCSEQ(CSEQ(cmd1,cmd2)) = (typeCheckCSEQ(cmd1);typeCheckCSEQ(cmd2))
     |typeCheckCSEQ(SET(s,e,exptyp)) = typeCheckExp(e,exptyp)
     |typeCheckCSEQ(ITE(e,cmd1,cmd2)) = (typeCheckExp(e,BOOLEAN);typeCheckCSEQ(cmd1);typeCheckCSEQ(cmd2))
     |typeCheckCSEQ(WH(e,cmd)) = (typeCheckExp(e,BOOLEAN);typeCheckCSEQ(cmd))
     |typeCheckCSEQ(READ(s,typ)) = ()
     |typeCheckCSEQ(WRITE(e)) = typeCheckExp(e,INTEGER)
and typeCheckExp(PLUS(exp1,exp2),exptyp) = if exptyp = INTEGER then (typeCheckExp(exp1,exptyp);typeCheckExp(exp2,exptyp))
                                                else raise BoolExpectedIntOperand("Expected boolean, however + operator gives integer results\n")     
    |typeCheckExp(MINUS(exp1,exp2),exptyp) = if exptyp = INTEGER then  (typeCheckExp(exp1,exptyp);typeCheckExp(exp2,exptyp))
                                                else raise BoolExpectedIntOperand("Expected boolean, however - operator gives integer results\n")
    |typeCheckExp(TIMES(exp1,exp2),exptyp) = if exptyp = INTEGER then (typeCheckExp(exp1,exptyp);typeCheckExp(exp2,exptyp))
                                                else raise BoolExpectedIntOperand("Expected boolean, however * operator gives integer results\n")
    |typeCheckExp(DIV(exp1,exp2),exptyp) = if exptyp = INTEGER then (typeCheckExp(exp1,exptyp);typeCheckExp(exp2,exptyp))
                                                else raise BoolExpectedIntOperand("Expected boolean, however / operator gives integer results\n")
    |typeCheckExp(MOD(exp1,exp2),exptyp) = if exptyp = INTEGER then (typeCheckExp(exp1,exptyp);typeCheckExp(exp2,exptyp))
                                                else raise BoolExpectedIntOperand("Expected boolean, however % operator gives integer results\n")                                                        
    |typeCheckExp(AND(exp1,exp2),exptyp) = if exptyp = BOOLEAN then (typeCheckExp(exp1,exptyp);typeCheckExp(exp2,exptyp))
                                                else raise IntExpectedBoolOperand("Expected integer, however && operator gives boolean results\n")                                                        
    |typeCheckExp(OR(exp1,exp2),exptyp) = if exptyp = BOOLEAN then (typeCheckExp(exp1,exptyp);typeCheckExp(exp2,exptyp))
                                                else raise IntExpectedBoolOperand("Expected integer, however || operator gives boolean results\n")       

    |typeCheckExp(TILDE(exp1),exptyp) = if exptyp = INTEGER then typeCheckExp(exp1,exptyp) else raise BoolExpectedIntOperand("Expected boolean, however tilde gives integer results\n")
    |typeCheckExp(TT,exptyp) = if exptyp = BOOLEAN then () else raise IntExpectedBoolValue("Expected integer, however boolean value tt is present\n")
    |typeCheckExp(FF,exptyp) = if exptyp = BOOLEAN then () else raise IntExpectedBoolValue("Expected integer, however boolean value ff is present\n")
    |typeCheckExp(NOT(exp),exptyp) =   if exptyp = BOOLEAN then (typeCheckExp(exp,exptyp)) else raise IntExpectedBoolOperand("Expected integer, however ! operator gives boolean results\n")
    |typeCheckExp(VARIABLE(s,typ),exptyp) = if ((exptyp = BOOLEAN andalso typ = BOOLEAN) orelse (exptyp = INTEGER andalso typ = INTEGER)) then () else (if exptyp = BOOLEAN then raise BoolExpectedIntIdentifier("Expected boolean, however type of identifier "^s^ " is integer\n") else raise IntExpectedBoolIdentifier("Expected integer, however type identifier "^s^ " is boolean\n") )
    |typeCheckExp(EQ(exp1,exp2),exptyp) = if exptyp = BOOLEAN then 
                                        
                                                        (typeCheckExp(exp1,BOOLEAN);
                                                        typeCheckExp(exp2,BOOLEAN))
                                                        handle _ => (typeCheckExp(exp2,INTEGER);typeCheckExp(exp1,INTEGER))

                                                else raise IntExpectedBoolOperand("Expected integer, however = operator gives boolean results\n") 
    |typeCheckExp(LEQ(exp1,exp2),exptyp) = if exptyp = BOOLEAN then 
                                        
                                                        (typeCheckExp(exp1,BOOLEAN);
                                                        typeCheckExp(exp2,BOOLEAN))
                                                        handle _ => (typeCheckExp(exp2,INTEGER);typeCheckExp(exp1,INTEGER))

                                                else raise IntExpectedBoolOperand("Expected integer, however <= operator gives boolean results\n") 
    |typeCheckExp(NEQ(exp1,exp2),exptyp) = if exptyp = BOOLEAN then 
                                        
                                                        (typeCheckExp(exp1,BOOLEAN);
                                                        typeCheckExp(exp2,BOOLEAN))
                                                        handle _ => (typeCheckExp(exp2,INTEGER);typeCheckExp(exp1,INTEGER))

                                                else raise IntExpectedBoolOperand("Expected integer, however <> operator gives boolean results\n")                                                                                                         
    |typeCheckExp(GEQ(exp1,exp2),exptyp) = if exptyp = BOOLEAN then 
                                        
                                                        (typeCheckExp(exp1,BOOLEAN);
                                                        typeCheckExp(exp2,BOOLEAN))
                                                        handle _ => (typeCheckExp(exp2,INTEGER);typeCheckExp(exp1,INTEGER))

                                                else raise IntExpectedBoolOperand("Expected integer, however >= operator gives boolean results\n") 
    |typeCheckExp(GT(exp1,exp2),exptyp) = if exptyp = BOOLEAN then 
                                        
                                                        (typeCheckExp(exp1,BOOLEAN);
                                                        typeCheckExp(exp2,BOOLEAN))
                                                        handle _ => (typeCheckExp(exp2,INTEGER);typeCheckExp(exp1,INTEGER))

                                                else raise IntExpectedBoolOperand("Expected integer, however > operator gives boolean results\n") 
    |typeCheckExp(LT(exp1,exp2),exptyp) = if exptyp = BOOLEAN then 
                                        
                                                        (typeCheckExp(exp1,BOOLEAN);
                                                        typeCheckExp(exp2,BOOLEAN))
                                                        handle _ => (typeCheckExp(exp2,INTEGER);typeCheckExp(exp1,INTEGER))
                                                else raise IntExpectedBoolOperand("Expected integer, however < operator gives boolean results\n")
    |typeCheckExp(INT(x),exptyp) = if exptyp = INTEGER then ()
                                                else raise BoolExpectedIntValue("Expected boolean, however integer value "^Int.toString(x)^" is present\n") 
    |typeCheckExp(UNPLUS(x),exptyp) = if exptyp = INTEGER then () else raise BoolExpectedIntValue("Expected boolean, however integer value "^Int.toString(x)^" is present\n")                                                       
end ;


