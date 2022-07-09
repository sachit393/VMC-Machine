structure PostFixConverter = 
struct

datatype TOKEN = Variable of string | Times | Plus | Div | Mod | Minus | Integer of int | Set | Ite | And | Or | Not  | Tilde | Lt | Leq | Neq | Eq | Geq | Gt | Wh | Read |  Seq  | Write | EmptyCmd

fun postfix(DataTypes.PROG(x,dec,cmd)) = commandPostfix(cmd) 

and commandPostfix(DataTypes.CSEQ(cmd1,cmd2)) = (commandPostfix(cmd1))@(commandPostfix(cmd2))@[Seq]
	|commandPostfix(DataTypes.EMPTYCMD) = [EmptyCmd]
	|commandPostfix(DataTypes.SET(x,exp,typ)) = [Variable(x)]@expPostfix(exp)@[Set]
	|commandPostfix(DataTypes.ITE(exp,cmd1,cmd2)) = expPostfix(exp)@commandPostfix(cmd1)@commandPostfix(cmd2)@[Ite]
	|commandPostfix(DataTypes.WH(exp,cmd)) = expPostfix(exp)@commandPostfix(cmd)@[Wh]
	|commandPostfix(DataTypes.READ(x,typ)) = [Variable(x),Read]
	|commandPostfix(DataTypes.WRITE(exp)) = expPostfix(exp)@[Write]

and expPostfix(DataTypes.PLUS(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Plus]
	|expPostfix(DataTypes.MINUS(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Minus]
	|expPostfix(DataTypes.TIMES(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Times]
	|expPostfix(DataTypes.DIV(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Div]
	|expPostfix(DataTypes.MOD(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Mod]
	|expPostfix(DataTypes.OR(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Or]
	|expPostfix(DataTypes.AND(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[And]
	|expPostfix(DataTypes.LT(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Lt]
	|expPostfix(DataTypes.LEQ(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Leq]
	|expPostfix(DataTypes.GT(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Gt]
	|expPostfix(DataTypes.GEQ(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Geq]
	|expPostfix(DataTypes.NEQ(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Neq]
	|expPostfix(DataTypes.EQ(exp1,exp2)) = expPostfix(exp1)@expPostfix(exp2)@[Eq]
	|expPostfix(DataTypes.NOT(exp)) = expPostfix(exp)@[Not]
	|expPostfix(DataTypes.TILDE(exp)) = expPostfix(exp)@[Tilde]
	|expPostfix(DataTypes.UNPLUS(n)) = [Integer(n)]
	|expPostfix(DataTypes.VARIABLE(s,typ)) = [Variable(s)]
	|expPostfix(DataTypes.TT) = [Integer(1)]
	|expPostfix(DataTypes.FF) = [Integer(0)]
	|expPostfix(DataTypes.INT(x)) = [Integer(x)]
end;
