

structure WHILE = 

struct  

fun compile (fileName) = 
        let val inStream = TextIO.openIn fileName;
          val grab : int -> string  = fn
              n => if TextIO.endOfStream inStream 
                        then ""
                        else TextIO.inputN(inStream, n);
          val printError: string * int * int -> unit = fn(msg, line, col) =>
          print("Following parsing error at line "^Int.toString(line)^"\n"^msg^"\n");
          val _ = Control.Print.printDepth:=1000;
          val _ = Control.Print.printLength := 500;

          val (x,lexer) = WHILEParser.parse(20, ((WHILEParser.makeLexer grab fileName)),printError, fileName);
          exception Error of string;
          exception TypeError;
          val _ =DataTypes.typeCheckProg(x)
          handle DataTypes.BoolExpectedIntOperand(s) => (print s;raise TypeError )
            |DataTypes.IntExpectedBoolOperand(s) => (print s;raise TypeError)
            |DataTypes.BoolExpectedIntIdentifier(s) => (print s;raise TypeError)
            |DataTypes.IntExpectedBoolIdentifier(s) => (print s;raise TypeError)
            |DataTypes.BoolExpectedIntValue(s) => (print s;raise TypeError)
            |DataTypes.IntExpectedBoolValue(s) => (print s;raise TypeError)

          val y = PostFixConverter.postfix(x);
          val _ = Vmc.degree := 0;
          val _ = Vmc.init := false;
          val _ = Vmc.firstSetVariable:=true;
          val _ = Vmc.degree2 :=0;
          val z = Vmc.execute(y)
          handle Vmc.NotConvertableToInt(s) => (print s;raise Error(s))
                |Vmc.DivisionByZero(s) => (print s;raise Error(s))
                |Vmc.IntCannotBeConvertedToBoolean(s) => (print s;raise Error(s))
                |Vmc.ErrorInRules(s) => (print s;raise Error(s))
                  
          val _ = TextIO.closeIn inStream;
         in  Vmc.toString z
        end;
end;

          
