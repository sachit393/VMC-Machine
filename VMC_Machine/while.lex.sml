functor WHILELexFun(structure Tokens: WHILEEXP_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct


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



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as (fileName: string)) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (pos:=(!pos)+1;col:=1; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);continue())
      end
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.PROG(!pos,!col))
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=(!(col)+ size yytext);Tokens.DOUBLECOLON(!pos,!col))
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.VAR(!pos,!col))
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.INT(!pos,!col))
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.BOOL(!pos,!col))
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.COMMA(!pos,!col))
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.LBRACE(!pos,!col))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.RBRACE(!pos,!col))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.SEQ(!pos,!col))
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.SET(!pos,!col))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.READ(!pos,!col))
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.WRITE(!pos,!col))
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.IF(!pos,!col))
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.THEN(!pos,!col))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.ELSE(!pos,!col))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.ENDIF(!pos,!col))
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.WHILE(!pos,!col))
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.DO(!pos,!col))
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.ENDWH(!pos,!col))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.LPAREN(!pos,!col))
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.RPAREN(!pos,!col))
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.OR(!pos,!col))
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.AND(!pos,!col))
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.TT(!pos,!col))
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.FF(!pos,!col))
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.NOT(!pos,!col))
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.LT(!pos,!col))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.LEQ(!pos,!col))
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.EQ(!pos,!col))
      end
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.GT(!pos,!col))
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.GEQ(!pos,!col))
      end
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.NEQ(!pos,!col))
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.PLUS(!pos,!col))
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.MINUS(!pos,!col))
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.TIMES(!pos,!col))
      end
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.DIV(!pos,!col))
      end
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.MOD(!pos,!col))
      end
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.COLON(!pos,!col))
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=(!(col)+ size yytext);Tokens.TILDE(!pos,!col))
      end
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=(!(col)+ size yytext);Tokens.NUMERAL(toInt yytext,!pos,!col))
      end
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=(!(col)+ size yytext);Tokens.IDENTIFIER (yytext,!pos,!col))
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error(yytext, !pos, !col); col := !col + size yytext ;continue())
      end
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ35(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction13(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"e"
              then yyQ41(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"t"
              then yyQ40(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"t"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"i"
              then yyQ39(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"i"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"e"
              then yyQ44(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"l"
              then yyQ43(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"i"
              then yyQ42(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"i"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ38(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"h"
                  then yyQ37(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"r"
              then yyQ46(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"r"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"b"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ45(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction25(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"n"
              then yyQ50(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"n"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"e"
              then yyQ49(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ48(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"h"
                  then yyQ47(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"d"
              then yyQ53(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"d"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"b"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ52(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"e"
              then yyQ51(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"m"
              then yyQ59(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"m"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"b"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ58(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"r"
              then yyQ57(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"r"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"g"
              then yyQ56(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"g"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"o"
              then yyQ55(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"r"
              then yyQ54(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"r"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"t"
              then yyQ62(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"t"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction14(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ61(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"f"
                  then yyQ60(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"f"
              then yyQ63(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"f"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"h"
              then yyQ69(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"h"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction17(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"f"
              then yyQ70(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"f"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"w"
              then yyQ68(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"w"
              then if inp = #"i"
                  then yyQ67(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"d"
              then yyQ66(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"d"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction16(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"e"
              then yyQ72(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"s"
              then yyQ71(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"s"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ65(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"l"
                  then yyQ64(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"o"
              then yyQ73(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"l"
              then yyQ76(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"o"
              then yyQ75(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"o"
              then yyQ74(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ77(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ79(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ78(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #":"
                  then yyQ80(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"="
              then yyQ81(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ82(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
            else if inp < #"0"
              then yyAction41(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ82(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ82(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
            else if inp < #"0"
              then yyAction41(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ82(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ83(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ84(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ84(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ84(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ84(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"*"
                  then yyQ9(strm', lastMatch)
                else if inp < #"*"
                  then if inp = #"\""
                      then yyQ1(strm', lastMatch)
                    else if inp < #"\""
                      then if inp = #"\v"
                          then yyQ1(strm', lastMatch)
                        else if inp < #"\v"
                          then if inp = #"\t"
                              then yyQ2(strm', lastMatch)
                            else if inp = #"\n"
                              then yyQ3(strm', lastMatch)
                              else yyQ1(strm', lastMatch)
                        else if inp = #" "
                          then yyQ2(strm', lastMatch)
                        else if inp = #"!"
                          then yyQ4(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = #"'"
                      then yyQ1(strm', lastMatch)
                    else if inp < #"'"
                      then if inp = #"%"
                          then yyQ5(strm', lastMatch)
                        else if inp = #"&"
                          then yyQ6(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = #"("
                      then yyQ7(strm', lastMatch)
                      else yyQ8(strm', lastMatch)
                else if inp = #":"
                  then yyQ15(strm', lastMatch)
                else if inp < #":"
                  then if inp = #"."
                      then yyQ1(strm', lastMatch)
                    else if inp < #"."
                      then if inp = #","
                          then yyQ11(strm', lastMatch)
                        else if inp = #"+"
                          then yyQ10(strm', lastMatch)
                          else yyQ12(strm', lastMatch)
                    else if inp = #"/"
                      then yyQ13(strm', lastMatch)
                      else yyQ14(strm', lastMatch)
                else if inp = #"="
                  then yyQ18(strm', lastMatch)
                else if inp < #"="
                  then if inp = #";"
                      then yyQ16(strm', lastMatch)
                      else yyQ17(strm', lastMatch)
                else if inp = #">"
                  then yyQ19(strm', lastMatch)
                  else yyQ1(strm', lastMatch)
            else if inp = #"q"
              then yyQ20(strm', lastMatch)
            else if inp < #"q"
              then if inp = #"e"
                  then yyQ23(strm', lastMatch)
                else if inp < #"e"
                  then if inp = #"b"
                      then yyQ21(strm', lastMatch)
                    else if inp < #"b"
                      then if inp = #"["
                          then yyQ1(strm', lastMatch)
                        else if inp < #"["
                          then yyQ20(strm', lastMatch)
                        else if inp = #"a"
                          then yyQ20(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = #"c"
                      then yyQ20(strm', lastMatch)
                      else yyQ22(strm', lastMatch)
                else if inp = #"i"
                  then yyQ25(strm', lastMatch)
                else if inp < #"i"
                  then if inp = #"f"
                      then yyQ24(strm', lastMatch)
                      else yyQ20(strm', lastMatch)
                else if inp = #"p"
                  then yyQ26(strm', lastMatch)
                  else yyQ20(strm', lastMatch)
            else if inp = #"x"
              then yyQ20(strm', lastMatch)
            else if inp < #"x"
              then if inp = #"u"
                  then yyQ20(strm', lastMatch)
                else if inp < #"u"
                  then if inp = #"s"
                      then yyQ20(strm', lastMatch)
                    else if inp = #"r"
                      then yyQ27(strm', lastMatch)
                      else yyQ28(strm', lastMatch)
                else if inp = #"v"
                  then yyQ29(strm', lastMatch)
                  else yyQ30(strm', lastMatch)
            else if inp = #"}"
              then yyQ33(strm', lastMatch)
            else if inp < #"}"
              then if inp = #"{"
                  then yyQ31(strm', lastMatch)
                else if inp = #"|"
                  then yyQ32(strm', lastMatch)
                  else yyQ20(strm', lastMatch)
            else if inp = #"~"
              then yyQ34(strm', lastMatch)
              else yyQ1(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
