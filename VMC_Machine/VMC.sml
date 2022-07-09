signature VMC = 

	sig
		exception NotConvertableToInt of string;
		exception DivisionByZero of string;
		exception IntCannotBeConvertedToBoolean of string ;
		exception ErrorInRules of string;
		val degree: int ref;
		val degree2: int ref;
		val init: bool ref;
		val firstSetVariable:bool ref;
		val rules:  PostFixConverter.TOKEN FunStack.Stack*int Array.array*PostFixConverter.TOKEN FunStack.Stack -> PostFixConverter.TOKEN FunStack.Stack*int Array.array*PostFixConverter.TOKEN FunStack.Stack
		val execute: PostFixConverter.TOKEN list -> PostFixConverter.TOKEN FunStack.Stack*int Array.array*PostFixConverter.TOKEN FunStack.Stack
		val toString: PostFixConverter.TOKEN FunStack.Stack*int Array.array*PostFixConverter.TOKEN FunStack.Stack -> string 
	end;


structure Vmc:>VMC = 
struct 
	open PostFixConverter;
	open FunStack;
	exception NotConvertableToInt of string;
	exception DivisionByZero of string;
	exception IntCannotBeConvertedToBoolean of string ;
	exception ErrorInRules of string;
		fun a2s (Variable(x)) = x
					|a2s(Times) = " x "
					|a2s(Plus) = " + "
					|a2s(Minus) = " - "
					|a2s(Div) = " / "
					|a2s(Mod) = " % "
					|a2s(Integer(n)) = Int.toString(n)
					|a2s(Set) =  " := "
					|a2s(Ite) = " Ite "
					|a2s(And) = " And "
					|a2s(Or) = " Or "
					|a2s(Not) = " Not " 
					|a2s(Tilde) = " ~ "
					|a2s(Lt) = " < "
					|a2s(Leq) = " <= "
					|a2s(Neq) = " <> "
					|a2s(Eq) = " = "
					|a2s(Gt) = " > "
					|a2s(Geq) = " >= "
					|a2s(Wh) = " Wh "
					|a2s(Read) = " Read "
					|a2s(Seq) = " Seq "
					|a2s(Write) = " Write "
					|a2s(EmptyCmd) = " "

val degree2 = ref 0;
fun shift(V,C,i) = if i=0 then (V,C) else let val x = top(C) in shift(push(x,V),pop(C),i-1) end

(*fun copyRev(V,C,i) = if i=0 then (V,C) else let val x = top(C) in copyRev(push(x,V),C,i-1) end*)


fun removeStack(C,i) = if i = 0 then C else removeStack(pop(C),i-1)


fun transferStack(V,C,i) = if i = 0 then (V,C) else let val x = top(C)
														in 
														transferStack(push(x,V),pop(C),i-1)
													end

fun evaluateBoolean(V,M,C,l) = if l = 0 then case top(V) of Integer(0) => (pop(V),M,C,0)
															|Integer(1) => (pop(V),M,C,1)
															| _ => raise ErrorInRules("There is an error in operational semantics\n")
								else 
									case top(C) of 
										Integer(m) => evaluateBoolean(push(Integer(m),V),M,pop(C),l-1)
										|Variable(x) => 
												let 
												val SOME(index,typ) = HashTable.find SymbolTable.htable x; val k = Array.sub (M, index)
												in 
													evaluateBoolean(push(Integer(k),V),M,pop(C),l-1)
												end

										|Plus => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => evaluateBoolean(push(Integer(m+n),tail2),M,pop(C),l-1)
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end
										|Minus => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => evaluateBoolean(push(Integer(m-n),tail2),M,pop(C),l-1)
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end
										|Div => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => ( if n = 0 then raise DivisionByZero("RuntimeError -  division by zero not allowed\n")
																			else case op2 of Integer(m) => evaluateBoolean(push(Integer(m div n),tail2),M,pop(C),l-1)
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end
										|Mod => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (if n = 0 then raise DivisionByZero("RuntimeError -  division by zero not allowed\n")
																			else
																			case op2 of Integer(m) => evaluateBoolean(push(Integer(m mod n),tail2),M,pop(C),l-1)
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end
										|Times => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => evaluateBoolean(push(Integer(m*n),tail2),M,pop(C),l-1)
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end
										|And => let 
													val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
												in 
													case op1 of Integer(0) => (
																				case op2 of Integer(m) => evaluateBoolean(push(Integer(0),tail2),M,pop(C),l-1)
																					| _ => raise ErrorInRules("There is an error in operational semantics\n")
																						)
																|Integer(1) => (
																				case op2 of Integer(m) => evaluateBoolean(push(Integer(m),tail2),M,pop(C),l-1)
																					| _ => raise ErrorInRules("There is an error in operational semantics\n")
																						)
																| _ => raise ErrorInRules("There is an error in operational semantics\n")
												end 
										|Or => let 
													val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
												in 
													case op1 of Integer(1) => (
																				case op2 of Integer(m) => evaluateBoolean(push(Integer(1),tail2),M,pop(C),l-1)
																					| _ => raise ErrorInRules("There is an error in operational semantics\n")
																						)
																|Integer(0) => (
																				case op2 of Integer(m) => evaluateBoolean(push(Integer(m),tail2),M,pop(C),l-1)
																					| _ => raise ErrorInRules("There is an error in operational semantics\n")
																						)
																| _ => raise ErrorInRules("There is an error in operational semantics\n")
												end 
										|Leq => let 
													val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
												in 
													case op1 of Integer(n) => (
																				case op2 of Integer(m) => if m<=n then evaluateBoolean(push(Integer(1),tail2),M,pop(C),l-1) else evaluateBoolean(push(Integer(0),tail2),M,pop(C),l-1)
																					| _ => raise ErrorInRules("There is an error in operational semantics\n")
																						)
													| _ => raise ErrorInRules("There is an error in operational semantics\n")
												end 		
										|Lt => let 
													val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
												in 
													case op1 of Integer(n) => (
																				case op2 of Integer(m) => if m<n then evaluateBoolean(push(Integer(1),tail2),M,pop(C),l-1) else (evaluateBoolean(push(Integer(0),tail2),M,pop(C),l-1))
																					| _ => raise ErrorInRules("There is an error in operational semantics\n")
																					)
													| _ => raise ErrorInRules("There is an error in operational semantics\n")
												end
										|Geq => let 
													val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
												in 
													case op1 of Integer(n) => (
																				case op2 of Integer(m) => if m>=n then evaluateBoolean(push(Integer(1),tail2),M,pop(C),l-1) else evaluateBoolean(push(Integer(0),tail2),M,pop(C),l-1)
																					| _ => raise ErrorInRules("There is an error in operational semantics\n")
																					)
													| _ => raise ErrorInRules("There is an error in operational semantics\n")
												end 		
										|Gt => let 
													val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
												in 
													case op1 of Integer(n) => (
																				case op2 of Integer(m) => if m>n then evaluateBoolean(push(Integer(1),tail2),M,pop(C),l-1) else evaluateBoolean(push(Integer(0),tail2),M,pop(C),l-1)
																					| _ => raise ErrorInRules("There is an error in operational semantics\n")
																					)
													| _ => raise ErrorInRules("There is an error in operational semantics\n")
												end 
										|Neq => let 
													val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
												in 
													case op1 of Integer(n) => (
																				case op2 of Integer(m) => if m<>n then evaluateBoolean(push(Integer(1),tail2),M,pop(C),l-1) else evaluateBoolean(push(Integer(0),tail2),M,pop(C),l-1)
																					| _ => raise ErrorInRules("There is an error in operational semantics\n")
																					)
													| _ => raise ErrorInRules("There is an error in operational semantics\n")
												end 		
										|Eq => let 
													val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
												in 
													case op1 of Integer(n) => (
																				case op2 of Integer(m) => if m=n then evaluateBoolean(push(Integer(1),tail2),M,pop(C),l-1) else evaluateBoolean(push(Integer(0),tail2),M,pop(C),l-1)
																					| _ => raise ErrorInRules("There is an error in operational semantics\n")
																					)
													| _ => raise ErrorInRules("There is an error in operational semantics\n")
												end 

										|Tilde => (case top(V) of Integer(m) => evaluateBoolean(push(Integer(~m),pop(V)),M,pop(C),l-1)
														| _ => raise ErrorInRules("There is an error in operational semantics\n"))
										|Not => (case top(V) of Integer(0) => evaluateBoolean(push(Integer(1),pop(V)),M,pop(C),l-1)
													|Integer(1)=> evaluateBoolean(push(Integer(0),pop(V)),M,pop(C),l-1)
													| _ => raise ErrorInRules("There is an error in operational semantics\n"))
										| _ => raise ErrorInRules("There is an error in operational semantics\n")



fun reduceCommand(V,M,C,i) = 
							case top(V) of Variable(x) => let val _ = degree2:=(!degree2)+1
												in 
													if !degree2= 1 then (pop(V),M,push(Variable(x),C),i+1)
													else 
														reduceCommand(pop(V),M,push(Variable(x),C),i+1)
												end
									|Integer(n) => let val _ = degree2:=(!degree2)+1
												in 
													if !degree2= 1 then (pop(V),M,push(Integer(n),C),i+1)
													else 
														reduceCommand(pop(V),M,push(Integer(n),C),i+1)
												end
									|Plus => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Plus,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Plus,C),i+1)
												end
									|Minus => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Minus,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Minus,C),i+1)
												end
									|Div => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Div,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Div,C),i+1)
												end
									|Mod => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Mod,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Mod,C),i+1)
												end
									|Times => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Times,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Times,C),i+1)
												end
									|Lt => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Lt,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Lt,C),i+1)
												end
												
									|Leq => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Leq,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Leq,C),i+1)
												end
									|Gt => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Gt,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Gt,C),i+1)	

												end									
									|Geq => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Geq,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Geq,C),i+1)
												end
									|Or => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Or,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Or,C),i+1)	


												end									
									|And => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(And,C),i+1)
													else 
														reduceCommand(pop(V),M,push(And,C),i+1)

												end
									|Neq => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Neq,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Neq,C),i+1)

												end
									|Eq => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Eq,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Eq,C),i+1)
												end
									|Not => reduceCommand(pop(V),M,push(Not,C),i+1)
									|Tilde => reduceCommand(pop(V),M,push(Tilde,C),i+1)
									|Set => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Set,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Set,C),i+1)
												end
									|Ite => let val _ = degree2:=(!degree2)-2
												in 
													if !degree2= 1 then (pop(V),M,push(Ite,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Ite,C),i+1)
												end
									|Wh => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Wh,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Wh,C),i+1)
												end
									|Read => reduceCommand(pop(V),M,push(Read,C),i+1)
									|Write => reduceCommand(pop(V),M,push(Write,C),i+1)
									|Seq => let val _ = degree2:=(!degree2)-1
												in 
													if !degree2= 1 then (pop(V),M,push(Seq,C),i+1)
													else 
														reduceCommand(pop(V),M,push(Seq,C),i+1)
												end
									|EmptyCmd => let val _ = degree2:=(!degree2)+1
												in 
													if !degree2= 1 then (pop(V),M,C,i+1)
													else 
														reduceCommand(pop(V),M,C,i+1)
												end
									



val degree = ref 0
val init = ref false
val firstSetVariable = ref true;

fun reduceStack(V,M,C,i,n) = 
		if (i = ~1) then (degree:=0;implementRules(V,M,C,0))
		else if nth(C,i) = Wh then (
				let 
					val (V1Bar,C1) = shift(V,C,i+1)
					val V1 = pop(V1Bar)
					val _ = degree2:=0;
					val (V2,M,C2,lcmd) = reduceCommand(V1,M,C,0);
					val _ = degree2:=0;
					val (V3,M,C3,lbool) = reduceCommand(V2,M,C2,0);
					val _ = degree2:=0;	
					val (V4,M,C4,x)= evaluateBoolean(V3,M,C3,lbool);

				in 
					if x = 0 then let val C5 = removeStack(C4,2*lcmd+lbool+1)
									in 
										(degree:=0;implementRules(V4,M,C5,0))
									end
					else 
										(degree:=0;implementRules(V4,M,C4,0))
									
				end
			)
		else if  nth(C,i) = Ite then (let 
											val (V1Bar,C1) = shift(V,C,i+1)
											val V1 = pop(V1Bar)  (*pop ITE from stack*)
											val _ = degree2:=0;

											val (V2,M,C2,lcmd2) = reduceCommand(V1,M,C1,0);
											val _ = degree2:=0;

											val (V3,M,C3,lcmd1) = reduceCommand(V2,M,C2,0);
											val _ = degree2:=0;

											val (V4,M,C4,lbool) = reduceCommand(V3,M,C3,0)

											val (V5,M,C5,x)= evaluateBoolean(V4,M,C4,lbool)

										in 
											if x = 0 then (degree:=0;implementRules(V5,M,removeStack(C5,lcmd1),0))													
											else let val (V6,C6) = transferStack(V5,C5,lcmd1)
													val C7 = removeStack(C6,lcmd2)
													val (C8,V8) = transferStack(C7,V6,lcmd1)
												in 
													(degree:=0;implementRules(V8,M,C8,0))
												end
										end)
		else
			case top(C) of 
			Integer(m)=> (reduceStack(push(Integer(m),V),M,pop(C),i-1,n))
			|Variable(x) => (
										if (depth(C)>i andalso nth(C,i) = Set andalso i=n) then 
																(firstSetVariable:=false;reduceStack(push(Variable(x),V),M,pop(C),i-1,n))
															else 
																if (depth(C)>i andalso nth(C,i) = Read) then 
																	let 
											
																		val str = valOf (TextIO.inputLine TextIO.stdIn);
																		val SOME(index,typ) = HashTable.find SymbolTable.htable x;
																		val n =  valOf (Int.fromString str)
																		handle Option => raise NotConvertableToInt("Given input is not an integer or boolean value(0 or 1)\n")
																	in 
																		if typ = SymbolTable.BOOLEAN then 
																			if n>1 orelse n<0 then raise IntCannotBeConvertedToBoolean("Only 0 and 1 are possible for boolean variables\n") 
																			else 
																				(Array.update (M, index, n);reduceStack(V,M,pop(pop(C)),i-2,n))
																		else (Array.update (M, index, n);reduceStack(V,M,pop(pop(C)),i-2,n))
																	end
										else 
											if (depth(C)>i andalso nth(C,i) = Read) then 
												let 
						
													val str = valOf (TextIO.inputLine TextIO.stdIn);
													val SOME(index,typ) = HashTable.find SymbolTable.htable x;
													val n =  valOf (Int.fromString str)
													handle Option => raise NotConvertableToInt("Given input is not an integer or boolean value(0 or 1)\n")
												in 
													if typ = SymbolTable.BOOLEAN then 
														if n>1 orelse n<0 then raise IntCannotBeConvertedToBoolean("Only 0 and 1 are possible for boolean variables\n") 
														else 
															(Array.update (M, index, n);reduceStack(V,M,pop(pop(C)),i-2,n))
													else (Array.update (M, index, n);reduceStack(V,M,pop(pop(C)),i-2,n))
												end

											else 
												let 
												val SOME(index,typ) = HashTable.find SymbolTable.htable x; val k = Array.sub (M, index)
												in 
													(reduceStack(push(Integer(k),V),M,pop(C),i-1,n))
												end)																	


									
									|Plus => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => (reduceStack(push(Integer(m+n),tail2),M,pop(C),i-1,n))
																			| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end
									|Minus => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => (reduceStack(push(Integer(m-n),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																			)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end 

									|Mod => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			if n = 0 then raise DivisionByZero("RuntimeError -  division by zero not allowed\n")
																			else
																			case op2 of Integer(m) => ((reduceStack(push(Integer(m mod n),tail2),M,pop(C),i-1,n)))
																			| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end
									|Div => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			if n = 0 then raise DivisionByZero("RuntimeError -  division by zero not allowed\n")
																			else
																			case op2 of Integer(m) => (reduceStack(push(Integer(m div n),tail2),M,pop(C),i-1,n))
																			| _ => raise ErrorInRules("There is an error in operational semantics\n")
																			    )
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end 
									|Times => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => (reduceStack(push(Integer(m*n),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																			    )
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end 
			
									|And => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(0) => (
																			case op2 of Integer(m) => (reduceStack(push(Integer(0),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																					)
															|Integer(1) => (
																			case op2 of Integer(m) => (reduceStack(push(Integer(m),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																					)
															| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end 
									|Or => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(1) => (
																			case op2 of Integer(m) => (reduceStack(push(Integer(1),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																					)
															|Integer(0) => (
																			case op2 of Integer(m) => (reduceStack(push(Integer(m),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																					)
															| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end 
									|Leq => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m<=n then (reduceStack(push(Integer(1),tail2),M,pop(C),i-1,n)) else (reduceStack(push(Integer(0),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																					)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end 		
									|Lt => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m<n then (reduceStack(push(Integer(1),tail2),M,pop(C),i-1,n)) else (reduceStack(push(Integer(0),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end
									|Geq => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m>=n then (reduceStack(push(Integer(1),tail2),M,pop(C),i-1,n)) else (reduceStack(push(Integer(0),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end 		
									|Gt => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m>n then (reduceStack(push(Integer(1),tail2),M,pop(C),i-1,n)) else (reduceStack(push(Integer(0),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end 
									|Neq => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m<>n then (reduceStack(push(Integer(1),tail2),M,pop(C),i-1,n)) else (reduceStack(push(Integer(0),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end 		
									|Eq => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m=n then (reduceStack(push(Integer(1),tail2),M,pop(C),i-1,n)) else (print "case - 27\n";reduceStack(push(Integer(0),tail2),M,pop(C),i-1,n))
																				| _ => raise ErrorInRules("There is an error in operational semantics\n")
																				)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end 
									|Tilde => (case top(V) of Integer(m) => (reduceStack(push(Integer(~m),pop(V)),M,pop(C),i-1,n))
										| _ => raise ErrorInRules("There is an error in operational semantics\n"))
									|Not => (case top(V) of Integer(0) => (reduceStack(push(Integer(1),pop(V)),M,pop(C),i-1,n))
												|Integer(1)=> (reduceStack(push(Integer(0),pop(V)),M,pop(C),i-1,n))
												| _ => raise ErrorInRules("There is an error in operational semantics\n"))
									|Seq => (reduceStack(V,M,pop(C),i-1,n))
									|Write => (let 
												val SOME(m,stk1) = poptop(V)
												in
													case m of Integer(k) =>
																		((print ((Int.toString k)^"\n"));reduceStack(stk1,M,pop(C),i-1,n))
													| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end)
									|Set => (let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)
											in
												case op1 of Integer(m) => (
																			case op2 of Variable(x) => (let 
																										val SOME(index,typ) = (HashTable.find SymbolTable.htable x)
																									in 
																										(Array.update(M,index,m);firstSetVariable:=true;reduceStack(tail2,M,pop(C),i-1,n))
																									end)
																			| _ => raise ErrorInRules("There is an error in operational semantics\n")
													)
												| _ => raise ErrorInRules("There is an error in operational semantics\n")
											end)
									|EmptyCmd => reduceStack(V,M,pop(C),i-1,n)
									| _ => raise ErrorInRules("There is an error in operational semantics\n")
and 
implementRules(V,M,C,i) = 
				if (depth(C)>0 andalso top(C)=Seq andalso i=0 ) then implementRules(V,M,pop(C),i) 
				else if depth(C) = 0 then (V,M,C)
				else if (depth(C)>i andalso (i=0 orelse !init = false orelse (!degree)<>1 orelse (nth(C,i-1)<>Set andalso nth(C,i-1)<>Ite andalso nth(C,i-1)<>Seq andalso nth(C,i-1)<>Read andalso nth(C,i-1)<>Write andalso nth(C,i-1)<>Wh) )) then
					(case nth(C,i) of Variable(x) => (init:=true;degree:=(!degree)+1;implementRules(V,M,C,i+1))
									|Integer(n) => (init:=true;degree:=(!degree)+1;implementRules(V,M,C,i+1))
									|Plus => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Minus => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Div => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Mod => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Times => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Lt => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Leq => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Gt => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))										
									|Geq => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Or => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))										
									|And => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Neq => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Eq => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Not => (init:=true;implementRules(V,M,C,i+1))
									|Tilde => (init:=true;implementRules(V,M,C,i+1))
									|Set => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Ite => (init:=true;degree:=(!degree)-2;implementRules(V,M,C,i+1))
									|Wh => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|Read => (init:=true;degree:=(!degree);implementRules(V,M,C,i+1))
									|Write => (init:=true;degree:=(!degree);implementRules(V,M,C,i+1))
									|Seq => (init:=true;degree:=(!degree)-1;implementRules(V,M,C,i+1))
									|EmptyCmd => (init:=true;degree:=(!degree)+1;implementRules(V,M,C,i+1)))
				else 
					(
					(reduceStack(V,M,C,i-1,i-1)))





		fun rules(V,M,C) = implementRules(V,M,C,0)

		fun execute(expPostfix) = let val ValueStack = FunStack.list2Stack [];
									  val Memory = Array.array(100,0);
									  val ControlStack = FunStack.list2Stack expPostfix
									in 
										rules(ValueStack,Memory,ControlStack)
									end 

		fun arrayToString (arr,n) = if n = Array.length arr then ""
		 							else Int.toString(Array.sub (arr,n))^" "^arrayToString(arr,n+1)^" "

		fun toString(V,M,C) = (FunStack.toString a2s V)^"; "^arrayToString(M,0)^"; "^(FunStack.toString a2s C);





		
end ;