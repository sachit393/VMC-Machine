
val degree = ref 0
val init = ref false


fun execute(V,M,C,i) = 
		if i = 0 then rules(V,M,C,0)
		case top(C) of Variable(x) => (
										if (depth(C)>i andalso nth(C,i) = Set) then 
																execute(push(Variable(x),V),M,pop(C),i-1)
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
																				(Array.update (M, index, n);execute(V,M,pop(pop(C)),i-2))
																		else (Array.update (M, index, n);execute(V,M,pop(pop(C)),i-2))
										else 
											if (depth(C)>=i andalso nth(C,i) = Read) then 
												let 
						
													val str = valOf (TextIO.inputLine TextIO.stdIn);
													val SOME(index,typ) = HashTable.find SymbolTable.htable x;
													val n =  valOf (Int.fromString str)
													handle Option => raise NotConvertableToInt("Given input is not an integer or boolean value(0 or 1)\n")
												in 
													if typ = SymbolTable.BOOLEAN then 
														if n>1 orelse n<0 then raise IntCannotBeConvertedToBoolean("Only 0 and 1 are possible for boolean variables\n") 
														else 
															(Array.update (M, index, n);execute(V,M,pop(pop(C)),i-2))
													else (Array.update (M, index, n);execute(V,M,pop(pop(C)),i-2))
												end

											else 
												let 
												val SOME(index,typ) = HashTable.find SymbolTable.htable x; val k = Array.sub (M, index)
												in 
													execute(push(Integer(k),V),M,pop(C),i-1)
												end)																	end


									)
									|Plus => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => execute(push(Integer(m+n),tail2),M,pop(C),i-1)
									|Minus => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => execute(push(Integer(m-n),tail2),M,pop(C),i-1)
																																							)
											end 

									|Mod => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => execute(push(Integer(m mod n),tail2),M,pop(C),i-1)
									|Div => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => execute(push(Integer(m div n),tail2),M,pop(C),i-1)
																																							)
											end 
									|Times => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => execute(push(Integer(m*n),tail2),M,pop(C),i-1)
									|Or => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => execute(push(Integer(m+n),tail2),M,pop(C),i-1)
																																							)
											end 
									|And => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(0) => (
																			case op2 of Integer(m) => execute(push(Integer(0),tail2),M,pop(C),i-1)
																				)
															|Integer(1) => (
																			case op2 of Integer(m) => execute(push(Integer(m),tail2),M,pop(C),i-1)
																			)
											end 
									|Or => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(1) => (
																			case op2 of Integer(m) => execute(push(Integer(1),tail2),M,pop(C),i-1)
																				)
															|Integer(0) => (
																			case op2 of Integer(m) => execute(push(Integer(m),tail2),M,pop(C),i-1)
																			)
											end 
									|Leq => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m<=n then execute(push(Integer(1),tail2),M,pop(C),i-1) else execute(push(Integer(0),tail2),M,pop(C),i-1)
																				)
											end 		
									|Lt => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m<n then execute(push(Integer(1),tail2),M,pop(C),i-1) else execute(push(Integer(0),tail2),M,pop(C),i-1)
																				)
											end
									|Geq => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m>=n then execute(push(Integer(1),tail2),M,pop(C),i-1) else execute(push(Integer(0),tail2),M,pop(C),i-1)
																				)
											end 		
									|Gt => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m>n then execute(push(Integer(1),tail2),M,pop(C),i-1) else execute(push(Integer(0),tail2),M,pop(C),i-1)
																				)
											end 
									|Neq => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m<>n then execute(push(Integer(1),tail2),M,pop(C),i-1) else execute(push(Integer(0),tail2),M,pop(C),i-1)
																				)
											end 		
									|Eq => let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)  
											in 
												case op1 of Integer(n) => (
																			case op2 of Integer(m) => if m=n then execute(push(Integer(1),tail2),M,pop(C),i-1) else execute(push(Integer(0),tail2),M,pop(C),i-1)
																				)
											end 
									|Tilde => (case top(V) of Integer(m) => execute(push(Integer(~m),pop(V)),M,pop(C),i-1))
									|Not => (case top(V) of Integer(0) => (execute(push(Integer(1),pop(V)),M,pop(C),i-1))
												|Integer(1)=> (execute(push(Integer(0),pop(V)),M,pop(C),i-1)))
									|Seq => execute(V,M,pop(C),i-1)
									|Write => let 
												val SOME(m,stk1) = poptop(V)
												in
													case m of Integer(k) =>
																		((print ((Int.toString k)^"\n"));execute(stk1,M,pop(C),i-1))
											end
									|Set => (let 
												val SOME(op1,tail1) = poptop(V);val SOME(op2,tail2) = poptop(tail1)
											in
												case op1 of Integer(m) => (
																			case op2 of Variable(x) => let 
																										val SOME(index,typ) = (HashTable.find SymbolTable.htable x)
																									in 
																										(Array.update(M,index,m);execute(tail2,M,pop(C),i-1))
																									end
													)
											end)	
fun rules(V,M,C,i) = 
				if (init = false orelse degree<>1 orelse nth(C,i)<>Set orelse nth(C,i)<>Ite orelse nth(C,i)<>Seq orelse nth(C,i)<>Read orelse nth(C,i)<>Write orelse nth(C,i)<>Wh) then
					case nth(C,i) of Variable(x) => (init:=true;degree:=!(degree)+1;rules(V,M,C,i+1))
									|Integer(n) => (init:=true;degree:=!(degree)+1;rules(V,M,C,i+1))
									|Plus => (init:=true;degree:=!(degree)-1;rules(V,M,C.i+1))
									|Minus => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Div => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Mod => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Plus => (init:=true;degree:=!(degree)-1;rules(V,M,C.i+1))
									|Lt => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Leq => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Gt => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))										
									|Geq => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Neq => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Eq => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Or => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))										
									|And => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Neq => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Eq => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Not => (init:=true;rules(V,M,C,i+1))
									|Tilde => (init:=true;rules(V,M,C,i+1))
									|Set => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Ite => (init:=true;degree:=!(degree)-2;rules(V,M,C,i+1))
									|Wh => (init:=true;degree:=!(degree)-2;rules(V,M,C,i+1))
									|Read => (init:=true;degree:=!(degree);rules(V,M,C,i+1))
									|Write => (init:=true;degree:=!(degree);rules(V,M,C,i+1))
									|Seq => (init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
									|Bool(b) =>(init:=true;degree:=!(degree)-1;rules(V,M,C,i+1))
				else 
					execute(V,M,C,i)





