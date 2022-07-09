signature Stack =
	sig
	type 'a Stack
	exception EmptyStack
	exception Error of string
	val create: unit-> 'a Stack
	val push : 'a * 'a Stack -> 'a Stack
	val pop : 'a Stack -> 'a Stack
	val top : 'a Stack -> 'a
	val empty: 'a Stack -> bool
	val poptop : 'a Stack -> ('a * 'a Stack) option
	val nth : 'a Stack * int -> 'a
	val drop : 'a Stack * int -> 'a Stack
	val depth : 'a Stack -> int
	val app : ('a -> unit) -> 'a Stack -> unit
	val map : ('a -> 'b) -> 'a Stack -> 'b Stack
	val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
	val find : ('a -> bool) -> 'a Stack -> 'a option
	val filter : ('a -> bool) -> 'a Stack -> 'a Stack
	val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
	val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
	val exists : ('a -> bool) -> 'a Stack -> bool
	val all : ('a -> bool) -> 'a Stack -> bool
	val list2Stack : 'a list -> 'a Stack (* Convert a list into a Stack*)
	val Stack2list: 'a Stack -> 'a list (* Convert a Stack into a list *)
	val toString: ('a -> string) -> 'a Stack -> string
	val appendStack: 'a Stack* 'a Stack -> 'a Stack
end;


structure FunStack :> Stack =
struct
	exception EmptyStack;
	exception Error of string;
	datatype 'a Stack = Cons of 'a * 'a Stack| EmptySt;
	infix Cons;
	fun create _= EmptySt;
	fun push (x,st) = x Cons st;
	fun pop(EmptySt) = raise EmptyStack
		|pop(hd Cons tail) = tail;
	fun top(EmptySt) = raise EmptyStack
		|top(hd Cons tail) = hd;
	fun empty(EmptySt) = true
		|empty(hd Cons tail) = false;
	fun poptop(hd Cons tail) = (SOME(hd,tail))
		|poptop(EmptySt) = NONE;

	fun nthst(hd Cons tail,n,i) = if n=i then hd else nthst(tail,n,i+1)
	|nthst(EmptySt,n,i) = raise Error("Invalid index to stack\n");

	fun nth(stk,n) = nthst(stk,n,0);

	fun depth(hd Cons tail) = 1+depth(tail)
		|depth(EmptySt) = 0;

	fun dropst(hd Cons tail,k,i) = if k=i then (hd Cons tail) else dropst(tail,k+1,i)
		|dropst(EmptySt,k,i) = raise Error("Invalid argument - number of elements to drop\n")

	fun drop(stk,i) = if i >=0 andalso i<=depth stk then dropst(stk,0,i)
						else raise Error("Invalid argument - number of elements to drop\n")

(*What is the role of app function*)
	fun app f (hd Cons tail) = ((f hd); app f tail)
		|app f (EmptySt) = ();

	fun map f (hd Cons tail) = (f hd) Cons (map f tail)
		|map f (EmptySt) = EmptySt;

	fun reverse (hd Cons tail,resstk) = (reverse (tail,hd Cons resstk))
		|reverse (EmptySt,resstk) = resstk;

	fun find f (EmptySt)= NONE
		|find f (hd Cons tail) = if (f hd) = true then SOME(hd) else find f (tail);
	fun filter f (hd Cons tail) = if (f hd) = true then (hd Cons filter f tail) else filter f tail
		|filter f (EmptySt) = EmptySt;

	fun mapPartial f stk = ((map valOf) o (filter isSome) o (map f)) stk;


	fun foldr f init (hd Cons tail) = f (hd, foldr f init tail)
		|foldr f init (EmptySt) = init


	fun foldl f init (stk) = let val revstk = reverse (stk,EmptySt) in
									foldr f init revstk
								end ;

	fun exists f (hd Cons tail) = if (f hd) = true then  true else exists f tail
		|exists f (EmptySt) = false;

	fun all f stk = not(exists (not o f) stk);
	fun list2Stack (hd::tl)  = hd Cons list2Stack(tl)
		|list2Stack([]) = EmptySt;
	fun Stack2list (hd Cons tl) = hd::Stack2list(tl)
		|Stack2list(EmptySt) = [];



	fun list2string(hd :: tl) = hd^" "^list2string(tl)^" "
		|list2string([]) = "\n"
	fun toString a2s stk = let val lst = Stack2list stk; val stringList = List.map a2s lst
							in
								list2string stringList
							end
	fun appendStack(hd Cons tl,st2) = hd Cons (appendStack(tl,st2))
		|appendStack(EmptySt, st2) = st2
(*	fun toString a2s stk = let val lst = Stack2list stk
							in
							list2string(lst)
							end
*)
 (*to string left*)
end
