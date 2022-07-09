

structure SymbolTable = 
struct
datatype TYPE = BOOLEAN|INTEGER;
exception IdentifierNotDeclared;
exception IdentifierRedeclartion;
val htable:(string,int*TYPE) HashTable.hash_table = HashTable.mkTable(HashString.hashString,op=) (100,IdentifierNotDeclared);
val index = ref 0;
fun insertListInTable([],valtyp) = ()
    | insertListInTable(hd::tl,valtyp) = 			
	
						case HashTable.find htable hd of SOME y =>  
							(print ("Redeclaration of variable "^hd^"\n");HashTable.clear htable;raise IdentifierRedeclartion)
							| NONE => (HashTable.insert htable (hd,(!index,valtyp)) ;index:= (!index)+1;insertListInTable(tl,valtyp))




end;


