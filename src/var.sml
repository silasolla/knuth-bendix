(* file: var.sml *)
(* description: variables for first-order terms *)
(* author: Masaki Haga *)

signature VAR = 
sig
    eqtype key
    val name: key -> string
    val index: key -> int
    val toString: key -> string
    val fromString: string -> key
end

structure Var : VAR =
struct

type key = string * int

fun name (x,i) = x

fun index (x,i) = i

fun toString (x,i) = if i = 0  then x
		     else if Int.> (i, 0) then x ^ "_" ^ (Int.toString i)
		     else raise Fail "Error: Var.toString: var index out of range"

(* .+_[1-9][0-9]* となっていれば末尾を index とする *)
fun fromString str = 
    let val ss = Substring.full str
	val (body,index) = Substring.splitr Char.isDigit ss
	val isProperIndexing = Int.> (Substring.size body, 1)
			    andalso Substring.sub (body, Substring.size body - 1) = #"_"
			    andalso not (Substring.isEmpty index)
			    andalso Substring.sub (index, 0) <> #"0"
    in if isProperIndexing
       then (Substring.string (Substring.trimr 1 body),
		  valOf (Int.fromString (Substring.string index)))
       else (str,0)
    end

end
