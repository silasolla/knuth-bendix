(* file: fun.sml *)
(* description: function symbols for first-order terms *)
(* author: Masaki Haga *)

signature FUN = 
sig 
    eqtype key
    val toString: key -> string
    val fromString: string -> key
end

structure Fun : FUN =
struct 

type key = string

fun toString key = if String.isPrefix "?" key
		   then raise Fail "Error: Fun.toString: invalid fun name"
		   else key

fun fromString str = str

end
