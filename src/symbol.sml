(* file: symbol.sml *)
(* description: symbols for first-order terms *)
(* author: Masaki Haga *)

signature SYMBOL =
sig
    datatype symbol = V of Var.key | F of Fun.key

    val toString : symbol -> string
    val fromString : string -> symbol
end

structure Symbol : SYMBOL =
struct

datatype symbol = V of Var.key | F of Fun.key

fun toString (V x) = "?" ^ (Var.toString x)
  | toString (F f) = (Fun.toString f)

fun fromString str = 
    if String.isPrefix "?" str
    then V (Var.fromString (String.substring (str, 1, (String.size str) - 1)))
    else F (Fun.fromString str)

end
