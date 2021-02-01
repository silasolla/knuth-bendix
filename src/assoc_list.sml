(* file: assoc_list.sml *)
(* description: functions for association list *)
(* author: Masaki Haga *)

signature ASSOC_LIST  =
sig
    val find: ''a -> (''a * 'b) list -> 'b option
    val add: (''a * ''b) -> (''a * ''b) list -> (''a * ''b) list option
end

structure AssocList : ASSOC_LIST=
struct 

fun find x [] = NONE
  | find x ((k,v)::ys) = if x = k then SOME v else find x ys

fun add (k,v) ys = case find k ys of
		       SOME w => if v = w then SOME ys
				 else NONE
		     | NONE => SOME ((k,v)::ys)

end
