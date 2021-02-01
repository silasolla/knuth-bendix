(* file: trs.sml *)
(* description: first-order term rewriting systems *)
(* author: Masaki Haga *)

signature TRS = 
sig 
    type trs = (Term.term * Term.term) list
    type eqs = (Term.term * Term.term) list
    type rule = (Term.term * Term.term)

    val prRule: Term.term * Term.term -> string
    val rdRule: string -> Term.term * Term.term

    val prRules: trs -> string
    val rdRules: string -> trs

    val prEq: Term.term * Term.term -> string
    val rdEq: string -> Term.term * Term.term

    val prEqs: eqs -> string
    val rdEqs: string -> eqs

    val rename: (Term.term * Term.term) * (Term.term * Term.term) -> (Term.term * Term.term) * (Term.term * Term.term)
end

structure Trs: TRS =
struct

local 
    structure LU = ListUtil
    structure T = Term
in

type trs = (T.term * T.term) list
type eqs = (T.term * T.term) list
type rule = (Term.term * Term.term)

fun prRule (l,r) = T.toString l ^ " -> " ^ T.toString r
fun rdRule str = T.readKeySeparatedTermPair "->" str 

fun prRules rs = LU.toStringCommaLnSquare prRule rs
fun rdRules str = T.readMultipleKeySepartedTermPairs ("[",",","]") "->" str

fun prEq (l,r) = T.toString l ^ " = " ^ T.toString r
fun rdEq str = T.readKeySeparatedTermPair "=" str 

fun prEqs es = LU.toStringCommaLnSquare prEq es
fun rdEqs str = T.readMultipleKeySepartedTermPairs ("[",",","]") "=" str

fun rename ((l1, r1), (l2, r2)) =
    let val varset = LU.union (T.vars l1, T.vars r1)
	val maxVarId = foldr (fn ((sym, id), i) => Int.max (id, i)) 0 varset
	fun rename' (T.Var (sym, id)) = T.Var (sym, id + maxVarId + 1)
	  | rename' (T.Fun (f, ts)) = T.Fun (f, map rename' ts)
    in ((l1, r1), (rename' l2, rename' r2))
    end
end (* of local *)

end
