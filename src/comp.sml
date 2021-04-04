(* file: comp.sml *)
(* description: completion *)
(* author: Masaki Haga *)

signature COMP = 
sig 
    type eqs = (Term.term * Term.term) list
    type rule = Term.term * Term.term
    type rules = (Term.term * Term.term) list
    val simplify: eqs * rules -> eqs * rules
    val delete: eqs * rules -> eqs * rules
    val orient: (Term.term * Term.term -> bool) -> eqs * rules -> eqs * rule * rules
    val compose: eqs * rule * rules -> eqs * rule * rules 
    val deduce: eqs * rule * rules -> eqs * rule * rules 
    val collapse: eqs * rule * rules -> eqs * rule * rules
    val kbstep: (Term.term * Term.term -> bool) -> (eqs * rules) -> (eqs * rules)
    val prStatus: eqs * rule * rules -> eqs * rule * rules
    val kb: (Term.term * Term.term -> bool) -> eqs -> rules option
end

structure Comp: COMP =
struct 

local 
    structure L = List
    structure AL = AssocList
    structure LP = ListPair
    structure LU = ListUtil
    structure T = Term
    structure S = Symbol
    structure SU = StringUtil
    structure RW = Rewrite
    structure MS = ListMergeSort
in

type eqs = (Term.term * Term.term) list
type rule = Term.term * Term.term
type rules = (Term.term * Term.term) list

exception Failure
exception Success of rules

fun simplify (es, rs) =
    (foldr (fn ((s, t), es') => LU.add (RW.linf rs s, RW.linf rs t) es') [] es, rs)

fun delete (es, rs) = (L.filter (fn (s, t) => s <> t) es, rs)

fun orient grter (es, rs) =
    let fun orient' es1 [] = raise Failure
	  | orient' es1 ((s, t) :: es2) =
	    if grter (s, t) then (foldl (fn (z, zs) => z :: zs) es2 es1, (s, t), rs)
	    else if grter (t, s) then (foldl (fn (z, zs) => z :: zs) es2 es1, (t, s), rs)
	    else orient' ((s, t) :: es1) es2
    in orient' [] (MS.sort (fn ((s1, t1), (s2, t2)) =>
			(T.size s1 + T.size t1) > (T.size s2 + T.size t2)) es)
    end

fun compose (es, rule, rs) =
    (es, rule,
     foldr (fn ((s, t), rs') => LU.add (s, RW.linf (LU.add rule rs) t) rs') [] rs)

fun deduce (es, rule, rs) =
    (foldr (fn ((v1, u, v2), es') => LU.add (v1,v2) es') es (Cr.cpk (LU.add rule rs)),
     rule, rs)
fun collapse (es, rule, rs) =
    foldr (fn ((l, r), (es', rule, rs')) =>
	      case RW.listep [rule] l of
		  SOME l' => (LU.add (l', r) es', rule, rs')
		| NONE => (es', rule, (l, r) :: rs')) (es, rule, []) rs

(* 完備化の進行状況の表示 *)
fun prStatus (es, rule, rs) =
    let val _ = print (Trs.prEqs (rule::es))
	val _ = print (Trs.prRules rs)
    in (es, rule, rs) end

(* 完備化の帰納ステップ *)
fun kbstep grter (es,rs) =
     ((fn (es,rule,rs) => (es, LU.add rule rs))
      o collapse
      o deduce
      o compose
      o (fn (es,rule,rs) => prStatus (es,rule,rs))
      o (orient grter)
      o (fn (es,rs) => if null es then raise (Success rs) else (es,rs))
      o delete
      o simplify) (es,rs)

fun kb grter es =
    let val n = ref 0
	fun prStep x =
	    (n := !n + 1; print ("Step " ^ Int.toString (!n) ^ "\n"); x)
	fun kb' (es, rs) =
	    case ((kbstep grter) o prStep) (es, rs) of
		([], rs') => (print ("Success\n" ^ (prStep o Trs.prRules) rs'); SOME rs')
	      | (es', rs') => kb' (es', rs')
    in kb' (es, []) handle Failure => (print "Failure\n" ; NONE)
			 | Success rs => (print ("Success\n" ^ Trs.prRules rs); SOME rs)
    end			 

end (* of local *)

end (* of struct *)
