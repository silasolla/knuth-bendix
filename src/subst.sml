(* file: subst.sml *)
(* description: substutition *)
(* author: Masaki Haga *)

signature SUBST = 
sig 
    type subst
    val apply: subst -> Term.term -> Term.term
    val dom: subst -> Var.key list
    val toString: subst -> string
    val fromString: string -> subst
    val match: Term.term -> Term.term -> subst option
    val compose: subst -> subst -> subst
    val unify: Term.term * Term.term -> subst option
end

structure Subst: SUBST =
struct 

local
    structure V = Var
    structure L = List
    structure LP = ListPair
    structure LU = ListUtil
    structure AL = AssocList
    structure T = Term
    structure S = Symbol
in

type subst = (Var.key * Term.term) list

fun apply sigma (T.Var x) = (case AL.find x sigma of SOME t => t
						   | NONE => T.Var x)
  | apply sigma (T.Fun (f,ts)) = T.Fun (f,L.map (apply sigma) ts) 

fun dom sigma = foldr (fn ((v,t),vs) => if T.Var v = t then vs else v :: vs) [] sigma

fun toString sigma = LU.toStringCommaSpaceBrace (fn (v,t) => "?" ^ V.toString v ^ " := " ^ T.toString t) sigma

fun fromString str = 
    let val tps = T.readMultipleKeySepartedTermPairs ("{",",","}") ":=" str
	fun getVar s  = case T.root s of 
			    S.V x => x 
			  | S.F _ => raise Fail ("Syntax error: var expected " ^ (T.toString s))
    in L.map (fn (s,t) => (getVar s, t)) tps
    end

fun match pattern term =
    let fun main [] sigma = SOME sigma (* Solution *)
	  | main ((T.Var x, t0) :: rest) sigma =
	    (case AL.find x sigma of
		 SOME t1 => if t0 = t1
			    then main rest sigma (* Match (Overlap) *)
			    else NONE (* Fail-V *)
	       | NONE => main rest ((x, t0) :: sigma)) (* Match *)
	  | main ((T.Fun (f,ts), T.Fun (f',ts')) :: rest) sigma =
	    if f = f' andalso L.length ts = L.length ts'
	    then main (LU.union (LP.zip (ts,ts'), rest)) sigma (* Decompose *)
	    else NONE (* Fail-FF *)
	  | main ((T.Fun _, T.Var _) :: _) _ = NONE (* Fail-FV *)
    in main [(pattern, term)] []
    end

(* composition of substitutions: rho o sigma *)
fun compose sigma rho =
    foldr (fn (vt, rhosigma) => case AL.add vt rhosigma of SOME vtrhosigma => vtrhosigma 
							 | NONE => rhosigma)
	  (foldr (fn ((v, t), sigma') => (v, apply rho t) :: sigma') [] sigma) rho

fun unify (term1, term2) =
    let fun main [] sigma = SOME sigma (* Solution *)
	  | main ((T.Var x, t as (T.Var y)) :: rest) sigma =
	    if x = y then main rest sigma (* Delete *)
	    else main (map (fn (t1, t2) => (apply [(x, t)] t1, apply [(x, t)] t2)) rest)
		      (compose sigma [(x, t)]) (* Eliminate 1 *)
	  | main ((T.Var x, t as T.Fun (f, ts)) :: rest) sigma =
	    if LU.member x (T.vars t) then NONE (* Occur-Check *)
	    else main (map (fn (t1, t2) => (apply [(x, t)] t1, apply [(x, t)] t2)) rest)
		      (compose sigma [(x, t)]) (* Eliminate 2 *)
	  | main ((T.Fun (f, ss) ,T.Fun (g, ts)) :: rest) sigma =
	    if f = g then main (LU.union (LP.zip (ss,ts), rest)) sigma (* Decompose *)
	    else NONE (* Crash *)
	  | main ((t, T.Var x) :: rest) sigma = main ((T.Var x, t) :: rest) sigma 
    in main [(term1,term2)] []
    end

end (* of local *)

end
