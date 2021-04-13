(* file: rewrite.sml *)
(* description: rewriting *)
(* author: Masaki Haga *)

signature REWRITE = 
sig 
    val rootStep: Trs.trs -> Term.term -> Term.term option
    val step: Trs.trs -> Term.term -> Term.position -> Term.term option
    val listep: Trs.trs -> Term.term -> Term.term option
    val isNF: Trs.trs -> Term.term -> bool
    (* val listepsToNF: Trs.trs -> Term.term -> unit *)
    val listepsToNF: Trs.trs -> Term.term -> string
    val linf: Trs.trs -> Term.term -> Term.term
    val isJoinable: Trs.trs -> Term.term * Term.term -> bool
end

structure Rewrite: REWRITE =
struct 

local 
    structure L = List
    structure MS = ListMergeSort
    structure LU = ListUtil
    structure S = Subst
    structure T = Term
in

(* rootStep rs term : TRS rs による根位置での書き換え *)
fun rootStep [] term = NONE
  | rootStep ((l,r)::rs) term =
    case S.match l term of
	SOME sigma => SOME (S.apply sigma r)
      | NONE => rootStep rs term

fun step rs term pos =
    let val C = T.makeContext term pos
	val subTerm = T.subterm term pos
    in case rootStep rs subTerm of
	   SOME termSigma => SOME (C termSigma)
	 | NONE => NONE
    end

fun listep rs term =
    let fun listep' [] = NONE
	  | listep' (p::ps) = case step rs term p of
				  SOME t => SOME t
				| NONE => listep' ps
    in listep' (MS.sort LU.liOrd (T.pos term))
    end

(* Option.isNone が MLton だと使えないようなので *)
(* fun isNF rs term = Option.isNone (listep rs term) *)
fun isNF rs term = listep rs term = NONE

(*
fun listepsToNF rs term =
    let val _ = print "    "
	fun listepsToNF' term' =
	    case listep rs term' of
		SOME t => (print (T.toString term' ^ "\n->R "); listepsToNF' t)
	      | NONE => print (T.toString term' ^ "\n")
    in listepsToNF' term
    end
*)

fun listepsToNF rs term =
    let fun listepsToNF' term' acc =
            case listep rs term' of
                SOME t => listepsToNF' t (acc ^ T.toString term' ^ "\n->R ")
              | NONE => acc ^ T.toString term' ^ "\n"
        val msg = listepsToNF' term "    "
        val _ = print msg
    in msg
    end

fun linf rs term = case listep rs term of
			      SOME t => linf rs t
			    | NONE => term

fun isJoinable rs (term1, term2) =
    let fun rewrittenTerms term =
	    foldr (fn (p, ts) => case step rs term p of
				     SOME t => LU.union (rewrittenTerms t, ts)
				   | NONE => ts) [term] (Term.pos term)
    in LU.intersection (rewrittenTerms term1, rewrittenTerms term2) <> []
    end
	
end (* of local *)

end
