(* file: cr.sml *)
(* description: confluence *)
(* author: Masaki Haga *)

signature CR = 
sig 
    val cpkRule: bool -> Term.term * Term.term -> Term.term * Term.term -> (Term.term * Term.term * Term.term) list
    val cpk: (Term.term * Term.term) list  -> (Term.term * Term.term * Term.term) list
    val checkConfluenceByKnuthBendix: Trs.trs -> (Trs.trs -> bool) -> bool
end

structure Cr: CR =
struct 

local 
    structure L = List
    structure LU = ListUtil
    structure T = Term
    structure RW = Rewrite
in

fun cpkRule isIdentical (l1,r1) (l2,r2) =
    let fun cpkTerm (T.Var _) = []
	  | cpkTerm (t as T.Fun (f,ts)) =
	    let val atRoot = case Subst.unify (t,l1) of
				 SOME sigma => [(fn x => x, sigma)] (* (Context (root), Subst) *)
			       | NONE => []
		val atArgs = L.map (fn (C,sigma) => (fn x => T.Fun (f,C x), sigma)) (cpkTermList [] ts)
	    in atRoot @ atArgs
	    end
	and cpkTermProper (T.Var _) = []
	  | cpkTermProper (t as T.Fun (f,ts)) = L.map (fn (C,sigma) => (fn x => T.Fun (f,C x), sigma)) (cpkTermList [] ts)
	and cpkTermList _ [] = []
	  | cpkTermList us (t::ts) = 
	    let val atHead = L.map (fn (C, sigma) => (fn x => us @ C x :: ts, sigma)) (cpkTerm t)
		val atTail = cpkTermList (us @ [t]) ts
	    in atHead @ atTail
	    end
    in L.map (fn (C,sigma) => (Subst.apply sigma (C r1),
			       Subst.apply sigma l2,
			       Subst.apply sigma r2)) (* CPK *)
             (if isIdentical 
	      then (cpkTermProper l2)
	      else (cpkTerm l2))
    end


fun cpk rs = LU.mapAppend (fn lr1 => 
			     (LU.mapAppend (fn lr2 => 
					       let val (lr1', lr2') = Trs.rename (lr1, lr2)
					       in cpkRule (lr1 = lr2) lr1' lr2'
					       end) rs))
			  rs

fun checkConfluenceByKnuthBendix rs checkSN =
    checkSN rs andalso L.all (fn (v1, u, v2) => RW.isJoinable rs (v1, v2)) (cpk rs)

end (* of local *)

end (* of struct *)
