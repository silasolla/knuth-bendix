(* file: path_order.sml *)
(* description: path orders for termination check *)
(* author: Masaki Haga *)

signature PATH_ORDER = 
sig
    type prec = Fun.key * Fun.key -> bool
    type fprec = Fun.key * Fun.key -> Formula.prop
    val rdPrec: (Fun.key * int) list -> prec
    val lpoGt: prec -> Term.term * Term.term -> bool
    val isLpoDecreasingTrs: prec -> Trs.trs -> bool
    val encodeLpoGt: fprec -> Term.term * Term.term -> Formula.prop
    val checkLpoTermination: Trs.trs -> bool
end

structure PathOrder: PATH_ORDER =
struct

local
    structure AL = AssocList
    structure L = List
    structure LP = ListPair
    structure LU = ListUtil
    structure T = Term
    structure F = Formula
in

type prec = Fun.key * Fun.key -> bool

type fprec = Fun.key * Fun.key -> Formula.prop

fun rdPrec wlist (f,g) =
    let fun weights h = case AL.find h wlist of SOME w => w
					      | NONE => 0
    in Int.> (weights f, weights g)
    end

(* 辞書式経路順序による比較 *)
fun lpoGt prec (term1,term2) = 
    let (* 辞書式拡張 *)
	fun lex rel ([],[]) = false
	  | lex rel (x::xs,y::ys) = if x = y then lex rel (xs,ys)
				    else rel (x, y)
	  | lex rel _ = raise Fail "Error: lex compares lists having different length"
	fun gt (T.Var _, _) = false
	  | gt (T.Fun (f,ss), t as (T.Var _)) = geqforsome ss t
	  | gt (s as (T.Fun (f,ss)), t as (T.Fun (g,ts))) =
	    if f = g
	    then geqforsome ss t orelse (lex gt (ss,ts) andalso gtforall s ts)
	    else geqforsome ss t orelse (prec (f,g) andalso gtforall s ts) 
	and geq (s,t) = s = t orelse gt (s,t)
	and gtforall s ts = L.all (fn ti => gt (s,ti)) ts
	and geqforsome ss t = L.exists (fn si => geq (si,t)) ss
    in gt (term1,term2)
    end

(* 辞書式経路順序による減少性判定 *)
fun isLpoDecreasingTrs prec rs = L.all (lpoGt prec) rs

fun encodeLpoGt prec (term1, term2) =
    let (* 辞書式拡張 *)
	fun lex rel ([],[]) = F.False
	  | lex rel (x::xs,y::ys) = if x = y then lex rel (xs,ys)
				    else rel (x, y)
	  | lex rel _ = raise Fail "Error: lex compares lists having different length"
	fun gt (T.Var _, _) = F.False
	  | gt (T.Fun (f,ss), t as (T.Var _)) = geqforsome ss t
	  | gt (s as (T.Fun (f,ss)), t as (T.Fun (g,ts))) =
	    if f = g then F.Or (geqforsome ss t, F.And (lex gt (ss,ts), gtforall s ts))
	    else F.Or (geqforsome ss t, F.And (prec (f,g), gtforall s ts))
	and geq (s,t) = if s = t then F.True else gt (s,t)
	and gtforall s ts = F.Conj (L.map (fn ti => gt (s,ti)) ts)
	and geqforsome ss t = F.Disj (L.map (fn si => geq (si,t)) ss)
    in gt (term1,term2)
    end

fun checkLpoTermination rs =
    let open Formula
	(* 函数記号と重み変数のテーブル *)
	val funs = L.foldr (fn ((l, r), fs) =>
			       LU.union (T.funs l, LU.union (T.funs r, fs))) [] rs
	val vars = L.tabulate (L.length funs, fn x => Var x)
	val table = LP.zip (funs, vars)
	fun lookup f = valOf (AL.find f table)
	fun prec (f, g) = Atom (Gt (lookup f, lookup g))

	(* LPO termination のエンコーディング *)
	val propGe0 = Conj (L.map (fn (_, x) => Atom (Ge (x, Const 0))) table)
	val propDist = Atom (Distinct (L.map (fn (_, x) => x) table))
	val propLpo = Conj (L.map (fn (s, t) => encodeLpoGt prec (s, t)) rs)
	val prop = Conj [propGe0, propDist, propLpo]

	(* assert formulas *)
	fun lpoTerminationInquiry outs =
	    let val _ = L.app (fn x => TextIO.output (outs, Yices2.defineIntVar x))
			      (L.tabulate (L.length funs, fn x => x))
		val _ = TextIO.output (outs, Yices2.assertProp prop)
		val _ = TextIO.output (outs, "(check)\n");
		val _ = TextIO.output (outs, "(show-model)\n");
	    in ()
	    end
    in case Yices2.runSolver lpoTerminationInquiry of
	   SOME assig =>
	   let val _ = L.app (fn (f, x) =>
				 print (f ^ ":" ^
					(Int.toString (valOf (AL.find (Yices2.prArith x)
								      assig))) ^ "\n"))
			     table
	   in true
	   end
	 | NONE => false
    end

end (* of local *)

end (* of struct *)
