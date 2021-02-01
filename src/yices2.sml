(* file: yices2.sml *)
(* description: utility functions for Yices2 *)
(* author: Masaki Haga *)

signature YICES2 =
sig
    val runSolver: (TextIO.outstream -> unit) -> (string * int) list option
    val test: unit -> (string * int) list option

    val prArith: Formula.arith -> string
    val prAtom: Formula.atom -> string
    val prProp: Formula.prop -> string
    val defineIntVar: int -> string
    val assertProp: Formula.prop -> string
    val test2: unit -> (string * int) list option
end

structure Yices2 : YICES2 =
struct

local
    structure F = Formula
    structure SS = Substring
in

val pathOfTool = "/usr/bin/yices"
val yesAnswer = "sat\n"

(* Yicesの出力を行毎に読み取る *)
fun receiveReport ins lines = 
    case TextIO.inputLine ins of
	SOME ln => receiveReport ins (ln::lines)
      | NONE => List.rev lines

(* 変数への割当の読み取り *)
fun readIntAssign line =
    let val toks = SS.tokens (Char.contains " ()\n") (SS.full line)
    in case toks of 
           [x,y,z] => (case (SS.string x, Int.fromString (SS.string z)) of 
			  ("=", SOME num) => SOME (SS.string y, num)
			| _ => NONE)
	 | _ => NONE
    end

fun runSolver mkquiry =
    let 
	(* Yices のよびだし *)
	val proc = Unix.execute (pathOfTool, [])   

	(* Yices への入力 *)
	val outs = Unix.textOutstreamOf proc (* コマンド出力用ストリームの準備 *)
	val _ = mkquiry outs  (* 入力実行の関数をよびだし *)
	val _ = TextIO.closeOut outs (* コマンド出力用ストリームの後処理 *)

	(* Yices からの出力 *)
	val ins = Unix.textInstreamOf proc (* 結果受け取リ用ストリームの準備 *)
	val ans = TextIO.inputLine ins (* ツール出力の先頭行の読み込み *)
	val result =  (isSome ans) andalso (valOf ans = yesAnswer) (* 結果判定 *)
	val _ = print (valOf ans)
	val reports = if result then receiveReport ins [] else [] (* モデルの読み取り *)
	val _ = TextIO.closeIn ins (* 結果受け取り用ストリームの後処理 *)
	val _ = Unix.reap proc (* プロセス終了処理 *)

	(* 変数割り当ての読み取り *)
	val ans = if result
		  then SOME (List.mapPartial readIntAssign reports)
		  else NONE
    in ans
    end

(* Yicesへのコマンド入力 *)
fun testInquiry outs =
    let
	val _ = TextIO.output (outs, "(define x::int)\n");
	val _ = TextIO.output (outs, "(define y::int)\n");
	val _ = TextIO.output (outs, "(define z::int)\n");
	val _ = TextIO.output (outs, "(assert (>= x 0))\n");
	val _ = TextIO.output (outs, "(assert (>= y 0))\n");
	val _ = TextIO.output (outs, "(assert (>= z 0))\n");
	val _ = TextIO.output (outs, "(assert (and (< (+ x y) 8) (< (+ y z) 6) (< (+ x y) 5)))\n");
	val _ = TextIO.output (outs, "(assert (ite (> (- x y) 3) (<= (+ y z) x) (<= (+ x z) y)))\n");
	val _ = TextIO.output (outs, "(assert (distinct x y z))\n");
	val _ = TextIO.output (outs, "(check)\n");
	val _ = TextIO.output (outs, "(show-model)\n");
    in ()
    end

fun test () = runSolver testInquiry 

fun prArith (F.Var i) = if i >= 0 
			then "x" ^ (Int.toString i)
			else "y" ^ (Int.toString (~i))
  | prArith (F.Const i) = if i >= 0
			  then (Int.toString i)
			  else ("(- 0 " ^ (Int.toString (~i)) ^ ")")
  | prArith (F.Neg e1) = "(- 0 " ^ (prArith e1) ^ ")"
  | prArith (F.Add (e1,e2)) = "(+ " ^ (prArith e1) ^ " " ^ (prArith e2) ^ ")"
  | prArith (F.Sub (e1,e2)) = "(- " ^ (prArith e1) ^ " " ^(prArith e2) ^ ")"
  | prArith (F.Mul (e1,e2)) = "(* " ^ (prArith e1) ^ " " ^(prArith e2) ^ ")"
  | prArith (F.Sum []) = "0"
  | prArith (F.Sum es) = "(+ " ^ (List.foldr (fn (e,str) => " " ^ (prArith e) ^ str) ")" es)
  | prArith (F.Prod []) = "1"
  | prArith (F.Prod es) = "(* " ^ (List.foldr (fn (e,str) => " " ^ (prArith e) ^ str) ")" es)
				      
fun prAtom (F.Eq (p,q)) = "(= " ^ (prArith p) ^ " " ^ (prArith q) ^ ")"
  | prAtom (F.Neq (p,q)) = "(/= " ^ (prArith p) ^ " " ^ (prArith q) ^ ")"
  | prAtom (F.Ge (p,q)) = "(>= " ^ (prArith p) ^ " " ^ (prArith q) ^ ")"
  | prAtom (F.Le (p,q)) = "(<= " ^ (prArith p) ^ " " ^ (prArith q) ^ ")"
  | prAtom (F.Gt (p,q)) = "(> " ^ (prArith p) ^ " " ^ (prArith q) ^ ")"
  | prAtom (F.Lt (p,q)) = "(< " ^ (prArith p) ^ " " ^ (prArith q) ^ ")"
  | prAtom (F.Distinct []) = raise Fail "Error: prAtom: Distinct needs at least two arguments"
  | prAtom (F.Distinct [e]) = raise Fail "Error: prAtom: Distinct needs at least two arguments"
  | prAtom (F.Distinct es) = "(distinct " ^ (List.foldr (fn (e,str) => " " ^ (prArith e) ^ str) ")" es)

fun prProp (F.True) = "true"
  | prProp (F.False) = "false"
  | prProp (F.Atom a) = prAtom a
  | prProp (F.Not p) = "(not " ^ (prProp p) ^ ")"
  | prProp (F.And (p,q)) = "(and " ^ (prProp p) ^ " " ^ (prProp q) ^ ")"
  | prProp (F.Or (p,q)) = "(or " ^ (prProp p) ^ " " ^ (prProp q) ^ ")"
  | prProp (F.Conj []) = "true"
  | prProp (F.Conj ps) = "(and " ^ (List.foldr (fn (p,str) => " " ^ (prProp p) ^ str) ")" ps)
  | prProp (F.Disj []) = "false"
  | prProp (F.Disj ps) = "(or " ^ (List.foldr (fn (p,str) => " " ^ (prProp p) ^ str) ")" ps)
  | prProp (F.Imp (p,q)) = "(=> " ^ (prProp p) ^ " " ^ (prProp q) ^ ")"
  | prProp (F.Iff (p,q)) = "(<=> " ^ (prProp p) ^ " " ^ (prProp q) ^ ")"
  | prProp (F.IfThenElse (p,q,r)) = "(ite " ^ (prProp p) ^ " "  ^ (prProp q) ^ " "  ^ (prProp r) ^ ")"


fun defineIntVar i = "(define " ^ (prArith (F.Var i)) ^ "::int)\n"
fun assertProp prop = "(assert " ^ (prProp prop) ^ ")\n"

fun testInquiryViaEncoding outs =
    let open Formula
	val x = Var 0
	val y = Var 1
	val z = Var 2
	val zero = Const 0
	val condVar = Conj [Atom (Ge (x, Const 0)), Atom (Ge (y, Const 0)), Atom (Ge (z, Const 0))]
	val cond1 = Conj [Atom (Lt (Add (x,y), Const 8)), Atom (Lt (Add (y,z), Const 6)), Atom (Lt (Add (x,y), Const 5))]
	val cond2 = IfThenElse  (Atom (Gt (Sub (x,y), Const 3)), Atom (Le (Add (y,z), x)), Atom (Le (Add (x,z), y)))
	val cond3 = Atom (Distinct [x,y,z])
	val prop = Conj [condVar, cond1, cond2, cond3]
	val _ = TextIO.output (outs, defineIntVar 0)
	val _ = TextIO.output (outs, defineIntVar 1)
	val _ = TextIO.output (outs, defineIntVar 2)
	val _ = TextIO.output (outs, assertProp prop)
	val _ = TextIO.output (outs, "(check)\n");
	val _ = TextIO.output (outs, "(show-model)\n");
    in ()
    end

fun test2 () = runSolver testInquiryViaEncoding

end (* of local *)

end (* of struct *)
