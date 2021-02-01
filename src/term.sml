(* file: term.sml *)
(* description: first-order terms *)
(* author: Masaki Haga *)

signature TERM = 
sig 
    type var_key = Var.key
    type fun_key = Fun.key
    datatype term = Var of var_key | Fun of fun_key * term list
    type position = int list
    exception PositionNotInTerm
    type context = term -> term

    val root: term -> Symbol.symbol
    val args: term -> term list
    val isVar: term -> bool
    val isFun: term -> bool
    val vars: term -> var_key list
    val funs: term -> fun_key list
    val size: term -> int

    val prPos: position -> string
    val pos: term -> position list
    val symbol: term -> position -> Symbol.symbol
    val subterm: term -> position -> term

    val replaceSubterm: term -> position -> term -> term
    val makeContext: term -> position -> context
    val fillContext: context -> term -> term

    val toString: term -> string
    val fromString: string -> term

    val isConstructor: fun_key list -> term -> bool
    val isBasic: fun_key list * fun_key list -> term -> bool
    val findBasicSubtermLiOc: fun_key list * fun_key list -> term -> (context * term) option
    
    val cropId: string -> (string * string) option
    val cropTerm: string -> (term * string) option
    val cropKeySeparatedTermPair: string -> string -> ((term * term) * string) option
    val readKeySeparatedTermPair: string -> string -> (term * term)
    val readMultipleKeySepartedTermPairs: string * string * string -> string -> string -> (term * term) list
end

structure Term : TERM =
struct

local
    structure L = List
    structure LP = ListPair
    structure LU = ListUtil
    structure SU = StringUtil
    structure S = Symbol
    structure MS = ListMergeSort
in

type var_key = Var.key
type fun_key = Fun.key
datatype term = Var of var_key | Fun of fun_key * term list
type position = int list
exception PositionNotInTerm
type context = term -> term

fun root (Var x) = S.V x
  | root (Fun (f,ts)) = Symbol.F f

fun args (Var x) = []
  | args (Fun (f,ts)) = ts

fun isVar (Var x) = true
  | isVar (Fun (f,ts)) = false

fun isFun (Var x) = false
  | isFun (Fun (f,ts)) = true

fun vars (Var x) = [x]
  | vars (Fun (f,ts)) = foldr LU.union [] (foldr (fn (z,zs) => vars z :: zs) [] ts)

fun funs (Var x) = []
  | funs (Fun (f,ts)) = LU.add f (foldr (fn (z,zs) => LU.union (funs z,zs)) [] ts)

fun size (Var x) = 1
  | size (Fun (f,ts)) = 1 + foldr (fn (t,sum) => size t + sum) 0 ts

fun prPos [] = "e"
  | prPos (posHd::posTl) =
    foldl (fn (z,zs) => zs ^ "." ^ Int.toString z) (Int.toString posHd) posTl

fun pos (Var x) = [[]]
  | pos (Fun (f,ts)) = [] :: (L.foldri (fn (i,z,zs) => map (fn w => (i+1) :: w) (pos z) @ zs) [] ts)

fun symbol (Var x) [] = S.V x
  | symbol (Var x) _ = raise PositionNotInTerm
  | symbol (Fun (f,ts)) [] = S.F f
  | symbol (Fun (f,ts)) (posHd :: posTl) = if L.length ts < posHd
					 then raise PositionNotInTerm
					 else symbol (L.nth (ts, posHd - 1)) posTl

fun subterm t [] = t
  | subterm (Var x) _ = raise PositionNotInTerm
  | subterm (Fun (f,ts)) (posHd :: posTl) = if L.length ts < posHd
					    then raise PositionNotInTerm
					    else subterm (L.nth (ts, posHd - 1)) posTl

fun replaceSubterm t [] u = u
  | replaceSubterm (Var x) p u = raise PositionNotInTerm
  | replaceSubterm (Fun (f,ts)) (posHd :: posTl) u =
    if L.length ts < posHd
    then raise PositionNotInTerm
    else Fun (f, LU.nthMap (posHd - 1) (fn t' => replaceSubterm t' posTl u) ts)

fun makeContext t p = replaceSubterm t p

(*
fun makeContext t [] = (fn x => x)
  | makeContext (Var x) p = raise PositionNotInTerm
  | makeContext (Fun (f,ts)) (posHd :: posTl) =
    if L.length ts < posHd
    then raise PositionNotInTerm
    else (fn x => Fun (f, LU.nthMap (posHd - 1) ((fn t' => makeContext t' posTl) x) ts))
*)
				     
fun fillContext c t = c t

fun toString (Var x) =  "?" ^ (Var.toString x)
  | toString (Fun (f,[])) = (Fun.toString f)
  | toString (Fun (f,ts)) = (Fun.toString f) ^ "(" ^ (toStringList ts)
and toStringList [] = "" (* does not reach here *)
  | toStringList ([t]) = (toString t) ^ ")"
  | toStringList (t::ts) = (toString t) ^ "," ^ (toStringList ts)

fun isConstructor c t = L.all (fn f => LU.member f c) (funs t)

fun isBasic (_,_) (Var _) = false
  | isBasic (d,c) (Fun (f,ts)) = L.all (isConstructor c) ts andalso LU.member f d

fun findBasicSubtermLiOc (d,c) t =
    let fun main [] = NONE
	  | main (p::ps) =
	    let val t' = subterm t p
	    in if isBasic (d,c) t' then SOME (makeContext t p,t') else main ps
	    end
    in main (MS.sort LU.liOrd (pos t))
    end

structure TermSpecialSymbols = struct val special = [#"(", #")", #","]  end
structure TermLex = Lexical (TermSpecialSymbols)
structure TermParsing = Parsing (TermLex)

local
    fun makeFun (id, ts) = (case Symbol.fromString id of
			    Symbol.F f => Fun (f, ts)
			  | Symbol.V _ => raise Fail "Syntax error: function symbol expected")

    fun makeList (t, ts) = t::ts

    fun makeList1 t = t::nil

    fun makeAtom id  = (case Symbol.fromString id of
			    Symbol.F c => Fun (c, []) 
			  | Symbol.V x => Var x)

    open TermParsing
    infix 6 $--
    infix 5 --
    infix 3 >>
    infix 0 ||

    fun term toks =
        ( id --  "(" $-- termlist >> makeFun || atom ) toks
    and termlist toks =
        ( termseq -- $ ")" >> #1 ) toks
    and termseq toks =
        ( term -- "," $-- termseq >> makeList || term >> makeList1 ) toks
    and atom toks  =
        ( id >> makeAtom ) toks
in

fun fromString str = reader term str

end (* of local *)

fun cropId str = TermLex.cropId str

fun cropTerm str = case cropId str of
		       NONE => NONE
		     | SOME (id,body) => let val (init, rest) = SU.scanBalancedPar body
					     val t = fromString (id ^ init)
					 in SOME (t, rest) end

fun cropKeySeparatedTermPair key str 
    = case cropTerm str of
	  NONE => NONE
	| SOME (lhs, str1) => (case SU.scanKey key str1 of 
				   NONE => raise Fail ("Syntax error: " ^ key ^ " expected after term" )
				 | SOME str2 => (case cropTerm str2 of
						     NONE => raise Fail ("Syntax error: term expected after " ^ key)
						   | SOME (rhs, rest) => SOME ((lhs,rhs), rest)))


(* term key term の形の文字列から，項の対を読み込む *)
fun readKeySeparatedTermPair key str = 
    case cropKeySeparatedTermPair key str of
	SOME (tp,rest) => if SU.ending rest then tp
			  else raise Fail ("Syntax error: trailing " ^ rest)
      | NONE => raise Fail ("Syntax error: not a " ^ key ^ " separated term pair")

(* start term key term sep ... sep term key term stop の形の文字列から *)
(* 項の対のリストを読み込む *)
fun readMultipleKeySepartedTermPairs (start,sep,stop) key str =
    let fun rdFirstItem s = cropKeySeparatedTermPair key s
	fun rdRemainingItems ans s = 
	    case SU.scanKey sep s of
		SOME rest => (case rdFirstItem rest of 
				 SOME (new,s2) => rdRemainingItems (new::ans) s2
			       | NONE => raise Fail ("Syntax error: starting term pair expected " ^ rest))
	      | NONE => if SU.ending s
			then rev ans
			else raise Fail ("Syntax error: trailing " ^ s)
    in case SU.scanBeginEnd (start,stop) str of
	   NONE => raise Fail ("Syntax error: " ^ start ^ "..." ^ stop ^ " expected")
	 | SOME str1 => (case rdFirstItem str1 of 
			    NONE => []
			  | SOME (tp,rest) => rdRemainingItems [tp] rest)
    end

end (* of local *)		     

end
