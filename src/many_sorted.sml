(* file: many_sorted.sml *)
(* description: utility for many-sorted term rewriting systems *)
(* author: Masaki Haga *)

signature MANY_SORTED = 
sig 

    eqtype sort_key
    type sort_spec = sort_key list * sort_key 
    type ms_fun = Fun.key * sort_spec
    type ms_sign = ms_fun list

    val rdSort: string -> sort_key
    val prSort: sort_key -> string

    val rdSortSpec: string -> sort_spec
    val prSortSpec: sort_spec -> string

    val rdMsFun: string -> ms_fun
    val prMsFun: ms_fun -> string

    val sortOfFun: ms_sign -> Fun.key -> sort_spec

    type ms_term = Term.term * sort_key
    val sortOfMsTerm: ms_term -> sort_key
    val dropSortInMsTerm: ms_term -> Term.term

    type sort_env = (Var.key * sort_key) list
    val sortInference: ms_sign -> ms_term list -> sort_env option

    exception NotWellSorted
    val sortAssignInMsTerm: ms_sign -> ms_term -> sort_env
    val sortAssignInMsTerms: ms_sign -> ms_term list -> sort_env
    val isWellSortedMsTerm: ms_sign -> ms_term -> bool

    type ms_rule = (Term.term * Term.term) * sort_key
    type ms_rules = ms_rule list
    type ms_eq = ms_rule
    type ms_eqs = ms_rules

    val sortOfMsRule: ms_rule -> sort_key
    val dropSortInMsRule: ms_rule -> Trs.rule
    val dropSortInMsRules: ms_rules -> Trs.trs

    val sortAssignInMsRule: ms_sign -> ms_rule -> sort_env
    val isWellSortedMsRule: ms_sign -> ms_rule -> bool

    val linf: ms_rules -> ms_term -> ms_term

    val complementOfLCTerm: ms_sign -> Term.term -> Term.term list
    val complementOfLCSubst: ms_sign -> Subst.subst -> Subst.subst list
(*  val patternDifference: ms_sign -> Term.term list * Term.term list -> Term.term list
    val isQuasiReducibleCs: ms_sign -> (Term.term * Term.term) list -> bool

    val test: int -> Term.term list
    val test2: int -> Subst.subst list
    val test3: int -> Term.term list
    val test4: int -> bool *)
end

structure ManySorted : MANY_SORTED =
struct
local 
    structure AL = AssocList
    structure L = List
    structure LP = ListPair
    structure LU = ListUtil
    structure SU = StringUtil
    structure T = Term
in

type sort_key = string
type sort_spec = sort_key list * sort_key 
type ms_fun = Fun.key * sort_spec
type ms_sign = ms_fun list

type ms_term = T.term * sort_key

type ms_rule = (T.term * T.term) * sort_key
type ms_rules = ms_rule list
type ms_eq = (T.term * T.term) * sort_key
type ms_eqs = ms_eq list

fun prSort str = str
fun prSortSpec ([],s) = prSort s
  | prSortSpec (ws,s) = (LU.toStringCommaSpace prSort ws) ^ " -> " ^ (prSort s)
fun prMsFun (f,spec) = (Fun.toString f) ^ " : " ^ (prSortSpec spec)

fun rdSort str = str

structure SortSpecialSymbols = struct val special = [#",",#"-",#">",#":"]  end
structure SortLex = Lexical (SortSpecialSymbols)
structure SortParsing = Parsing (SortLex)

local
    fun makePair1 s = ([],s)
    fun makeList (s,ws) = s::ws
    fun makeList1 s = s::nil

    open SortParsing
    infix 6 $--
    infix 5 --
    infix 3 >>
    infix 0 ||

    fun msfun toks =
        ( id --  ":" $-- sortspec) toks
    and sortspec toks =
        ( sortseq --  "-" $-- (">" $-- id) || id >> makePair1 ) toks
    and sortseq toks =
        ( id -- "," $-- sortseq >> makeList || id >> makeList1 ) toks

in 
fun rdMsFun str = reader msfun str
fun rdSortSpec str = reader sortspec str
end (* of local *)

fun sortOfFun sign f = case AL.find f sign of 
			   SOME spec => spec 
			 | NONE => raise Fail "Error: ManySorted.sortOfFun: function without sort specification"

fun sortOfMsTerm (t,ty) =  ty
fun dropSortInMsTerm (t,ty) = t

type sort_env = (Var.key * sort_key) list
fun sortInference sign cnstr = 
    let fun main [] env = SOME env
	  | main ((t,ty)::rest) env = 
            case t of
		T.Var x => (case AL.add (x,ty) env of SOME env2 => main rest env2 | NONE => NONE)
              | T.Fun (f,ts) => let val (ws,s) = sortOfFun sign f 
				in if ty = s then main ((LP.zip (ts, ws)) @ rest) env  else NONE
				end
    in main cnstr []
    end

exception NotWellSorted
fun sortAssignInMsTerms sign cnstr = case sortInference sign cnstr of
				       NONE => raise NotWellSorted | SOME env => env
fun sortAssignInMsTerm sign (term, ty) = sortAssignInMsTerms sign [(term,ty)]
fun isWellSortedMsTerm sign (term,ty) = isSome (sortInference sign [(term,ty)])

fun sortOfMsRule ((l,r),ty) = ty
fun dropSortInMsRule ((l,r),ty) = (l,r)
fun dropSortInMsRules rs = L.map dropSortInMsRule rs

fun sortAssignInMsRule sign ((l,r),ty) = sortAssignInMsTerms sign [(l,ty),(r,ty)]
fun isWellSortedMsRule sign ((l,r),ty) = isSome (sortInference sign [(l,ty),(r,ty)])

fun linf rs (t,ty) = (Rewrite.linf (dropSortInMsRules rs) t, ty)

(* rename variable occurrences to x_1,x_2,... from leftmost innermost one *)
(* used in complementation algorithm below *)
fun normalizeVarIndex t =
    let val idx = ref 0
	fun newVar () = (idx := (!idx) + 1; T.Var ("x",!idx))
	fun ren (T.Var _) = newVar ()
	  | ren (T.Fun (f,ts)) = let val ts' = renList ts in T.Fun (f,ts') end
	and renList [] = []
	  | renList (t::ts) = let val t' = ren t
				  val ts' = renList ts
			      in t'::ts' end
    in ren t end

(* complement of linear constructor term *)
(* we assume that t is linear constructor term *)
(* the var index of resulting terms are normalized *)
fun complementOfLCTerm csign t = 
    let (* we first use a variable "x" conventionally  *)
	fun makeVarTerms len  = L.tabulate (len, fn n=> T.fromString("?x"))
	fun compl (T.Var x) = []
	  | compl (t as T.Fun (f,ts)) =
	    let val (_,ty) = sortOfFun csign f
		val fromRoot = L.map (fn (c,(ws,s)) => T.Fun (c, makeVarTerms (L.length ws))) 
				     (L.filter (fn (c,(ws,s)) => s = ty andalso c <> f) csign)
		val fromArgs = L.map (fn us => T.Fun (f,us)) (complList ts)
	    in fromRoot @ fromArgs end
	and complList [] = []
	  | complList (t::ts) = 
	    let val fromHead = L.map (fn u => u::makeVarTerms (L.length ts)) (compl t)
		val fromTail = L.map (fn us => t::us) (complList ts)
	    in fromHead @ fromTail end
    in L.map normalizeVarIndex (compl t) end


(* we assume that the 2nd argment is a linear constructor substitution *)
(* the var index of the terms in range are normalized (hence they might not be disjoint!)  *)
fun complementOfLCSubst csign sigma = 
    let fun main [] = [[]]
          | main ((x,t)::rest) =
	    let val subs = main rest
		val t' = normalizeVarIndex t
		val compls = complementOfLCTerm csign t'
	    in ListXProd.mapX (fn (u,rho) => (x,u)::rho) (t'::compls, subs) 
	    end
    in tl (main sigma)  (* take tail to remove sigma itself *)
    end 
			      
(* we assume that pattern is a set of linear basic term *)
(*fun patternDifference csign (pat1,pat2) = 
    let exception UnifiedWith of T.term * T.term * T.term * T.term * Subst.subst 
	val ans = (ListXProd.appX (fn (u,v) => let val [u',v'] = T.renameDisjointly [u,v]
					    in case Subst.unify (u',v') of 
						   SOME sigma => raise UnifiedWith (u,v,u',v',sigma)
						 | NONE => ()
					    end) (pat1, pat2); NONE)
		handle UnifiedWith (u,v,u',v',sigma) => SOME (u,v,u',v',sigma)
    in case ans of 
	   NONE => L.map normalizeVarIndex pat1
	 | SOME (u,v,u',v',sigma) => let val pat1' = LU.delete u pat1
					 val pat2' = LU.delete v pat2
					 val compls = complementOfLCSubst csign sigma
					 val cmp1 = L.map (fn rho=> normalizeVarIndex (Subst.apply rho u')) compls
					 val cmp2 = L.map (fn rho=> normalizeVarIndex (Subst.apply rho v')) compls
					 val w = normalizeVarIndex (Subst.apply sigma u')
					 (* deletion and duplication elimination work, due to normalization *)
					 val add1 = LU.delete w (LU.elimDupl cmp1) 
					 val add2 = LU.delete w (LU.elimDupl cmp2)
				     in patternDifference csign (pat1'@add1, pat2'@add2)
				     end
    end

(* we assume that rs is a constructor system *)
fun isQuasiReducibleCs sign rs =
    let val dfuns = LU.elimDupl (L.mapPartial (fn (l,r)=> case T.root l of Symbol.F f => SOME f | _ => NONE) rs)
	val dsign = L.filter (fn (f,_)  => LU.member f dfuns) sign
	val csign = L.filter (fn (f,_) => not (LU.member f dfuns)) sign
	fun makeVarTerms len  = L.tabulate (len, fn n => T.Var ("?x",n))
	fun check f = let val (ws,_) = sortOfFun dsign f
			  val pat1 = [ T.Fun (f,makeVarTerms (L.length ws)) ]
			  val pat2 = L.mapPartial (fn (l,r) => if T.root l = Symbol.F f then SOME l else NONE) rs
		      in null (patternDifference csign (pat1,pat2))
		      end
    in List.all check dfuns
    end


local
    val Nat = rdSort "Nat"
    val List = rdSort "List"
(*    val sigNat = [(Fun.fromString "s", ([Nat],Nat)), (Fun.fromString "0", ([],Nat))] *)
    val sigNat = L.map rdMsFun ["s:Nat->Nat", "0:Nat" ]
    val sigListt = L.map rdMsFun ["cons:Nat,List->List", "nil:List" ]
    val sigList = [(Fun.fromString "cons", ([Nat,List],List)), (Fun.fromString "nil", ([],List))]
    val csign = sigNat @ sigList
    val sigPlus = [(Fun.fromString "+", ([Nat,Nat],Nat)), 
		   (Fun.fromString "s", ([Nat],Nat)), (Fun.fromString "0", ([],Nat))]
in
    fun test 1 = complementOfLCTerm csign (T.fromString "0")
      | test 2 = complementOfLCTerm csign (T.fromString "s(?x)")
      | test 3 = complementOfLCTerm csign (T.fromString "s(s(?x))")
      | test 4 = complementOfLCTerm csign (T.fromString "s(0)")
      | test 5 = complementOfLCTerm csign (T.fromString "cons(0,?xs)")
      | test 6 = complementOfLCTerm csign (T.fromString "cons(?x,nil)")
      | test 7 = complementOfLCTerm csign (T.fromString "cons(s(?x),?ys)")
      | test _ = []

    fun test2 1 = complementOfLCSubst csign (Subst.fromString "{ ?x := s(?y) }")
      | test2 2 = complementOfLCSubst csign (Subst.fromString "{ ?x := s(0), ?xs := nil }")
      | test2 _ = []

    fun test3 1 = patternDifference csign ([T.fromString "+(?x,?y)"], [T.fromString "+(0,0)"])
      | test3 2 = patternDifference csign ([T.fromString "+(?x,?y)"], [T.fromString "+(0,?y)", T.fromString "+(s(?x),?y)"])
      | test3 3 = patternDifference csign ([T.fromString "+(?x,?y)"], [T.fromString "+(?x,0)", T.fromString "+(?x,s(?y))"])
      | test3 4 = patternDifference csign ([T.fromString "+(?x,?y)"], [T.fromString "+(?x,0)"])
      | test3 5 = patternDifference csign ([T.fromString "+(?x,?y)"], [T.fromString "+(s(?x),?y)"])
      | test3 6 = patternDifference csign ([T.fromString "+(?x,?y)"], [T.fromString "+(?x,0)", T.fromString "+(s(?x),?y)"])
      | test3 7 = patternDifference csign ([T.fromString "+(?x,?y)"], [T.fromString "+(0,0)", T.fromString "+(0,s(?y))", 
									  T.fromString "+(s(?x),?y)"])
      | test3 8 = patternDifference csign ([T.fromString "+(?x,?y)"], [T.fromString "+(0,?y)"])
      | test3 9 = patternDifference csign ([T.fromString "+(?x,?y)"], [T.fromString "+(0,?y)", T.fromString "+(?x,0)"])
      | test3 10 = patternDifference csign ([T.fromString "+(?x,?y)"], [T.fromString "+(0,?y)", T.fromString "+(?x,0)", 
									  T.fromString "+(s(?x),s(?y))"])
      | test3 _ = []

    fun test4 1 = isQuasiReducibleCs sigPlus (L.map Trs.rdRule ["+(0,?y) -> ?y","+(s(?x),?y) -> s(+(?x,?y))"])
      | test4 2 = isQuasiReducibleCs sigPlus (L.map Trs.rdRule ["+(0,0) -> 0","+(s(?x),?y) -> s(+(?x,?y))"])
      | test4 3 = isQuasiReducibleCs sigPlus (L.map Trs.rdRule ["+(0,?y) -> ?y","+(?x,s(?y)) -> s(+(?x,?y))"])
      | test4 4 = isQuasiReducibleCs sigPlus (L.map Trs.rdRule ["+(0,?y) -> ?y","+(?x,0) -> ?x", "+(s(?x),s(?y)) -> s(s(+(?x,?y)))"])
      | test4 _ = true

end
*)

end (* of local *)		     

end
