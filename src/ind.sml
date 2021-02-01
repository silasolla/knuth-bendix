(* file: ind.sml *)
(* description: functions for inductive theorem proving *)

signature IND = 
sig 
    val ind: ManySorted.ms_sign * ManySorted.ms_rules * (Term.term * Term.term -> bool) -> ManySorted.ms_eqs -> bool
end

structure Ind : IND =
struct
local 
    structure AL = AssocList
    structure L = List
    structure LP = ListPair
    structure LU = ListUtil
    structure MS = ManySorted
    structure SU = StringUtil
    structure T = Term
in

fun expd rs ((s,t),ty) (C,u) =
    let val rs2 = Trs.renameRules (T.varsList [s,t]) (MS.dropSortInMsRules rs)
    in L.mapPartial (fn (l,r) => 
		     case Subst.unify (u,l) of 
			 SOME sigma => SOME ((..., ...),ty)
		       | NONE => NONE) rs2
    end


(* select directable equation of smallest size (at least of size <= maxValue) *)
(* fun select grter es = ... *)

fun expand ((dfuns,cfuns),rs,grter) (es0,hs0) = 
    let fun removeEq ((l,r),ty) es = LU.diff (es, [((l,r),ty), ((r,l),ty)])
	fun selectAndExpand es =
	    case select grter es of 
		SOME (rule as ((s,t),ty)) => ...
	      | NONE => NONE
    in case selectAndExpand es0 of
	   SOME (es2,rule) => (es2 @ removeEq rule es0, rule::hs0)
	 | NONE => (print "Ind.expand: no equation to expand\n"; raise Failure)

    end

fun simplify rs (es,hs) = ...

fun delete (es,hs)  = ...

exception Success
exception Stopped
exception Failure

fun indStep ((dfuns,cfuns),rs,grter) (es,hs)
    = let val (es1,hs1) = expand ((dfuns,cfuns),rs,grter) (es,hs)
	  val (es2,hs2) = simplify rs (es1,hs1)
	  val (es3,hs3) = delete (es2,hs2)
      in (es3,hs3)
      end

fun ind (sign,rs,grter) es =
     let val dfuns = ...
	 val cfuns = ...
	 val _ = print "We assume that R is a left-linear quasi-reducible CS."
	 val _ = print "Check whether R is oriented w.r.t. given ordering..."
	 val _ = if ... 
		 then print "OK\n"
		 else (print "Failed\n"; raise Failure)
	 val (es',_) = delete (simplify rs (es,[]))
	 val _ = if null es' then raise Success else ()

	 val max = 20
         fun indsub n (es0,hs0) = 
	     if n > max then raise Stopped
	     else let val (es1,hs1) = indStep ((dfuns,cfuns),rs,grter) (es0,hs0)
		  in if null es1
		     then raise Success
		     else indsub (n+1) (es1,hs1)
		  end
     in indsub 1 (es',[])
     end 
     handle Success => (print "Success\n"; true)
	  | Failure => (print "Failed\n"; false)
	  | Stopped => (print "Stopped\n"; false)
end

end (* of local *)		     
end

