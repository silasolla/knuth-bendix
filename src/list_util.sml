(* file: list_util.sml *)
(* description: utility functions for list *)
(* author: Masaki Haga *)

signature LIST_UTIL = 
sig 
    (* functions for set *)
    val member: ''a  -> ''a list -> bool
    val add: ''a  -> ''a list -> ''a list
    val union: ''a list * ''a list -> ''a list
    val intersection: ''a list * ''a list -> ''a list
    val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val cutInHalf: int -> 'a list -> 'a list * 'a list
    val nthMap: int -> ('a -> 'a) -> 'a list -> 'a list
    val mapAppend: ('a -> 'b list) -> 'a list -> 'b list
    val lexOrd: int list * int list -> bool
    val liOrd: int list * int list -> bool
    val qsort: ('a * 'a -> bool) -> 'a list -> 'a list
    val toStringBuilt: ('a -> string) -> (string * string * string) -> 'a list -> string
    val toStringCommaSpace: ('a -> string) -> 'a list  -> string
    val toStringCommaSpaceBrace: ('a -> string) -> 'a list  -> string
    val toStringCommaLnSquare: ('a -> string) -> 'a list -> string
end

structure ListUtil : LIST_UTIL =
struct 

local
    structure L = List
in

fun member x ys = L.exists (fn y => x = y) ys

fun add x ys = if member x ys then ys else x::ys

fun union (xs, ys) = foldr (fn (z,zs) => add z zs) ys xs

fun intersection (xs, ys) = L.filter (fn x => member x ys) xs

fun foldri f e xs =
    let fun foldri' n [] = e
	  | foldri' n (z :: zs) = f (n, z, foldri' (n+1) zs)
    in foldri' 0 xs
    end
fun foldli f e xs =
    let fun foldli' n e' [] = e'
	  | foldli' n e' (z :: zs) = foldli' (n+1) (f (n, z, e')) zs
    in foldli' 0 e xs
    end

fun cutInHalf n xs = (L.take (xs, n), L.drop (xs, n))

fun nthMap n f [] = []
  | nthMap 0 f (x :: xs) = f x :: xs
  | nthMap n f (x :: xs) = x :: nthMap (n - 1) f xs

fun mapAppend f xs = foldr (fn (z, zs) => f z @ zs) [] xs

fun lexOrd ([],ys) = false
  | lexOrd (xs,[]) = true
  | lexOrd (x::xs,y::ys) =
    if x = y then lexOrd (xs,ys)
    else x > y	

fun liOrd (xs,ys) =
    let val (xsLen,ysLen) = (L.length xs,L.length ys)
    in if xsLen = ysLen
       then lexOrd (xs,ys)
       else xsLen < ysLen
    end

fun qsort order [] = []
  | qsort order (x :: xs) =
    let val pp = L.partition (fn z => order (x,z)) xs
    in qsort order (#1 pp) @ (x :: qsort order (#2 pp))
    end

fun toStringBuilt prElm (start,sep,stop) xs =
    let fun toStringSub [] = ""
          | toStringSub [x] = (prElm x)
          | toStringSub (x::xs)= (prElm x) ^ sep ^ (toStringSub xs)
    in  start ^ (toStringSub xs) ^ stop
    end

fun toStringCommaSpace prElm xs = toStringBuilt prElm ("",", ","") xs;
fun toStringCommaSpaceBrace prElm xs = toStringBuilt prElm ("{",", ","}") xs;

fun toStringCommaLnSquare prElm xs = toStringBuilt prElm ("   [ ",",\n     "," ]\n") xs;

end (* of local *)

end
