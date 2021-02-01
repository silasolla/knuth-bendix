(* file: string_util.sml *)
(* description: utility functions for string *)
(* author: Masaki Haga *)

signature STRING_UTIL  = 
sig 
    val scanBalancedPar: string -> string * string
    val scanKey: string -> string -> string option
    val scanBeginEnd: string * string -> string -> string option
    val ending: string -> bool
end

structure StringUtil: STRING_UTIL =
struct

(* 空白文字をよみとばす *)
fun dropNonGraph ss = Substring.dropl (not o Char.isGraph) ss

(* 開き括弧と閉じ括弧がバランスするところまでと，それ以降に分割する *)
(* ただし，最初の文字までは，空白文字であればスキップする *)
fun scanBalancedPar str = 
    let fun rest 0 len ss1 = SOME len
	  | rest depth len ss =
	    case Substring.getc ss of
		NONE => NONE
	      | SOME (#"(", ss1) => rest (depth+1) (len+1) ss1
	      | SOME (#")", ss1) => rest (depth-1) (len+1) ss1
	      | SOME (c, ss1) => rest depth (len+1) ss1
	fun start (body,ss) = 
	    case Substring.getc ss of
		SOME (#"(", ss1) => rest 1 1 ss1
	      | _ => SOME 0
	val ss0 = dropNonGraph (Substring.full str)
    in case start ("", ss0) of
	   NONE => raise Fail "Syntax error: no closing \")\""
	 | SOME len => let val (balanced,rest) = Substring.splitAt (ss0,len)
		       in (Substring.string balanced, Substring.string rest)
		       end
    end

(* 先頭に key があれば，その後ろを返す *)
(* ただし，最初の文字までは，空白文字であればスキップする *)
fun scanKey key str = 
    let val ss = dropNonGraph (Substring.full str)
    in if Substring.isPrefix key ss
       then SOME (Substring.string (Substring.triml (String.size key) ss))
       else NONE 
    end

(* 先頭と最後に，それぞれ start と stop があれば，その途中を返す *)
(* ただし，最初の空白列は最後の空白列は，スキップする *)
fun scanBeginEnd (start,stop) str =
    let val ss = Substring.dropr (not o Char.isGraph) (dropNonGraph (Substring.full str))
    in if Substring.isPrefix start ss
	  andalso Substring.isSuffix stop ss
       then SOME (Substring.string 
		      (Substring.trimr (String.size stop)
				       (Substring.triml (String.size start) ss)))
       else NONE 
    end

(* 終了判定．ただし，前後に空白文字であればスキップする *)
fun ending str = Substring.isEmpty (dropNonGraph (Substring.full str))

end
