(* file: lexical.sml *)
(* description: lexer generator *)

signature LEXICAL =
sig
    datatype token = Id of string | Key of string
    val scan: string -> token list
    val cropId: string -> (string * string) option
end

signature SPECIAL_SYMBOLS =
sig 
    val special : char list
end

functor Lexical (SpecialSymbols: SPECIAL_SYMBOLS) : LEXICAL =
struct

datatype token = Key of string | Id of string;
	
local 
    structure SS = SpecialSymbols
    structure LU = ListUtil
in

fun scanning (toks, ss) =
    case Substring.getc ss of
	NONE => rev toks
      | SOME (c, ss1) => 
	if LU.member c SS.special
	then let val tok = Key (Char.toString c)
	     in scanning (tok::toks, ss1)
	     end
	else if not (Char.isGraph c)
	then scanning (toks, ss1)
	else let val (ss2, ss3) = Substring.splitl (fn d => not (LU.member d  SS.special)
							    andalso Char.isGraph d) ss
		 val tok = Id (Substring.string ss2)
	     in scanning (tok::toks, ss3)
	     end

fun scan a = scanning ([], Substring.full a);

fun cropId str = let val (id,rest) = 
			 Substring.splitl (fn d => not (LU.member d  SS.special) andalso Char.isGraph d) 
					  (Substring.dropl (not o Char.isGraph) (Substring.full str))
		 in if Substring.isEmpty id 
		    then NONE
		    else SOME (Substring.string id, Substring.string rest)
		end

end (* of local *)

end
