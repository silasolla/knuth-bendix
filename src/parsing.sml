(* file: parsing.sml *)
(* description: paser generator from "ML for Working Programmer", C.Paulson *)

signature PARSING =
sig 
    exception SyntaxErr of string
    type token
    val id : token list -> string * token list
    val $ : string -> token list -> string * token list
    val empty : 'a -> 'b list * 'a
    val || : ('a -> 'b) * ('a -> 'b) -> 'a -> 'b
    val !! : ('a -> 'b * 'c) -> ('a -> 'b * 'c)
    val -- : ('a -> 'b * 'c) * ('c -> 'd * 'e) -> 'a -> ('b * 'd ) * 'e
    val $-- : string * (token list -> 'a * 'b) -> token list -> 'a * 'b
    val >> : ('a -> 'b * 'c) * ('b -> 'd) -> 'a -> 'd * 'c
    val repeat : ('a -> 'b * 'a) -> 'a -> 'b list * 'a
    val infixes :
	(token list -> 'a * token list) * (string -> int) *
	(string -> 'a -> 'a -> 'a) -> token list -> 'a * token list
    val reader : (token list -> 'a * 'b list) -> string -> 'a 
end

functor Parsing (Lex: LEXICAL) : PARSING =
	struct
	infix 6 $--
	infix 5 --
	infix 3 >>
	infix 0 ||

	type token = Lex.token;

	exception SyntaxErr of string;

	fun id (Lex.Id a :: toks) = (a, toks)
	  | id toks = raise SyntaxErr "Identifier expected";

	fun $ a (Lex.Key b :: toks) = if (a = b) then (a, toks)
				      else raise SyntaxErr a
	  | $ a _ = raise SyntaxErr "Symbol expected";

	fun empty toks = ([], toks);

	fun (ph1 || ph2) toks = ph1 toks 
				handle SyntaxErr _ => ph2 toks;

	fun !! ph toks = ph toks 
			 handle SyntaxErr msg => raise Fail ("Syntax error: " ^ msg);

	fun (ph1 -- ph2) toks = let val (x, toks2) = ph1 toks
				    val (y, toks3) = ph2 toks2
				in ((x,y), toks3) 
				end;
	
	fun (ph >> f) toks = let val (x, toks2) = ph toks
			     in (f x, toks2) 
			     end;

	fun (a $-- ph) = ($ a) -- (!! ph) >> #2;

	fun repeat ph toks = (ph -- repeat ph >> (op::)  || empty) toks;

	fun infixes (ph, prec_of, apply) =
	    let fun over k toks = next k (ph toks)
		and next k (x, Lex.Key (a) :: toks) = 
		    if prec_of a < k then (x, Lex.Key a :: toks)
		    else next k ((over (prec_of a) >> apply a x) toks)
		  | next k (x, toks) = (x, toks)
	    in
		over 0
	    end;

	fun reader ph a = case ph (Lex.scan a) of
			      (x, []) => x
			    | (_, _::_) => raise SyntaxErr "Extra characters in phrase";
        
	end
