(* file: main.sml *)
(* description: main procedure *)
(* author: Masaki Haga *)

local
  structure T = Term
  structure L = List
  structure PO = PathOrder
  structure LU = ListUtil
in

exception Failure of string
exception Error of string

val name = CommandLine.name ()
val args = CommandLine.arguments ()

fun readFile pass =
  let val s = TextIO.openIn pass
      val str = TextIO.inputAll s
      val _ = TextIO.closeIn s
  in str
  end 

fun writeFile pass str =
  let val s = TextIO.openOut pass
  in (TextIO.output (s, str); TextIO.closeOut s)
  end

fun readInt () =
  TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn

(* (pass to eqs file, pass to trs file, pass to output) *)
fun scanOpts opts =
  let fun scanOpts' opts (eqs, trs, out) =
        case opts of
          [] => (eqs, trs, out)
        | "-e" :: pass :: opts' => scanOpts' opts' ((Trs.rdEqs o readFile) pass, trs, out)
        | "-r" :: pass :: opts' => scanOpts' opts' (eqs, (Trs.rdRules o readFile) pass, out)
        | "-o" :: pass :: opts' => scanOpts' opts' (eqs, trs, pass)
        | _ => raise Error "Invalid Options"
  in scanOpts' opts ([],[],"./log.txt")
  end

fun comp opts =
  let val (eqs, _, out) = scanOpts opts
      fun funsEqs eqs =
        let fun fst (x, _) = x
            fun snd (_, y) = y
        in foldl (fn (r,acc) => LU.union ((T.funs (fst r) @ T.funs (snd r)), acc)) [] eqs
        end
      val fs = funsEqs eqs
      fun prFunc fs = foldl (fn (f, acc) => (print (" " ^ f); acc)) () fs
      val _ = print "Given equations:\n"
      val _ = (print o Trs.prEqs) eqs
      val _ = print "\nFunction symbols:\n   "
      val _ = prFunc fs
      val _ = if fs <> []
              then (print "\n\nPlease type weights of function symbols (for example:";
                   L.app (fn _ => print " 0") fs; print "):\n")
              else ()
      val grter = (PO.lpoGt o PO.rdPrec) (foldl (fn (f, acc) => (f, valOf (readInt ())) :: acc) [] fs)
      val result = (print "\n"; Comp.kb grter eqs)
  in case result of
       SOME trs => (print ("\nWriting this TRS to `" ^ out ^ "'... ");
                   writeFile out (Trs.prRules trs); print "Done!\n")
     | NONE => ()
  end

fun info opts =
  let val (eqs, trs, out) = scanOpts opts
      val msg = "Given Equations:\n    " ^ Trs.prEqs eqs ^ "\nGiven TRS:\n    " ^ Trs.prRules trs
      val _ = print msg
      val _ = (print ("\nWriting this information to `" ^ out ^ "'... ");
              writeFile out msg; print "Done!\n")
  in ()
  end

fun help () = print ("USAGE:\n    " ^ name ^ " <SUBCOMMAND> [OPTIONS ... ]\n\nSUBCOMMAND:\n    comp, Applying Knuth-Bendix completion to equations\n    term, Checking if given TRS is terminate by lexicographic path order\n    cpk, Showing critical peaks of TRS\n    info, Showing equations and TRS\n    help, Showing this help message\n\nOPTIONS:\n    -e, Pass to input equations (default: empty set)\n    -r, Pass to input TRS (default: empty set)\n    -o, Pass to output file (default: ./log.txt)\n")

fun usage () = print ("USAGE:\n    For basic information, try `" ^ name ^ " help'.\n")

fun main _ =
  case args of
    "comp" :: opts => comp opts
  | "info" :: opts => info opts
  | "help" :: _ => help ()
  | _ => usage ()

val _ = main ()
val _ = OS.Process.exit (OS.Process.success)

end
