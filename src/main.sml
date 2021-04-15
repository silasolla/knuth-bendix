(* file: main.sml *)
(* description: main procedure *)
(* author: Masaki Haga *)

local
  structure T = Term
  structure L = List
  structure RW = Rewrite
  structure PO = PathOrder
  structure LU = ListUtil
in

exception Error of string

val name = CommandLine.name ()
val args = CommandLine.arguments ()

fun usage () = print ("USAGE:\n    For basic information, try `" ^ name ^ " help'.\n")

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

fun readInt () = TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn

fun readStr () = TextIO.inputLine TextIO.stdIn

(* (pass to eqs file, pass to trs file, pass to output) *)
fun scanOpts opts =
  let fun scanOpts' opts (eqs, trs, out) =
        case opts of
          [] => (eqs, trs, out)
        | "-e" :: pass :: opts' => scanOpts' opts' ((Trs.rdEqs o readFile) pass, trs, out)
        | "-r" :: pass :: opts' => scanOpts' opts' (eqs, (Trs.rdRules o readFile) pass, out)
        | "-o" :: pass :: opts' => scanOpts' opts' (eqs, trs, pass)
        | other :: _ => raise (Error ("Invalid option `" ^ other ^ "'.\n"))
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

fun sn opts =
  let val (_, trs, out) = scanOpts opts
      fun funsRules trs =
        let fun fst (x, _) = x
            fun snd (_, y) = y
        in foldl (fn (r,acc) => LU.union ((T.funs (fst r) @ T.funs (snd r)), acc)) [] trs
        end
      val fs = funsRules trs
      fun prFunc fs = foldl (fn (f, acc) => (print (" " ^ f); acc)) () fs
      val _ = print "Given TRS:\n"
      val _ = (print o Trs.prRules) trs
      val _ = print "\nFunction symbols:\n   "
      val _ = prFunc fs
      val _ = if fs <> []
              then (print "\n\nPlease type weights of function symbols (for example:";
                   L.app (fn _ => print " 0") fs; print "):\n")
              else ()
      val isSN = (PO.isLpoDecreasingTrs o PO.rdPrec) (foldl (fn (f, acc) => (f, valOf (readInt ())) :: acc) [] fs) trs
  in if isSN
     then print "\nThis is lexicographic path order decreasing TRS.\nHence this is terminating TRS.\n"
     else print "\nThis is not lexicographic path order decreasing TRS.\n"
  end

fun cpk opts =
  let val (_, trs, out) = scanOpts opts
      fun funsRules trs =
        let fun fst (x, _) = x
            fun snd (_, y) = y
        in foldl (fn (r,acc) => LU.union ((T.funs (fst r) @ T.funs (snd r)), acc)) [] trs
        end
      val fs = funsRules trs
      fun prFunc fs = foldl (fn (f, acc) => (print (" " ^ f); acc)) () fs
      val _ = print "Given TRS:\n"
      val _ = (print o Trs.prRules) trs
      val _ = print "\nCritical Peaks:\n"
      val cpks = Cr.cpk trs
      fun prCpk (v1,u,v2) = "<" ^ (T.toString v1) ^ " <- " ^ (T.toString u) ^ " -> " ^ (T.toString v2) ^ ">\n"
      val msg = foldl (fn (c,acc) => acc ^ prCpk c) "" cpks
      val _ = print msg
      val _ = (print ("\nWriting this information to `" ^ out ^ "'... ");
              writeFile out msg; print "Done!\n")
  in ()
  end

fun lirw opts =
  let val (_, trs, out) = scanOpts opts
      fun funsRules trs =
        let fun fst (x, _) = x
            fun snd (_, y) = y
        in foldl (fn (r,acc) => LU.union ((T.funs (fst r) @ T.funs (snd r)), acc)) [] trs
        end
      val fs = funsRules trs
      fun prFunc fs = foldl (fn (f, acc) => (print (" " ^ f); acc)) () fs
      val _ = print "Given TRS:\n"
      val _ = (print o Trs.prRules) trs
      val _ = print "\nPlease type a first order term:\n"
      val s = case readStr () of
                SOME s' => s'
              | NONE => ""
      val _ = print "\nLeftmost innermost rewrite steps:\n"
      val msg = RW.listepsToNF trs (T.fromString s)
      val _ = (print ("\nWriting this information to `" ^ out ^ "'... ");
              writeFile out msg; print "Done!\n")
  in ()
  end

fun info opts =
  let val (eqs, trs, out) = scanOpts opts
      val msg = "Given Equations:\n" ^ Trs.prEqs eqs ^ "\nGiven TRS:\n" ^ Trs.prRules trs
      val _ = print msg
      val _ = (print ("\nWriting this information to `" ^ out ^ "'... ");
              writeFile out msg; print "Done!\n")
  in ()
  end

fun help () = print ("USAGE:\n    " ^ name ^ " <SUBCOMMAND> [OPTIONS ... ]\n\nSUBCOMMAND:\n    comp, Apply Knuth-Bendix completion to given equations and write given TRS to a file\n    sn, Check if given TRS is terminate by lexicographic path order\n    cpk, Show critical peaks of given TRS and write them to file\n    lirw, Show leftmost innermost rewrite steps to normal form by given TRS and write them to a file\n    info, Show given equations and TRS and write them to a file\n    help, Show this help message\n\nOPTIONS:\n    -e, Pass to input equations (default: empty set)\n    -r, Pass to input TRS (default: empty set)\n    -o, Pass to output file (default: ./log.txt)\n")

fun main _ =
  case args of
    "comp" :: opts => comp opts
  | "sn" :: opts => sn opts
  | "cpk" :: opts => cpk opts
  | "lirw" :: opts => lirw opts
  | "info" :: opts => info opts
  | "help" :: _ => help ()
  | other :: _ => raise (Error ("Invalid subcommand `" ^ other ^ "'.\n"))
  | [] => raise (Error "No subcommand given.\n")

val _ = main ()
  handle Error str => (print (name ^ ": Error: " ^ str ^ "\n"); usage ())

val _ = OS.Process.exit (OS.Process.success)

end
