(* file: formula.sml *)
(* description: encoding of formulas *)
(* author: Masaki Haga *)

signature FORMULA = 
sig
    datatype arith  = Var of int 
                    | Const of int
                    | Neg of arith
                    | Add of arith * arith
                    | Sub of arith * arith
                    | Mul of arith * arith
                    | Sum of arith list
                    | Prod of arith list

    datatype atom = Eq of arith * arith 
                  | Neq of arith * arith 
                  | Ge of arith * arith
                  | Le of arith * arith
                  | Gt of arith * arith
                  | Lt of arith * arith
                  | Distinct of arith list

    datatype prop = True 
                  | False 
                  | Atom of atom
                  | Not of prop
                  | And of prop * prop
                  | Or of prop * prop
                  | Conj of prop list
                  | Disj of prop list
                  | Imp of prop * prop
                  | Iff of prop * prop
                  | IfThenElse of prop * prop * prop
end

structure Formula : FORMULA =
struct

datatype arith  = Var of int 
                | Const of int
                | Neg of arith
                | Add of arith * arith
                | Sub of arith * arith
                | Mul of arith * arith
                | Sum of arith list
                | Prod of arith list

datatype atom = Eq of arith * arith 
              | Neq of arith * arith 
              | Ge of arith * arith
              | Le of arith * arith
              | Gt of arith * arith
              | Lt of arith * arith
              | Distinct of arith list

datatype prop = True 
              | False 
              | Atom of atom
              | Not of prop
              | And of prop * prop
              | Or of prop * prop
              | Conj of prop list
              | Disj of prop list
              | Imp of prop * prop
              | Iff of prop * prop
              | IfThenElse of prop * prop * prop

end
