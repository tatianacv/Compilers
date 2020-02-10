(* depython.sml - data structures for a simplified pseudo python abstract syntax *)
(* modeled after program 1.5 of "Modern compiler impementation in ML" *)
(* Copyright 2018 - Humberto Ortiz-Zuazaga - humberto.ortiz@upr.edu *)
(* Released under the GNU General Public License v3 see LICENSE *)

type id = string

datatype binop = Add | Mult

datatype prog = Module of stm list

and stm = Assign of id * exp
	| Print of exp list
	| Expr of exp

and exp = Num of int
	| BinOp of exp * binop * exp
	| Name of id
	| Eseq of stm * exp

val p1 = Module [Expr (Num 123)];

val p2 = Module [Expr (BinOp (Num 3, Add, Num 2))] : prog;

val p3 = Module [Assign ("a", Num 3), Print [BinOp (Name "a", Mult, Num 2)]];
