type var = { level : int; name : string; }
type term = Var of var | App of string * term list
val var : string -> term
type eqn = { eq_lhs : term; eq_rhs : term; }
type rule = { r_lhs : term; r_rhs : term; }
type trs = { vars : string list; eqns : eqn list; rules : rule list; }
module Var : sig type t = var val compare : var -> var -> int end
module VarMap : Map.S with type key = Var.t
