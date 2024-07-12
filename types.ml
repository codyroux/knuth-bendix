
type var = { level : int; name : string }

type term = Var of var | App of string * term list

let var s = Var { level = 0; name = s }

type eqn = { eq_lhs : term; eq_rhs : term }
type rule = { r_lhs : term; r_rhs : term }

type trs = {
    vars : string list;
    eqns : eqn list;
    rules : rule list;
  }

module Var =
  struct
    type t = var
    let compare { level = i1; name = s1 } { level = i2; name = s2 } =
      match Int.compare i1 i2 with
      | 0 -> String.compare s1 s2
      | x -> x
  end

module VarMap = Map.Make(Var)
