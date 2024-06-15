open Printf
open Types
open Print
open Parse

type precedence = string -> string -> int

(* Turns a list into a precedence, (a well-founded partial order on
   function symbols) e.g. the precendence [["a"; "b"]; ["f"; "g"];
   ["h"]] denotes a ~ b < f ~ g < h *)
let list_to_prec ll =
  fun s1 s2 ->
  let i1 = List.find_index (List.mem s1) ll in
  let i2 = List.find_index (List.mem s2) ll in
  match i1, i2 with
  | Some i1, Some i2 -> compare i1 i2
  | _ -> raise (Invalid_argument "symbol not found in precedence list")

let rec lpo (prec : precedence) t1 t2 =
  match t1 with
  | Var _ -> false
  | App (f, ts) ->
     List.exists (fun ti -> lpo_eq prec ti t2) ts
     || begin
        match t2 with
        (* Handled by the lpo_eq case*)
        | Var _ -> false
        | App (g, us) ->
           if prec g f < 0 then
             List.for_all (fun ui -> lpo prec t1 ui) us
           else if prec g f == 0 then (* hard case! *)
             lex_lpo prec ts us
           else false
        end
and lpo_eq prec t1 t2 =
  t1 == t2
  || lpo prec t1 t2
and lex_lpo prec ts us =
  match ts, us with
  | [], _ -> false
  | _, [] -> false
  | t::ts, u::us ->
     if lpo prec t u then true
     else if t == u then lex_lpo prec ts us
     else false

(* Run with: *)
(* ocamlbuild -r main.native && ./main.native *)
let () =
  glob_in := "(VAR X Y Z)(THEORY (EQUATIONS f(X,Y) == Y))(RULES g(X,Y) -> X g(X,Y) -> f(X,Y))";
  glob_cursor := 0;
  let trs = parse_spec () in
  print_trs stdout trs;
  let test_prec = list_to_prec [["f"]; ["g"]] in
  printf "f comp g is %d\n" (test_prec "g" "f")
