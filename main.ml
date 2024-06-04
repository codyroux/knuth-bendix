open Printf
open Types
open Print
open Parse

type precedence = string -> string -> int

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
           (assert false)
           || (assert false)
        end
and lpo_eq prec t1 t2 =
  t1 == t2
  || (assert false)

(* Run with: *)
(* ocamlbuild -r main.native && ./main.native *)
let () =
  glob_in := "(VAR X Y Z)(THEORY (EQUATIONS f(X,Y) == Y))";
  glob_cursor := 0;
  let vars = parse_vars () in
  let theories = parse_theories vars () in
  print_vars stdout vars;
  print_eqns stdout theories;

