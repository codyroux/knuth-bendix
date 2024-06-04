open Printf
open Types

let glob_in = ref ""

let glob_cursor = ref 0

exception Parse_Error of string * int

let eof () = !glob_cursor >= String.length !glob_in

let peek () = !glob_in.[!glob_cursor]

let pop () = let c = peek() in incr glob_cursor; c

let is_delim c =
  c == ' ' || c == '(' || c == ')' || c == '\n' || c == ','

let parse_ident () =
  if is_delim (peek ()) then
    begin
      let err = sprintf "Expected alphanum at %d" (!glob_cursor) in
      raise (Parse_Error (err, !glob_cursor))
    end;
  let pos = !glob_cursor in
  let len = ref 0 in
  while not (eof () || is_delim (peek ())) do
    ignore (pop ());
    incr len;
  done;
  String.sub !glob_in pos !len

let parse_key key =
  let pos = !glob_cursor in
  let str_pos = ref 0 in
  while !str_pos < String.length key && pop () == key.[!str_pos] do
    incr str_pos;
  done;
  if !str_pos != String.length key then
    begin
      let err = sprintf "Expected %s at position %d" key pos in
      raise (Parse_Error (err, pos))
    end;
  ()

let space c = c == ' ' || c == '\n'
let comma c = c == ','

let rec parse_list sep p =
  try let v = p() in
      if sep (peek ()) then
        begin
          ignore (pop ());
          v :: parse_list sep p
        end
      else [v]
  with
  | Parse_Error _ -> []


let parse_vars () =
  parse_key "(VAR ";
  let vs = parse_list space parse_ident in
  parse_key ")";
  vs

let rec parse_arg_list vars () =
  parse_key "(";
  let args = parse_list comma (parse_term vars) in
  parse_key ")";
  args
and parse_term vars () =
  let f = parse_ident () in
  if not (eof ()) && peek () == '(' then
    let args = parse_arg_list vars () in
    App(f, args)
  else if List.mem f vars then
    Var(f)
  else App(f, [])

let parse_equation vars () =
  let t1 = parse_term vars () in
  parse_key " == ";
  let t2 = parse_term vars () in
  { eq_lhs = t1; eq_rhs = t2}


let parse_theory vars () =
  parse_key "(EQUATIONS ";
  let eqns = parse_list space (parse_equation vars) in
  parse_key ")";
  eqns

let parse_theories vars () =
  parse_key "(THEORY ";
  let th = parse_list space (parse_theory vars) in
  parse_key ")";
  List.hd th
