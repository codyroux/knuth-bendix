type zip_loc = {
  sym : string;
  left_rev : Types.term list;
  right : Types.term list;
  parent : term_zip;
}
and term_zip = Here | There of zip_loc
val get_max_var : Types.term -> int
val freshen : int -> Types.term -> Types.term
val occurs : Types.var -> Types.term -> bool
val add_binding :
  Types.term Types.VarMap.t ->
  Types.VarMap.key -> Types.term -> Types.term Types.VarMap.t
val unify :
  Types.term Types.VarMap.t ->
  Types.term -> Types.term -> Types.term Types.VarMap.t option
val unify_list :
  Types.term Types.VarMap.t ->
  Types.term list -> Types.term list -> Types.term Types.VarMap.t option
val narrow_head :
  Types.term -> Types.rule -> (Types.term * Types.term Types.VarMap.t) option
val zip_with :
  Types.term Types.VarMap.t -> term_zip -> Types.term -> Types.term
val splay : term_zip -> Types.term -> (term_zip * Types.term) list
val splay_list : zip_loc -> Types.term list -> (term_zip * Types.term) list
val narrow_open :
  term_zip ->
  Types.term -> Types.rule -> (Types.term * Types.term Types.VarMap.t) option
val all_somes : 'a option list -> 'a list
val narrow_splay :
  Types.term -> Types.rule -> (Types.term * Types.term Types.VarMap.t) list
val crit_rule_aux :
  Types.term -> Types.term -> Types.rule -> (Types.term * Types.term) list
val crit_rule : Types.rule -> Types.rule -> (Types.term * Types.term) list
val list_pairs : 'a list -> ('a * 'a) list
val crit_all_rules : Types.rule list -> (Types.term * Types.term) list
val filter_all :
  Types.rule list ->
  (Types.term * Types.term) list -> (Types.term * Types.term) list
