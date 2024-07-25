val print_term : out_channel -> Types.term -> unit
val print_term_list : out_channel -> Types.term list -> unit
val print_eqn : out_channel -> Types.eqn -> unit
val print_rule : out_channel -> Types.rule -> unit
val print_eqn_list : out_channel -> Types.eqn list -> unit
val print_rule_list : out_channel -> Types.rule list -> unit
val print_eqns : out_channel -> Types.eqn list -> unit
val print_rules : out_channel -> Types.rule list -> unit
val print_vars : out_channel -> string list -> unit
val print_trs : out_channel -> Types.trs -> unit
val print_subst : out_channel -> Types.term Types.VarMap.t -> unit
