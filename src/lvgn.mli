open Rule_abstraction

type linear_view_info = {
  view_parameters: bool list;
  poc: int;
  soc: int;
}

type linear_view_result = (linear_view_info, unit) result

type predicate_definition_linear_view_infos = linear_view_result PredicateMap.t

val linear_view_result_of_rule_abstraction: string -> predicate_definition_linear_view_infos -> rule_abstraction -> linear_view_result

val merge_linear_view_result_for_predicate_definition: linear_view_result list -> linear_view_result
