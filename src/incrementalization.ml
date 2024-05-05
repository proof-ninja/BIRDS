
open Expr
open Utils
open Rule_abstraction
open Sorted_rules

(** The prefix used for variables generated during incrementalization. *)
let updated_variable_prefix = "__updated__"

type error = Rule_abstraction.error

let string_of_intermediate_predicate = function
  | ImPred table        -> Printf.sprintf "%s" table
  | ImDeltaInsert table -> Printf.sprintf "+%s" table
  | ImDeltaDelete table -> Printf.sprintf "-%s" table

let string_of_error = function
  | UnexpectedHeadVarForm var ->
      Printf.sprintf "unexpected head var form: %s" (string_of_var var)
  | UnexpectedBodyVarForm var ->
      Printf.sprintf "unexpected body var form: %s" (string_of_var var)
  | NoViewVariable ->
      "There is no view in this program."
  | CyclicDependency defs ->
    let s =
      defs |>
      List.map (fun (impred, _) -> string_of_intermediate_predicate impred) |>
      String.concat ", "
    in
    Printf.sprintf "cyclic dependency found among %s" s

let incrementalization_ast (ast: expr): (rule list, error) result =
  let open ResultMonad in

  match ast.view with
    | Some (view_name, params) ->
      let updated_view_name = updated_variable_prefix ^ view_name  in

      let intermediate_rules =
        let param_vars = params |> List.map(fun (var_name, _) -> NamedVar(var_name)) in

        let delete_rule = Pred (updated_view_name, param_vars), Rel (Pred (view_name, param_vars)) :: Not (Deltadelete (view_name, param_vars)) :: [] in

        let insert_rule = Pred (updated_view_name, param_vars), Rel (Deltainsert (view_name, param_vars)) :: [] in

        delete_rule :: insert_rule :: []
      in

      let convert_not_linear_rule_abstraction (ruleabs: rule_abstraction): rule_abstraction =
        let replace_name (name: string): string =
          if name = view_name then updated_view_name else name 
        in

        let convert_intermediate_predicate: intermediate_predicate -> intermediate_predicate = function
          | ImPred table_name -> ImPred (replace_name table_name)
          | ImDeltaInsert table_name -> ImDeltaInsert (replace_name table_name)
          | ImDeltaDelete table_name -> ImDeltaDelete (replace_name table_name)
        in

        let convert_intermediate_clause: intermediate_clause -> intermediate_clause = function
          | ImPositive (pred, vars) -> ImPositive (convert_intermediate_predicate pred, vars)
          | ImNegative (pred, vars) -> ImNegative (convert_intermediate_predicate pred, vars)
          | ImEquation  eterm -> ImEquation  eterm
          | ImNonequation eterm -> ImNonequation eterm
          | ImConstTerm bool -> ImConstTerm bool
        in
        
        { binder = ruleabs.binder; body = ruleabs.body |> List.map convert_intermediate_clause; }
      in

      let convert_linear_rule_abstraction (ruleabs: rule_abstraction): rule_abstraction =
        let convert_intermediate_clause: intermediate_clause -> intermediate_clause = function
          | ImPositive (ImPred table_name, vars) ->
            if table_name = view_name then ImPositive (ImDeltaInsert table_name, vars) else ImPositive (ImPred table_name, vars)
          | ImNegative (ImPred table_name, vars) ->
            if table_name = view_name then ImPositive (ImDeltaDelete table_name, vars) else ImNegative (ImPred table_name, vars)
          | ImPositive (intermediate_predicate, vars) -> ImPositive (intermediate_predicate, vars)
          | ImNegative (intermediate_predicate, vars) -> ImNegative (intermediate_predicate, vars)
          | ImEquation  eterm -> ImEquation  eterm
          | ImNonequation eterm -> ImNonequation eterm
          | ImConstTerm bool -> ImConstTerm bool
        in

        { binder = ruleabs.binder; body = ruleabs.body |> List.map convert_intermediate_clause; }
      in

      sort_rules ast.rules >>= fun sorted_predicate_definitions ->

      let (infos, converted_rules) =
        List.fold_left (fun acc predicate_definition ->

          let (acc_infos, acc_rules) = acc in
          let (impred, ruleabs_set) = predicate_definition in

          let (results, rules) =
            ruleabs_set |>
            RuleAbstractionSet.to_list |>
            List.map (fun ruleabs ->
              let result = Lvgn.linear_view_result_of_rule_abstraction view_name acc_infos ruleabs in

              let converted_ruleabs =
                if Result.is_error result then
                  convert_not_linear_rule_abstraction ruleabs
                else
                  convert_linear_rule_abstraction ruleabs
              in

              (result, Rule_abstraction.inject_rule impred converted_ruleabs)
            ) |>
            List.split
          in

          (PredicateMap.add impred (Lvgn.merge_linear_view_result_for_predicate_definition results) acc_infos, rules :: acc_rules)

        ) (PredicateMap.empty, []) sorted_predicate_definitions
      in

      return (
        if (PredicateMap.exists (fun _ -> Result.is_error) infos) then
          List.flatten (intermediate_rules :: List.rev converted_rules)
        else
          List.flatten (List.rev converted_rules)
      )

    | None -> err NoViewVariable

