
open Expr
open Utils

(** The prefix used for variables generated during incrementalization. *)
let updated_variable_prefix = "__updated__"

type named_var =
  | ImNamedVar of string

type intermediate_predicate =
  | ImPred        of table_name
  | ImDeltaInsert of table_name
  | ImDeltaDelete of table_name

type intermediate_argument =
  | ImNamedVarArg of named_var
  | ImConstArg    of const
  | ImAnonVarArg

type intermediate_clause =
  | ImPositive    of intermediate_predicate * intermediate_argument list
  | ImNegative    of intermediate_predicate * intermediate_argument list
  | ImEquation    of eterm
  | ImNonequation of eterm
  | ImConstTerm   of bool

(** The type for rule abstractions,
    i.e. data of the form `(X_1, ..., X_n) -> C_1, ..., C_m.` *)
type rule_abstraction = {
  binder: named_var list;
  body  : intermediate_clause list;
}

module RuleAbstraction = struct
  type t = rule_abstraction

  let compare = compare
end

module RuleAbstractionSet = Set.Make(RuleAbstraction)

type predicate_definition =
  intermediate_predicate * RuleAbstractionSet.t

module Predicate = struct
  type t = intermediate_predicate

  let compare (impred1: t) (impred2: t): int =
    match (impred1, impred2) with
    | (ImPred t1, ImPred t2)               -> String.compare t1 t2
    | (ImPred _, _)                        -> 1
    | (_, ImPred _)                        -> -1
    | (ImDeltaInsert t1, ImDeltaInsert t2) -> String.compare t1 t2
    | (ImDeltaInsert _, _)                 -> 1
    | (_, ImDeltaInsert _)                 -> -1
    | (ImDeltaDelete t1, ImDeltaDelete t2) -> String.compare t1 t2
end

module PredicateMap = Map.Make(Predicate)

module PredicateDependencyGraph = Dependency_graph.Make(Predicate)

type intermediate_program = RuleAbstractionSet.t PredicateMap.t

type view_parameters = (bool list) PredicateMap.t

type pararell_occurrence_counts = int PredicateMap.t

type error =
  | UnexpectedHeadVarForm  of var
  | UnexpectedBodyVarForm  of var
  | NoViewVariable
  | CyclicDependency       of predicate_definition list

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
      defs |> List.map (fun (impred, _) ->
        string_of_intermediate_predicate impred
      ) |> String.concat ", "
    in
    Printf.sprintf "cyclic dependency found among %s" s

let separate_predicate_and_vars (rterm: rterm): intermediate_predicate * var list =
  match rterm with
  | Pred (t, vars)        -> (ImPred t, vars)
  | Deltainsert (t, vars) -> (ImDeltaInsert t, vars)
  | Deltadelete (t, vars) -> (ImDeltaDelete t, vars)

let convert_head_var (var: var): (named_var, error) result =
  let open ResultMonad in
  match var with
  | NamedVar x -> return (ImNamedVar x)
  | _          -> err (UnexpectedHeadVarForm var)

let convert_head_rterm (rterm: rterm): (intermediate_predicate * named_var list, error) result =
  let open ResultMonad in
  let (impred, vars) = separate_predicate_and_vars rterm in
  vars |> mapM convert_head_var >>= fun imvars ->
  return (impred, imvars)

let convert_body_var (var: var): (intermediate_argument, error) result =
  let open ResultMonad in
  match var with
  | NamedVar x ->
      return (ImNamedVarArg (ImNamedVar x))

  | AnonVar ->
      return ImAnonVarArg

  | ConstVar const ->
      return (ImConstArg const)

  | _ ->
      err (UnexpectedBodyVarForm var)

let convert_body_rterm (rterm: rterm): (intermediate_predicate * intermediate_argument list, error) result =
  let open ResultMonad in
  let (impred, vars) = separate_predicate_and_vars rterm in
  vars |> foldM (fun imarg_acc var ->
    convert_body_var var >>= fun imarg ->
    return (imarg :: imarg_acc)
  ) [] >>= fun imarg_acc ->
  return (impred, List.rev imarg_acc)

let convert_body_clause (term: term): (intermediate_clause, error) result =
  let open ResultMonad in
  match term with
  | Rel rterm ->
      convert_body_rterm rterm >>= fun (impred, imargs) ->
      return (ImPositive (impred, imargs))

  | Not rterm ->
      convert_body_rterm rterm >>= fun (impred, imvars) ->
      return (ImNegative (impred, imvars))

  | Equat eterm ->
      return (ImEquation eterm)

  | Noneq eterm ->
      return (ImNonequation eterm)

  | ConstTerm bool ->
      return (ImConstTerm bool)


let convert_rule (rule: rule): (intermediate_predicate * rule_abstraction, error) result =
  let open ResultMonad in
  let (head, body) = rule in
  convert_head_rterm head >>= fun (impred, binder) ->
  body |> foldM (fun imclause_acc term ->
    convert_body_clause term >>= fun imclause ->
    return (imclause :: imclause_acc)
  ) [] >>= fun imclause_acc ->
  let body = List.rev imclause_acc in
  let ruleabs = { binder; body } in
  return (impred, ruleabs)
    
(** Adds a mapping `(impred |-> ruleabs)` to `improg`. *)
let add_rule_abstraction (impred: intermediate_predicate) (ruleabs: rule_abstraction) (improg: intermediate_program): intermediate_program =
  match improg |> PredicateMap.find_opt impred with
  | None ->
      improg |> PredicateMap.add impred (RuleAbstractionSet.singleton ruleabs)

  | Some ruleabsset ->
      improg |> PredicateMap.add impred (ruleabsset |> RuleAbstractionSet.add ruleabs)

(** Performs topological sorting of IDB predicates based on the dependencies among them. *)
let resolve_dependencies_among_predicates (improg: intermediate_program): (predicate_definition list, error) result =
  (* Adds vertices corresponding to IDB predicates to the graph: *)
  let (graph, acc) =
    PredicateMap.fold (fun impred ruleabsset (graph, acc) ->
      match graph |> PredicateDependencyGraph.add_vertex impred ruleabsset with
      | Error _            -> assert false
      | Ok (graph, vertex) -> (graph, (impred, vertex, ruleabsset) :: acc)
    ) improg (PredicateDependencyGraph.empty, [])
  in

  (* Adds directed edges that represent dependencies among IDB predicates.
     Here, `impred_to` depends on `impred_from`: *)
  let graph =
    acc |> List.rev |> List.fold_left (fun graph (_impred_to, vertex_to, ruleabsset) ->
      let ruleabss = RuleAbstractionSet.elements ruleabsset in
      ruleabss |> List.fold_left (fun graph ruleabs ->
        ruleabs.body |> List.fold_left (fun graph clause ->
          match clause with
          | ImPositive (impred_from, _) | ImNegative (impred_from, _) ->
              begin
                match graph |> PredicateDependencyGraph.get_vertex impred_from with
                | Some vertex_from ->
                    (* If `impred_to` is an IDB predicate and thus registered to `graph` as `vertex_to`: *)
                    graph |> PredicateDependencyGraph.add_edge ~from:vertex_from ~to_:vertex_to

                | None ->
                    (* If `impred_to` is NOT an IDB predicate: *)
                    graph
              end

          | ImEquation _ | ImNonequation _ | ImConstTerm _ ->
              graph
        ) graph
      ) graph
    ) graph
  in

  (* Performs topological sorting according to the dependency graph: *)
  PredicateDependencyGraph.topological_sort graph
    |> Result.map_error (fun cycle -> CyclicDependency cycle)

let sort_rules (rules: rule list): (predicate_definition list, error) result =
  let open ResultMonad in

  rules |> foldM (fun improg rule ->
    convert_rule rule >>= fun (impred, ruleabs) ->
    return (improg |> add_rule_abstraction impred ruleabs)
  ) PredicateMap.empty >>= fun improg ->

  (* Extracts dependencies among IDB predicates and perform a topological sorting: *)
  resolve_dependencies_among_predicates improg

let incrementalization_ast (ast: expr): (rule list, error) result =
  let open ResultMonad in

  match ast.view with
    | Some (view_name, params) ->
      let updated_view_name = updated_variable_prefix ^ view_name  in

      let not_linear_view_rules =
        let replace_name (name: string): string =
          if name = view_name then updated_view_name else name 
        in

        let convert_rterm: rterm -> rterm = function
          | Pred (name, vars) -> Pred (replace_name name, vars)
          | Deltainsert (name, vars) -> Deltainsert (replace_name name, vars)
          | Deltadelete (name, vars) -> Deltadelete (replace_name name, vars)
        in

        let convert_term: term -> term = function
          | Rel rterm -> Rel (convert_rterm rterm)
          | Not rterm -> Not (convert_rterm rterm)
          | Equat eterm -> Equat eterm
          | Noneq eterm -> Noneq eterm
          | ConstTerm bool -> ConstTerm bool 
        in

        ast.rules |> List.map(fun (head, body) -> (head, body |> List.map convert_term)) in

      let param_vars = params |> List.map(fun (var_name, _) -> NamedVar(var_name)) in

      let delete_rule = Pred (updated_view_name, param_vars), Rel (Pred (view_name, param_vars)) :: Not (Deltadelete (view_name, param_vars)) :: [] in

      let insert_rule = Pred (updated_view_name, param_vars), Rel (Deltainsert (view_name, param_vars)) :: [] in

      sort_rules ast.rules >>= fun sorted_predicate_definitions ->

      let is_view_predicate = function
        | ImPred name -> name = view_name
        | ImDeltaInsert _ -> false
        | ImDeltaDelete _ -> false
      in

      let has_anonymous_view_patermeters =

        let swap_view_parameter_result_list (list: (unit, bool list) Either.t list): (unit, bool list list) Either.t =
          List.fold_left (fun a b -> match (a, b) with
            | _, Either.Left () -> Either.left ()
            | Either.Left (), _ -> Either.left ()
            | Either.Right xss, Either.Right ys -> Either.right (ys :: xss)
          ) (Either.right []) list
        in

        let merge_view_parameter_list (list: bool list list): bool list =
          list |>
          List.fold_left (fun a b -> List.combine a b |> List.map(fun (a, b) -> a || b)) ((List.hd list) |> List.map(fun _ -> false))
        in

        let merge_view_parameter_result_list (list: (unit, bool list) Either.t list): (unit, bool list) Either.t =
          list |>
          swap_view_parameter_result_list |>
          Either.map_right merge_view_parameter_list
        in

        let process_bound_predicate (parameters: view_parameters) (binder: named_var list) (predicate: intermediate_predicate) (arguments: intermediate_argument list): (unit, bool list) Either.t  =
          let bound_pair =
            if is_view_predicate predicate then
              arguments |> List.map(fun a -> (a, true))
            else
              match PredicateMap.find_opt predicate parameters with
                | Some list -> List.combine arguments list
                | None -> arguments |> List.map(fun a -> (a, false)) (* source or built-in predicate *)
          in

          let has_anonymous_view_patermeters =
            bound_pair |> 
            List.exists (function
              | ImAnonVarArg, true -> true
              | _ -> false
            )
          in

          let view_argument_names =
            bound_pair |> 
            List.filter_map (function
              | ImNamedVarArg named_var, true -> Some named_var
              | _ -> None
            )
          in
          
          if has_anonymous_view_patermeters then
            Either.left ()
          else
            binder |> List.map(fun v -> List.mem v view_argument_names) |> Either.right
        in

        let process_intermediate_clause (parameters: view_parameters) (binder: named_var list) (clause: intermediate_clause): (unit, bool list) Either.t =
          match clause with
            | ImPositive (predicate, arguments) -> process_bound_predicate parameters binder predicate arguments
            | ImNegative (predicate, arguments) -> process_bound_predicate parameters binder predicate arguments
            | ImEquation    _ -> binder |> List.map(fun _ -> false) |> Either.right
            | ImNonequation _ -> binder |> List.map(fun _ -> false) |> Either.right
            | ImConstTerm   _ -> binder |> List.map(fun _ -> false) |> Either.right
        in

        let process_rule_abstraction (parameters: view_parameters) (rule: rule_abstraction): (unit, bool list) Either.t =
          rule.body |>
          List.map (process_intermediate_clause parameters rule.binder) |>
          merge_view_parameter_result_list
        in

        let process_predicate_definition (either: (unit, view_parameters) Either.t) (definition: predicate_definition): (unit, view_parameters) Either.t =
          match either with
            | Either.Left () -> Either.Left ()
            | Either.Right parameters ->
              let (predicate, rule_set) = definition in
              let rule_results = rule_set |> RuleAbstractionSet.to_list |> List.map(process_rule_abstraction parameters) in
              let result = rule_results |> merge_view_parameter_result_list in
              Either.map_right (fun result -> PredicateMap.add predicate result parameters) result
        in

        List.fold_left process_predicate_definition (Either.right PredicateMap.empty) sorted_predicate_definitions |>
        Either.is_right
      in
      
      let is_linear_occurrence =

        let poc_of_predicate (counts: pararell_occurrence_counts) (predicate: intermediate_predicate): int =
          if is_view_predicate predicate then
            1
          else
            PredicateMap.find_opt predicate counts |> Option.value ~default:0
        in

        let poc_of_intermediate_clause (counts: pararell_occurrence_counts) (clause: intermediate_clause): int =
          match clause with
            | ImPositive (predicate, _) -> poc_of_predicate counts predicate
            | ImNegative (predicate, _) -> min (poc_of_predicate counts predicate) 1
            | ImEquation    _ -> 0
            | ImNonequation _ -> 0
            | ImConstTerm   _ -> 0
        in

        let poc_of_rule_abstraction (counts: pararell_occurrence_counts) (rule: rule_abstraction): int =
          rule.body |>
          List.map (poc_of_intermediate_clause counts) |>
          List.fold_left max 0
        in
        
        let poc_of_rule_abstraction_list (counts: pararell_occurrence_counts) (rule_list: rule_abstraction list): int =
          rule_list |>
          List.map(poc_of_rule_abstraction counts) |>
          List.fold_left (+) 0
        in
        
        let soc_of_intermediate_clause (counts: pararell_occurrence_counts) (clause: intermediate_clause): int =
          match clause with
            | ImPositive (predicate, _) -> min (poc_of_predicate counts predicate) 1
            | ImNegative (predicate, _) -> poc_of_predicate counts predicate
            | ImEquation    _ -> 0
            | ImNonequation _ -> 0
            | ImConstTerm   _ -> 0
        in

        let soc_of_rule_abstraction (counts: pararell_occurrence_counts) (rule: rule_abstraction): int =
          rule.body |>
          List.map (soc_of_intermediate_clause counts) |>
          List.fold_left (+) 0
        in
        
        let soc_of_rule_abstraction_list (counts: pararell_occurrence_counts) (rule_list: rule_abstraction list): int =
          rule_list |>
          List.map(soc_of_rule_abstraction counts) |>
          List.fold_left max 0
        in
        
        let process_predicate_definition (either: (unit, pararell_occurrence_counts) Either.t) (definition: predicate_definition): (unit, pararell_occurrence_counts) Either.t =
          match either with
            | Either.Left () -> Either.Left ()
            | Either.Right counts ->
              let (predicate, rule_set) = definition in
              let rule_list = rule_set |> RuleAbstractionSet.to_list in
              let soc = soc_of_rule_abstraction_list counts rule_list in

              if soc <= 1 then
                let poc = poc_of_rule_abstraction_list counts rule_list in
                Either.right (PredicateMap.add predicate poc counts)
              else
                Either.left ()
        in

        List.fold_left process_predicate_definition (Either.right PredicateMap.empty) sorted_predicate_definitions |> Either.is_right
      in

      let is_linear_view = has_anonymous_view_patermeters && is_linear_occurrence in

      let linear_view_rules =
        let convert_term: term -> term = function
          | Rel (Pred (name, vars)) ->
            if name = view_name then Rel (Deltainsert (name, vars)) else Rel (Pred (name, vars))
          | Not (Pred (name, vars)) -> 
            if name = view_name then Rel (Deltadelete (name, vars)) else Not (Pred (name, vars))
          | Rel rterm -> Rel rterm
          | Not rterm -> Not rterm
          | Equat eterm -> Equat eterm
          | Noneq eterm -> Noneq eterm
          | ConstTerm bool -> ConstTerm bool 
        in

        ast.rules |> List.map(fun (head, body) -> (head, body |> List.map convert_term)) in

      return (
        if is_linear_view then 
          List.rev linear_view_rules
        else
          delete_rule :: insert_rule :: List.rev not_linear_view_rules
      )

    | None -> err NoViewVariable

