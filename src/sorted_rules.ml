
open Expr
open Utils
open Rule_abstraction

module PredicateDependencyGraph = Dependency_graph.Make(Predicate)

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
