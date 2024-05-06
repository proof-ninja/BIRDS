
open Expr
open Utils

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

type intermediate_program = RuleAbstractionSet.t PredicateMap.t

type error =
  | UnexpectedHeadVarForm  of var
  | UnexpectedBodyVarForm  of var
  | NoViewVariable
  | CyclicDependency       of predicate_definition list

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

let inject_rterm (impred: intermediate_predicate) (imargs: intermediate_argument list): rterm =
  let vars =
    imargs |> List.map (function
    | ImNamedVarArg (ImNamedVar x) -> NamedVar x
    | ImConstArg const -> ConstVar const
    | ImAnonVarArg -> AnonVar
    )
  in
  match impred with
  | ImPred table -> Pred (table, vars)
  | ImDeltaInsert table -> Deltainsert (table, vars)
  | ImDeltaDelete table -> Deltadelete (table, vars)


let inject_clause (clause: intermediate_clause): term =
  match clause with
  | ImPositive (impred, imargs) -> Rel (inject_rterm impred imargs)
  | ImNegative (impred, imargs) -> Not (inject_rterm impred imargs)
  | ImEquation eterm  -> Equat eterm
  | ImNonequation eterm -> Noneq eterm
  | ImConstTerm bool -> ConstTerm bool


let inject_rule (impred: intermediate_predicate) (ruleabs: rule_abstraction): rule =
  let { binder; body } = ruleabs in
  let rterm = binder |> List.map (fun var -> ImNamedVarArg var) |> inject_rterm impred in
  let terms = body |> List.map inject_clause in
  (rterm, terms)
      
let inject_rules (improg: intermediate_program): rule list =
    PredicateMap.fold (fun impred ruleabsset acc ->
      let ruleabss = RuleAbstractionSet.elements ruleabsset in
      ruleabss |> List.fold_left (fun acc ruleabs ->
        let rule = inject_rule impred ruleabs in
        rule :: acc
      ) acc
    ) improg []
