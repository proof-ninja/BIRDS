
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

module RuleAbstraction: sig
  type t = rule_abstraction

  val compare : t -> t -> int
end

module RuleAbstractionSet: sig
  include module type of Set.Make(RuleAbstraction)
end

type predicate_definition =
  intermediate_predicate * RuleAbstractionSet.t

module Predicate: sig
  type t = intermediate_predicate

  val compare : t -> t -> int
end

module PredicateMap: sig
  include module type of Map.Make(Predicate)
end

type intermediate_program = RuleAbstractionSet.t PredicateMap.t

type error =
  | UnexpectedHeadVarForm  of var
  | UnexpectedBodyVarForm  of var
  | NoViewVariable
  | CyclicDependency       of predicate_definition list

val convert_rule: rule -> (intermediate_predicate * rule_abstraction, error) result
    
(** Adds a mapping `(impred |-> ruleabs)` to `improg`. *)
val add_rule_abstraction: intermediate_predicate -> rule_abstraction -> intermediate_program -> intermediate_program

val inject_rule: intermediate_predicate -> rule_abstraction -> rule
