
open Expr

(** The prefix used for generated predicates. *)
let constraint_predicate_prefix = "__constraint__"

let remove_bottom (ast: expr): expr =

  let preds =
    Seq.iterate (fun i -> i + 1) 0 |>
    Seq.take (List.length ast.constraints) |>
    List.of_seq |>
    List.map (fun i -> Pred (constraint_predicate_prefix ^ Int.to_string i, []))
   in

  let crules =
    (Pred (constraint_predicate_prefix, []), List.map (fun p -> Not p) preds) ::
    List.map2 (fun c pred -> match c with (_, body) -> (pred, body)) ast.constraints preds
  in

  { ast with rules = List.append ast.rules crules; constraints = [] }
