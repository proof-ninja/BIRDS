
open Expr
open Utils

(** The prefix used for variables generated during incrementalization. *)
let updated_variable_prefix = "__updated__"

type error =
  | NoViewVariable

let string_of_error = function
  | NoViewVariable ->
      "There is no view in this program."

let incrementalization_ast (ast: expr): (rule list, error) result =
  let open ResultMonad in

  match ast.view with
    | Some (view_name, params) ->

      let updated_view_name = updated_variable_prefix ^ view_name  in

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

      let rules = ast.rules |> List.map(fun (head, body) -> (head, body |> List.map convert_term)) in

      let param_vars = params |> List.map(fun (var_name, _) -> NamedVar(var_name)) in

      let delete_rule = Pred (updated_view_name, param_vars), Rel (Pred (view_name, param_vars)) :: Not (Deltadelete (view_name, param_vars)) :: [] in

      let insert_rule = Pred (updated_view_name, param_vars), Rel (Deltainsert (view_name, param_vars)) :: [] in

      return (delete_rule :: insert_rule :: List.rev rules)

    | None -> err NoViewVariable

