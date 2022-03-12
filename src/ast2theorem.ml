(*******************************************************)
(**
Theorem generation for verification
 *)
(********************************************************)
(*
@author: Vandang Tran
*)

open Expr
open Utils
open Rule_preprocess
open Stratification

open Lib
open Formulas
open Fol
open Fol_ex

(*
type lambda_variable = string

type lambda_term =
  | LambdaApply  of lambda_term * lambda_term list
  | LambdaVar    of lambda_variable
  | LambdaAbs    of lambda_variable list * lambda_term
  | LambdaExists of lambda_variable list * lambda_term
  | LambdaNot    of lambda_term
  | LambdaConj   of lambda_term list
  | LambdaDisj   of lambda_term list


let rec lambda_of_symtkey (idb : symtable) (cnt : colnamtab) (goal : symtkey) =
  let rule_lst =
    try Hashtbl.find idb goal with
    | Not_found ->
        print_endline ("Not_found in idb the goal "^string_of_symtkey goal);
        exit 0;
  in
  (* disjunction of all rules then we have lambda expression for a idb predicate*)
  let lambda_of_rule_lst (idb : symtable) (cnt : colnamtab) (rules : (rterm * term list) list) =
    let lambda_of_rule (idb : symtable) (cnt : colnamtab) (rule : rterm * term list) =
      let lambda_of_rterm (r : rterm) : lambda_term =
        let lst = get_rterm_varlist r in
        (* convert anonymous variables to named variable with alias,
           they will be existential variables *)
        let (anony_names, var_lst) =
          List.fold_right (fun v (anony_names, vars) ->
            match v with
            | AnonVar ->
                let alias = "anon_" ^ string_of_int (List.length lst - 1 - List.length vars) in
                (alias :: anony_names, (NamedVar alias) :: vars)

            | _ ->
                (anony_names, v :: vars)
          ) lst ([], [])
        in

        let lambda_body =
          let lambda_fun =
            if Hashtbl.mem idb (symtkey_of_rterm r) then
            (* in the case that the predicate is of idb relation, need to recursive construct lambda expression for it *)
              lambda_of_symtkey idb cnt (symtkey_of_rterm r)
            else
            (* if this predicate is of an edb relation, just need to call by its name *)
              LambdaVar (get_rterm_predname r)
          in
          let lambda_args = var_lst |> List.map (fun var -> LambdaVar (string_of_var var)) in
          LambdaApply (lambda_fun, lambda_args)
        in
        LambdaExists (anony_names, lambda_body)
(* ORIGINAL:
        (if List.length anony_names > 0 then "∃ " ^ String.concat " " anony_names ^ ", " else "") ^
          if Hashtbl.mem idb (symtkey_of_rterm r) then
            (* in the case that the predicate is of idb relation, need to recursive construct lambda expression for it *)
            "(" ^ lambda_of_symtkey idb cnt (symtkey_of_rterm r) ^ ") " ^ String.concat "  " (List.map string_of_var var_lst)
          else
            (* if this predicate is of an edb relation, just need to call by its name *)
            get_rterm_predname r ^ " " ^ String.concat "  " (List.map string_of_var var_lst)
*)
      in
      let lambda_of_term (t : term) : lambda_term =
        match t with
        | Rel r -> lambda_of_rterm r
        | _     -> failwith "TODO: lambda_of_term"
      in
      let head = rule_head rule in
      let body = rule_body rule in
      let (p_rt, n_rt, all_eqs, all_ineqs) = split_terms body in
      (* lambda argument is vars in head *)
      (* existential vars of the body is vars in body but not in the head *)
      let exvars =
        VarSet.filter
          (fun x -> not (is_anon x))
          (VarSet.diff (get_termlst_varset body) (VarSet.of_list (get_rterm_varlist head)))
      in
      (* positive predicate: *)
      let lambdas_pos = p_rt |> List.map lambda_of_rterm in
      (* negative predicate: *)
      let lambdas_neg = n_rt |> List.map (fun rt -> LambdaNot (lambda_of_rterm rt)) in
      (* conjunction of all_eqs and all_ineqs: *)
      let lambdas_eq_and_ineq : lambda_term list =
        (List.append all_eqs all_ineqs) |> List.map lambda_of_term
      in
      LambdaAbs (List.map string_of_var (get_rterm_varlist head),
        (* for existential variables *)
        LambdaExists (List.map string_of_var (VarSet.elements exvars),
          LambdaConj (List.concat [lambdas_pos; lambdas_neg; lambdas_eq_and_ineq])))
(* ORIGINAL:
      "λ " ^ String.concat " " (List.map string_of_var (get_rterm_varlist head))
        ^ ", "
      (* for existential variables *)
        ^ (if VarSet.is_empty exvars then "" else "∃ " ^ String.concat " " (List.map string_of_var (VarSet.elements exvars)) ^", ")
      (* positive predicate *)
        ^ String.concat " ∧ " (List.map (fun x -> "(" ^ lambda_of_rterm x ^ ")") p_rt)
      (* negative predicate *)
        ^ (if List.length n_rt > 0 then " ∧ " else "") ^ String.concat " ∧ " (List.map (fun x -> "¬ (" ^ lambda_of_rterm x^")" ) n_rt)
      (* conjunction of all_eqs and all_ineqs *)
        ^ (if List.length (all_eqs @ all_ineqs) > 0 then " ∧ " else "" ) ^ String.concat " ∧ " (List.map (fun x -> "(" ^ string_of_term x^")" ) (all_eqs @ all_ineqs))
*)
    in
    let lambda_list = List.map (lambda_of_rule idb cnt) rules in
    let cols = gen_cols 0 (snd goal) in
    let lambda_args = cols |> List.map (fun col -> LambdaVar col) in
    LambdaAbs (cols,
      LambdaDisj (List.map (fun pred -> LambdaApply (pred, lambda_args)) lambda_list))
(* ORIGINAL:
    "λ " ^ String.concat " " cols ^ ", " ^
      String.concat " ∨ "  (List.map (fun pred -> "(" ^ pred ^ ") " ^ String.concat " " cols) lambda_list)
*)
  in
  let lambda_expr = lambda_of_rule_lst idb cnt rule_lst in
  lambda_expr


(** Take a query term and rules of IDB relations stored in a symtable, generate lambda expression for it. *)
let lambda_of_query (idb : symtable) (cnt : colnamtab) (query : rterm) : lambda_term =
  (* query is just a rterm which is a predicate therefore need to create a new temporary rule for this query term
     for example if query is q(X,Y,_,5) we create a rule for it: _dummy_(X,Y) :- q(X,Y,_,Z), Z=5.
     (_dummy_ is a fixed name in the function rule_of_query) *)
  let qrule = rule_of_query query idb in
  (* qrule is in the form of _dummy_(x,y) :- query_predicate(x,y), x=1 *)
  let local_idb = Hashtbl.copy idb in
  (* because insert a temporary dummy qrule, we should work with a local variable of idb *)
  symt_insert local_idb qrule;
  lambda_of_symtkey local_idb cnt (symtkey_of_rterm (rule_head qrule))


(** Generate lambda expression from the ast, the goal is the query predicate of datalog program. *)
let lambda_of_stt (debug : bool) (prog : expr) : lambda_term =
  let edb = extract_edb prog in
  (* todo: need to check if prog is non-recursive *)
  let view_rt = get_schema_rterm (get_view prog) in
  (* Extract and pre-process the IDB from the program *)
  let idb = extract_idb prog in
  preprocess_rules idb;
  (* print_symtable idb; *)
  (* Build the colnamtab for referencing the table's columns *)
  let cnt = build_colnamtab edb idb in
  (* Return the desired lambda expression *)
  let lambda = lambda_of_query idb cnt view_rt  in
  lambda


(* transform edb relations to a list of functions from product of n (the arity) types to Prop *)
let edb_to_func_types edb =
  (* currently just set all the types are int (ℤ) *)
  let rel_to_function rel =
    get_rterm_predname rel ^ ": " ^
      String.concat " → " ( List.map (fun x -> "ℤ") (get_rterm_varlist rel)) ^ " → Prop"
  in
  let p_el funcs s = (rel_to_function (rule_head s)) :: funcs in
  let p_lst _ lst funcs = (List.fold_left p_el [] lst) @ funcs in
  Hashtbl.fold p_lst edb []
*)


let stype_to_lean_type (st : stype) =
  match st with
  (* | Sint -> "ℤ" *)
  | Sint -> "int"
  (* | Sreal -> "ℝ" *)
  (* | Sreal -> "real" *)
  | Sreal -> "rat"
  | Sbool -> "Prop"
  | Sstring -> "string"


let stype_to_z3_type (st : stype) =
  match st with
  (* | Sint -> "ℤ" *)
  | Sint -> "Int"
  (* | Sreal -> "ℝ" *)
  | Sreal -> "Real"
  | Sbool -> "Bool"
  | Sstring -> "String"


type lean_type =
  | LeanBaseType of stype
  | LeanFuncType of lean_type * lean_type


(* transform source relations in program to a list of functions from product of n (the arity) types to Prop *)
let source_to_lean_func_types (prog : expr) : (string * lean_type) list =
  (* currently just set all the types are int (ℤ) *)
  let p_el (funcs : (string * lean_type) list) ((name, lst) : source) =
    let ltyp =
      List.fold_right (fun (_col, styp) ltyp -> LeanFuncType (LeanBaseType styp, ltyp)) lst (LeanBaseType Sbool)
    in
    (name, ltyp) :: funcs
(* ORIGINAL:
    (name ^ ": " ^ String.concat " → " (List.map (fun (col, typ) -> stype_to_lean_type typ) lst) ^ " → Prop") :: funcs
*)
  in
  List.fold_left p_el [] prog.sources


(* transform source relations in program to a list of functions from product of n (the arity) types to Prop *)
let source_to_z3_func_types (prog : expr) =
  (* currently just set all the types are int (ℤ) *)
  let p_el funcs (name, lst) =
    ("(declare-fun " ^ name ^ " (" ^
      String.concat " " (List.map (fun (col, typ) -> stype_to_z3_type typ) lst) ^ ") Bool)") :: funcs
  in
  List.fold_left p_el [] prog.sources


(* transform source and view relations in program to a list of functions from product of n (the arity) types to Prop *)
let source_view_to_lean_func_types (prog : expr) : (string * lean_type) list =
  (* currently just set all the types are int (ℤ) *)
  let p_el (funcs : (string * lean_type) list) ((name, lst) : source) =
    let ltyp =
      List.fold_right (fun (_col, styp) ltyp -> LeanFuncType (LeanBaseType styp, ltyp)) lst (LeanBaseType Sbool)
    in
    (name, ltyp) :: funcs
(* ORIGINAL:
    (name ^ ": " ^ String.concat " → " ( List.map (fun (col, typ) -> stype_to_lean_type typ) lst) ^ " → Prop" ) :: funcs
*)
  in
  List.fold_left p_el [] (get_schema_stts prog)


(* transform source and view relations in program to a list of functions from product of n (the arity) types to Prop *)
let source_view_to_z3_func_types (prog : expr) =
  (* currently just set all the types are int (ℤ) *)
  let p_el funcs (name, lst) =
    ("(declare-fun " ^ name ^ " (" ^
      String.concat " " (List.map (fun (col, typ) -> stype_to_z3_type typ) lst) ^ ") Bool)") :: funcs
  in
  List.fold_left p_el [] (get_schema_stts prog)

(*
(* take a view update datalog program and generate the theorem of checking whether all delta relations are disjoint *)
let lean_theorem_of_disjoint_delta (debug : bool) (prog : expr) =
  (* need to change the view (in query predicate) to a edb relation *)
  let edb = extract_edb prog in
  let view_rt = get_schema_rterm (get_view prog) in
  (* need to convert the query to be an edb relation *)
  symt_insert edb (view_rt, []);
  let idb = extract_idb prog in
  preprocess_rules idb;
  let cnt = build_colnamtab edb idb in
  let delta_rt_lst = get_delta_rterms prog in
  (* get each pair of delta relations from the delta relation lst delta_rt_lst *)
  let delta_pair_lst =
    let pair_of_delta_insert (lst : (rterm * rterm) list) (ins_rel : rterm) =
      let del_rels = List.filter (is_delta_pair ins_rel) delta_rt_lst in
      if List.length del_rels = 0 then lst else (ins_rel, (List.hd del_rels)) :: lst
    in
    List.fold_left pair_of_delta_insert [] delta_rt_lst
  in

  (* get the emptiness FO sentence of a relation *)
  let disjoint_fo_sentence (ins_rel : rterm) (del_rel : rterm) : lambda_term =
    let cols = gen_cols 0 (get_arity ins_rel) in
    let lambda_args = cols |> List.map (fun col -> LambdaVar col) in
    let lambda_ins = LambdaApply (lambda_of_query idb cnt ins_rel, lambda_args) in
    let lambda_del = LambdaApply (lambda_of_query idb cnt del_rel, lambda_args) in
    LambdaExists (cols, LambdaConj [lambda_ins; lambda_del])
(* ORIGINAL:
    "∃ " ^ String.concat " " cols ^ ", (" ^  (lambda_of_query idb cnt ins_rel) ^ ") "
      ^ String.concat " " cols ^ " ∧ " ^ "(" ^  (lambda_of_query idb cnt del_rel) ^ ") "
      ^ String.concat " " cols
*)
  in
  let disjoint_sen_lst = List.map (fun (r1, r2) -> disjoint_fo_sentence r1 r2) delta_pair_lst in
  "theorem disjoint_deltas "
    ^ String.concat " " (List.map (fun x -> "{" ^ x ^ "}") (source_view_to_lean_func_types prog))
    ^ ": " ^ (String.concat " ∨ " (List.map (fun pred -> "(" ^ pred ^ ")") disjoint_sen_lst)) ^ " → false"
*)

type lean_theorem =
  | LeanTheorem of {
      name      : string;
      parameter : (string * lean_type) list;
      statement : Fol_ex.lean_formula;
    }


(* take a view update datalog program and generate the theorem of checking whether all delta relations are disjoint *)
let lean_simp_theorem_of_disjoint_delta (debug : bool) (prog : expr) : lean_theorem =
  if debug then print_endline "==> generating theorem for disjoint deltas" else ();
  let statement =
    Fol_ex.lean_formula_of_fol_formula
      (Imp (Ast2fol.constraint_sentence_of_stt debug prog,
        (Imp (Ast2fol.disjoint_delta_sentence_of_stt debug prog, False))))
  in
  LeanTheorem {
    name      = "disjoint_deltas";
    parameter = source_view_to_lean_func_types prog;
    statement = statement;
  }
(* ORIGINAL:
  "theorem disjoint_deltas "
    ^ String.concat " " (List.map (fun x -> "{" ^ x ^"}") (source_view_to_lean_func_types prog)) ^ ": "
    ^ (Fol_ex.lean_string_of_fol_formula
        (Imp (Ast2fol.constraint_sentence_of_stt debug prog,
          (Imp (Ast2fol.disjoint_delta_sentence_of_stt debug prog, False)))))
*)

let lean_simp_theorem_of_getput (debug : bool) (prog : expr) : lean_theorem =
  if debug then print_endline "==> generating theorem of getput property" else ();
  let statement =
    Fol_ex.lean_formula_of_fol_formula
      (Imp (Ast2fol.non_view_constraint_sentence_of_stt debug prog,
        (Imp (Ast2fol.getput_sentence_of_stt debug prog, False))))
  in
  LeanTheorem {
    name      = "getput";
    parameter = source_to_lean_func_types prog;
    statement = statement;
  }
(* ORIGINAL:
  "theorem getput " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (source_to_lean_func_types prog)) ^ ": "
    ^ (Fol_ex.lean_string_of_fol_formula
         (Imp (Ast2fol.non_view_constraint_sentence_of_stt debug prog,
           (Imp (Ast2fol.getput_sentence_of_stt debug prog, False)))))
*)

let lean_simp_theorem_of_putget (debug : bool) (prog : expr) : lean_theorem =
  if debug then print_endline "==> generating theorem of putget property" else ();
  let statement =
    Fol_ex.lean_formula_of_fol_formula
      (Imp (Ast2fol.constraint_sentence_of_stt debug prog, Ast2fol.putget_sentence_of_stt debug prog))
  in
  LeanTheorem {
    name      = "putget";
    parameter = source_view_to_lean_func_types prog;
    statement = statement;
  }
(* ORIGINAL:
  "theorem putget "
    ^ String.concat " " (List.map (fun x -> "{" ^ x ^ "}") (source_view_to_lean_func_types prog)) ^ ": "
    ^ (Fol_ex.lean_string_of_fol_formula
        (Imp (Ast2fol.constraint_sentence_of_stt debug prog, Ast2fol.putget_sentence_of_stt debug prog)))
*)

(* take a view update datalog program and generate the theorem of checking whether all delta relations are disjoint *)
let z3_assert_of_disjoint_delta (debug : bool) (prog : expr) =
  if debug then print_endline "==> generating z3 assert for disjoint deltas" else ();
  String.concat "\n" (source_view_to_z3_func_types prog) ^ "\n (assert "
    ^ (Fol_ex.z3_string_of_fol_formula
        (Not (Imp (Ast2fol.constraint_sentence_of_stt debug prog,
          (Imp (Ast2fol.disjoint_delta_sentence_of_stt debug prog, False))))))
    ^ ") \n (check-sat)"


let z3_assert_of_getput (debug : bool) (prog : expr) =
  if debug then print_endline "==> generating z3 assert of getput property" else ();
  String.concat "\n" (source_to_z3_func_types prog) ^ "\n(assert "
    ^ (Fol_ex.z3_string_of_fol_formula
        (Not (Imp (Ast2fol.non_view_constraint_sentence_of_stt debug prog,
          (Imp (Ast2fol.getput_sentence_of_stt debug prog, False))))))
    ^ ") \n (check-sat)"


let z3_assert_of_putget (debug : bool) (prog : expr) =
  if debug then print_endline "==> generating z3 assert of putget property" else ();
  String.concat " " (source_view_to_z3_func_types prog) ^ "\n (assert "
    ^ (Fol_ex.z3_string_of_fol_formula
        (Not (Imp (Ast2fol.constraint_sentence_of_stt debug prog, Ast2fol.putget_sentence_of_stt debug prog))))
    ^ ")\n (check-sat)"

(*
(* (unnecessary now see sourcestability_sentence_of_stt in ast2fol.ml) take a view update datalog program and generate SourceStability constraint (put s v = s) for its view update strategy *)
let sourcestability_of_stt (debug : bool) (prog : expr) =
  let edb = extract_edb prog in
  (* need to change the view (in query predicate) to a edb relation *)
  let view_rt = get_schema_rterm (get_view prog) in
  (* need to convert the view to be an edb relation *)
  symt_insert edb (view_rt,[]);
  let idb = extract_idb prog in
  symt_remove idb (symtkey_of_rterm view_rt);
  preprocess_rules idb;
  let cnt = build_colnamtab edb idb in
  let delta_rt_lst = get_delta_rterms prog in
  (* get the emptiness FO sentence of a relation *)
  let emptiness_fo_sentence (rel : rterm) =
    let cols = gen_cols 0 (get_arity rel) in
    "∃ " ^ String.concat " " cols ^ ", (" ^ (lambda_of_query idb cnt rel) ^ ") " ^ String.concat " " cols in
    let delta_lambda_exp_lst = List.map emptiness_fo_sentence delta_rt_lst in
    "theorem sourcestability "
      ^ String.concat " " (List.map (fun x -> "{" ^ x ^ "}") (source_view_to_lean_func_types prog)) ^ ": "
      ^ String.concat " ∨ " (List.map (fun pred -> "(" ^ pred ^ ")") delta_lambda_exp_lst) ^ " → false"
*)

let lean_simp_sourcestability_theorem_of_stt (debug : bool) (prog : expr) : lean_theorem =
  let statement =
    Fol_ex.lean_formula_of_fol_formula
      (Imp (Ast2fol.sourcestability_sentence_of_stt debug prog, False))
  in
  LeanTheorem {
    name      = "sourcestability";
    parameter = source_to_lean_func_types prog;
    statement = statement;
  }
(* ORIGINAL:
  "theorem sourcestability "
    ^ String.concat " " (List.map (fun x -> "{" ^ x ^ "}") (source_to_lean_func_types prog)) ^ ": "
    ^ (Fol_ex.lean_string_of_fol_formula (Imp (Ast2fol.sourcestability_sentence_of_stt debug prog, False)))
*)


let lean_theorem_of_view_existence (log : bool) (prog : expr) (sentence_of_view_existence : Fol.fol Formulas.formula) (phi : Fol.fol Formulas.formula) : lean_theorem =
  let statement =
    Fol_ex.lean_formula_of_fol_formula
      (Imp (Ast2fol.non_view_constraint_sentence_of_stt log prog,
        And (sentence_of_view_existence, Fol.generalize (Imp (phi, False)))))
  in
  LeanTheorem {
    name      = "view_existence";
    parameter = source_to_lean_func_types prog;
    statement = statement;
  }
(* ORIGINAL:
  "theorem view_existence " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (Ast2theorem.source_to_lean_func_types prog)) ^
       ": " ^ (Fol_ex.lean_string_of_fol_formula (Imp (non_view_constraint_sentence_of_stt log prog,
       And(sentence_of_view_existence, generalize (Imp (phi, False))))))
*)


(** Take a put datalog program and generate the FO sentence of checking whether the view is unique or not. *)
let view_uniqueness_sentence_of_stt (log : bool) (prog : Expr.expr) =
  let fm = Ast2fol.sourcestability_sentence_of_stt log prog in
  let view_name = Expr.get_rterm_predname (Expr.get_schema_rterm (get_view prog)) in
  let view_vars =
    List.map (fun x -> Expr.string_of_var x) @@ Expr.get_rterm_varlist (Expr.get_schema_rterm (get_view prog))
  in
  let (phi, lst) = Fol_ex.ranf2lvnf view_name fm in
  (* we do not need vars any more because it must be all free variables in vfol and phi_i *)
  let lst2 =
    List.map (fun (vars, vfol, phi_i) ->
      match vfol with
      | Atom (R (view_name, lst)) | Not (Atom (R (view_name, lst))) ->
          let subfn =
            fpf (List.map (fun x -> Fol_ex.string_of_term 0 x) lst) (List.map (fun x -> Fol.Var x) view_vars)
          in
          (subst subfn vfol, subst subfn phi_i)

      | _ ->
          (vfol, phi_i)
    ) lst
  in
  if log then begin
    print_endline "===> solving sourcestability constraint to check view uniqueness";
    print_endline "______constraints from view-predicate normal form_______";
    print_endline @@ "phi: " ^ (stringify_lean_formula (lean_formula_of_fol_formula phi));
    List.iter (fun (vfol, phi_i) ->
      print_endline @@ "false <=> : " ^ (stringify_lean_formula (lean_formula_of_fol_formula vfol)) ;
      print_endline @@ ", " ^ (stringify_lean_formula (lean_formula_of_fol_formula phi_i)) ^ "\n";
    ) lst2;
  end;

  (* contruct a upper bound FO formula of view *)
  let view_upper_fol =
    List.fold_left (fun fm (vfol, phi_i) ->
      match vfol with
      | Not (Atom (R (view_name, lst))) ->
          let ex_vars = subtract (fv phi_i) view_vars in
          Or(fm, itlist mk_exists ex_vars phi_i)

      | _ ->
          fm
    ) False lst2
  in
  if log then print_endline @@ "upper bound of view: "^ (stringify_lean_formula (lean_formula_of_fol_formula view_upper_fol));

  (* contruct a lower bound FO formula of view *)
  let view_lower_fol =
    List.fold_left (fun fm (vfol, phi_i) ->
      match vfol with
      | Atom (R (view_name, lst)) ->
          let ex_vars = subtract (fv phi_i) view_vars in
          Or (fm, Not (itlist mk_exists ex_vars phi_i))

      | _ ->
          fm
    ) False lst2
  in
  if log then print_endline @@ "lower bound of view: " ^ (stringify_lean_formula (lean_formula_of_fol_formula view_lower_fol));

  (* make the equivalence sentence of checking whether upper FOL and lower FOL of view are equvalent *)
  let sentence_of_view_uniqueness = generalize (Iff (view_upper_fol, view_lower_fol)) in
  if log then (print_endline @@ "FO sentence of view uniqueness : " ^ (stringify_lean_formula (lean_formula_of_fol_formula sentence_of_view_uniqueness));
  print_endline "_______________________________________\n");
  sentence_of_view_uniqueness


(* Take a view update put datalog program and generate the theorem of checking view uniqueness. *)
let lean_simp_theorem_of_view_uniqueness (log : bool) (prog : expr) : lean_theorem =
  if log then print_endline "==> generating theorem for view uniqueness" else ();
  let statement =
    Fol_ex.lean_formula_of_fol_formula
      (Imp (Ast2fol.constraint_sentence_of_stt log prog, view_uniqueness_sentence_of_stt log prog))
  in
  LeanTheorem {
    name = "view_uniqueness";
    parameter = source_to_lean_func_types prog;
    statement = statement;
  }
(* ORIGINAL:
  "theorem view_uniqueness " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (Ast2theorem.source_to_lean_func_types prog)) ^
   ": " ^ (Fol_ex.lean_string_of_fol_formula (Imp (Ast2fol.constraint_sentence_of_stt log prog,
   view_uniqueness_sentence_of_stt log prog)))
*)


let stringify_lean_type (ltyp : lean_type) : string =
  let rec aux (ltyp : lean_type) : string =
    match ltyp with
    | LeanBaseType styp           -> stype_to_lean_type styp
    | LeanFuncType (ltyp1, ltyp2) -> Printf.sprintf "(%s -> %s)" (aux ltyp1) (aux ltyp2)
  in
  aux ltyp


let stringify_lean_theorem (thm : lean_theorem) : string =
  let LeanTheorem { name; parameter; statement } = thm in
  let s_parameter =
    parameter |> List.map (fun (x, ltyp) ->
      Printf.sprintf " {%s: %s}" x (stringify_lean_type ltyp)
    ) |> String.concat ""
  in
  let s_statement = stringify_lean_formula statement in
  Printf.sprintf "theorem %s%s: %s" name s_parameter s_statement


let gen_lean_code_for_theorems (thms : lean_theorem list) : string =
    "import bx

local attribute [instance] classical.prop_decidable

" ^ String.concat "\n\n" (List.map (fun x ->  (stringify_lean_theorem x) ^ ":=
    begin
    z3_smt
    end") thms)
    (* try{super {max_iters := 200, timeout := 200000}} *)


let validity_lean_code_of_bidirectional_datalog (debug : bool) (prog : expr) : string =
  gen_lean_code_for_theorems [
    lean_simp_theorem_of_disjoint_delta debug prog;
    lean_simp_theorem_of_getput debug prog;
    lean_simp_theorem_of_putget debug prog;
  ]
