type const =
  | Int of int
  | Real of float
  | String of string
  | Bool of bool 
  | Null

type var = 
  | NamedVar of string 
  | NumberedVar of int (* this is not used in parser *)
  | ConstVar of const (* var in a literal in allowed to be a const like int or string, for example p(X,1) or p(X, 'tran') *)
  | AnonVar (* anonimous variable *)
  | AggVar of string * string (* the first string is function, the second is variable *)

type vterm = (* value term *)
  | Const of const
  | Var of var
  (* arithmetic expression *)
  | BinaryOp of string * vterm * vterm (* string is one of '+', '-', '*', '/', '^' *)
  | UnaryOp of string * vterm (* string is one of '-' *)

type eterm = (* equation *)
  | Equation of string * vterm * vterm (* string is one of '=', '<>', '<', '>', '<=', '>=' *)

type rterm = (* rterm is literal *)
  | Pred of string * var list (* string is name of predicate, var list is a list of variables *)
  | Deltainsert of string * var list (* delta predicate for insertion *)
  | Deltadelete of string * var list (* delta predicate for deletion *)

type term = (* term is one of predicate (positive or negative), equation, non-equation *)
  | Rel of rterm (* positive predicate *)
  | Not of rterm (* negative predicate *)
  | Equat of eterm  (* for example x = 5 *)
  | Noneq of eterm (* for example NOT x = 5 *)
  | ConstTerm of bool (* true/false *)

type stype = (* data type in schema *)
  | Sint
  | Sreal
  | Sstring
  | Sbool

type rule = rterm * term list

type fact = rterm

type query = rterm

type source = string * (string * stype) list

type view = string * (string * stype) list

type constraint' = rterm * term list

type primary_key = string * string list

type expr = {
  rules: rule list;
  facts: fact list;
  query: query option;
  sources: source list;
  view: view option;
  constraints: constraint' list;
  primary_keys: primary_key list;
}

let compare_multi = List.fold_left (fun a b -> if a = 0 then b else a) 0

module Const = struct
  type t = const

  let compare (c1 : const) (c2 : const) : int =
    match (c1, c2) with
    | (Int n1, Int n2)       -> Int.compare n1 n2
    | (Int _, _)             -> 1
    | (_, Int _)             -> -1
    | (Real r1, Real r2)     -> Float.compare r1 r2
    | (Real _, _)            -> 1
    | (_, Real _)            -> -1
    | (String s1, String s2) -> String.compare s1 s2
    | (String _, _)          -> 1
    | (_, String _)          -> -1
    | (Bool b1, Bool b2)     -> Bool.compare b1 b2
    | (Bool _, _)            -> 1
    | (_, Bool _)            -> -1
    | (Null, Null)           -> 0

  let equal (c1 : const) (c2 : const) : bool =
    compare c1 c2 = 0
end

module Var = struct
  type t = var

  let compare v1 v2 =
    match v1, v2 with
    | NamedVar v1, NamedVar v2 -> String.compare v1 v2
    | NamedVar _, _ -> 1
    | _, NamedVar _ -> -1
    | NumberedVar v1, NumberedVar v2 -> Int.compare v1 v2
    | NumberedVar _, _ -> 1
    | _, NumberedVar _ -> -1
    | ConstVar v1, ConstVar v2 -> Const.compare v1 v2
    | ConstVar _, _ -> 1
    | _, ConstVar _ -> -1
    | AnonVar, AnonVar -> 0
    | AnonVar, _ -> 1
    | _, AnonVar -> -1
    | AggVar (v11, v12), AggVar (v21, v22) -> compare_multi [String.compare v11 v21; String.compare v12 v22]

    let equal v1 v2 =
      compare v1 v2 = 0
end

module VTerm = struct
  type t = vterm

  let rec compare v1 v2 =
    match v1, v2 with
    | Const c1, Const c2 -> Const.compare c1 c2
    | Const _, _ -> 1
    | _, Const _ -> -1
    | Var v1, Var v2 -> Var.compare v1 v2
    | Var _, _ -> 1
    | _, Var _ -> -1
    | BinaryOp (op1, v11, v12), BinaryOp (op2, v21, v22) ->
        compare_multi [String.compare op1 op2; compare v11 v12; compare v21 v22]
    | BinaryOp _, _ -> 1
    | _, BinaryOp _ -> -1
    | UnaryOp (op1, v1), UnaryOp (op2, v2) ->
        compare_multi [String.compare op1 op2; compare v1 v2]
  
  let equal v1 v2 =
    compare v1 v2 = 0
end

module RTerm = struct
  type t = rterm

  let compare r1 r2 =
    match r1, r2 with
    | Pred (n1, vs1), Pred (n2, vs2) -> compare_multi [String.compare n1 n2; List.compare Var.compare vs1 vs2]
    | Pred _, _ -> 1
    | _, Pred _ -> -1
    | Deltainsert (n1, vs1), Deltainsert (n2, vs2) -> compare_multi [String.compare n1 n2; List.compare Var.compare vs1 vs2]
    | Deltainsert _, _ -> 1
    | _, Deltainsert _ -> -1
    | Deltadelete (n1, vs1), Deltadelete (n2, vs2) -> compare_multi [String.compare n1 n2; List.compare Var.compare vs1 vs2]
    
  let equal r1 r2 =
    compare r1 r2 = 0
end

module ETerm = struct
  let compare e1 e2 =
    match e1, e2 with
    | Equation (r1, v11, v12), Equation (r2, v21, v22) ->
        compare_multi [
          String.compare r1 r2;
          VTerm.compare v11 v21;
          VTerm.compare v12 v22
        ]
  
  let equal e1 e2 =
    compare e1 e2 = 0
end

module Term = struct
  type t = term

  let compare t1 t2 =
    match t1, t2 with
    | Rel r1, Rel r2 -> RTerm.compare r1 r2
    | Rel _, _ -> 1
    | _, Rel _ -> -1
    | Not r1, Not r2 -> RTerm.compare r1 r2
    | Not _, _ -> 1
    | _, Not _ -> -1
    | Equat e1, Equat e2 -> ETerm.compare e1 e2
    | Equat _, _ -> 1
    | _, Equat _ -> -1
    | Noneq e1, Noneq e2 -> ETerm.compare e1 e2
    | Noneq _, _ -> 1
    | _, Noneq _ -> -1
    | ConstTerm e1, ConstTerm e2 -> Bool.compare e1 e2
end

module Rule = struct
  type t = rule

  let compare (r1, ts1) (r2, ts2) =
    compare_multi [
      RTerm.compare r1 r2;
      List.compare Term.compare ts1 ts2
    ]
  
  let equal r1 r2 =
    compare r1 r2 = 0
end

module Alpha = struct
  module VarMap = Map.Make(String)

  let getvar state =
    let state = state + 1 in
    state, Printf.sprintf "GENV%d" state

  let convert (rterm, terms) =
    let conv_vars vars =
      let map, _, vars =
        List.fold_left (fun (map, state, vars) var ->
          match var with
          | NamedVar name ->
              let state, name' = getvar state in
              (VarMap.add name name' map), state, (NamedVar name') :: vars
          | other ->
              map, state, other :: vars
        ) (VarMap.empty, 0, []) vars
      in
      map, List.rev vars
    in
    let conv_rterm = function
      | Pred (name, vars) ->
          let map, vars = conv_vars vars in
          map, Pred (name, vars)
      | Deltainsert (name, vars) ->
          let map, vars = conv_vars vars in
          map, Deltainsert (name, vars)
      | Deltadelete (name, vars) ->
          let map, vars = conv_vars vars in
          map, Deltadelete (name, vars)
    in
    let map, rterm = conv_rterm rterm in

    let conv_var = function
      | NamedVar name as var ->
          begin match VarMap.find_opt name map with
          | Some name -> NamedVar name
          | None -> var
          end
      | var -> var
    in
    let rec conv_vterm = function
      | Var var -> Var (conv_var var)
      | BinaryOp (op, v1, v2) -> BinaryOp (op, conv_vterm v1, conv_vterm v2)
      | UnaryOp (op, v) -> UnaryOp (op, conv_vterm v)
      | vterm -> vterm
    in
    let conv_eterm = function
      | Equation (op, v1, v2) -> Equation (op, conv_vterm v1, conv_vterm v2)
    in
    let conv_rterm = function
      | Pred (name, vs) -> Pred (name, List.map conv_var vs)
      | Deltainsert (name, vs) -> Deltainsert (name, List.map conv_var vs)
      | Deltadelete (name, vs) -> Deltadelete (name, List.map conv_var vs)
    in
    let conv_term = function
      | Rel rterm -> Rel (conv_rterm rterm)
      | Not rterm -> Not (conv_rterm rterm)
      | Equat eterm -> Equat (conv_eterm eterm)
      | Noneq eterm -> Noneq (conv_eterm eterm)
      | ConstTerm _ as c -> c
    in
    let terms =
      terms
      |> List.map conv_term
      |> List.sort Term.compare
    in
    rterm, terms
end

module RuleSet = struct
  include Set.Make(Rule)

  module RuleMap = Map.Make(Rule)

  (* let string_of_rules (rules: t): string =
    rules
      |> to_seq
      |> List.of_seq
      |> List.map string_of_rule
      |> String.concat "; "
  ;; *)

  let diff rs1 rs2 =
    let rs1, rule_map =
      fold (fun r (rs, map) ->
        let r' = Alpha.convert r in
        add r' rs, RuleMap.add r' r map
      ) rs1 (empty, RuleMap.empty)
    in
    let rs2 = map Alpha.convert rs2 in
    (* Printf.printf "%s\n" @@ string_of_rules rs1;
    Printf.printf "%s\n" @@ string_of_rules rs2; *)
    let result = diff rs1 rs2 in
    map (fun x -> RuleMap.find x rule_map) result
end


type conj_query =
  | Conj_query of var list * rterm list * rterm list

let get_empty_pred = Pred ("⊥", [])

let get_empty_expr = {
  rules= [];
  facts= [];
  query= None;
  sources= [];
  view= None;
  constraints= [];
  primary_keys= [];
}

type stt =
  | Stt_Rule of rule
  | Stt_Fact of fact
  | Stt_Query of query
  | Stt_Source of source (* the predicate of edb relation which is Source relation want to update *)
  | Stt_View of view
  | Stt_Constraint of constraint'
  | Stt_Pk of primary_key (* primary key *)

(****************************************************
 *
 *  AST accessor / check / transformation functions
 *
 ****************************************************)

let add_stt stt expr = match stt with
  | Stt_Rule rule -> { expr with rules= rule :: expr.rules }
  | Stt_Fact fact -> { expr with facts= fact :: expr.facts }
  | Stt_Query query -> begin match expr.query with
      | Some _ -> invalid_arg "Query should appear at most once"
      | None -> { expr with query= Some query }
    end
  | Stt_Source source -> { expr with sources= source :: expr.sources }
  | Stt_View view -> begin match expr.view with
      | Some _ -> invalid_arg "View should appear at most once"
      | None -> { expr with view= Some view }
    end
  | Stt_Constraint constraint' -> { expr with constraints= constraint' :: expr.constraints }
  | Stt_Pk primary_key -> { expr with primary_keys= primary_key :: expr.primary_keys }

(** add a list of stt to a program *)
let add_stts stts prog = List.fold_left (fun expr stt -> add_stt stt expr) prog stts

let add_rules rs prog = {prog with rules = rs@(prog.rules)}

(** Insert a new statement. *)
let insert_stt stt expr = add_stt stt expr

(** Take a rterm and return its delta of insertion *)
let get_ins_delta_pred del_rterm = match del_rterm with 
    | Pred (x, vl) -> Deltainsert (x, vl) 
    | Deltainsert (x, vl) -> Deltainsert (x, vl) 
    | Deltadelete (x, vl) -> Deltainsert (x, vl) 

(** Take a rterm and return its delta of deletion *)
let get_del_delta_pred del_rterm = match del_rterm with 
    | Pred (x, vl) -> Deltadelete (x, vl) 
    | Deltainsert (x, vl) -> Deltadelete (x, vl) 
    | Deltadelete (x, vl) -> Deltadelete (x, vl) 

(** Take a delta rterm and return a dummy rterm of new source  *)
let get_new_source_rel_pred del_rterm = match del_rterm with 
    | Pred (x, vl) | Deltainsert (x, vl) | Deltadelete (x, vl) -> Pred("__dummy_new_"^ x,vl)

(** Take a delta rterm and return a rterm of source relation *)
let get_source_rel_pred del_rterm = match del_rterm with 
    | Pred (x, vl) | Deltainsert (x, vl) | Deltadelete (x, vl) -> Pred(x,vl)

let get_empty_pred = Pred ("⊥", [])

(** Get the predicate name of an rterm using delta__ins_/delta__del_ for delta predicates. *)
let get_rterm_predname rterm = match rterm with
    | Pred ("⊥", []) -> "bot"
    | Pred (x, _vl) -> x
    | Deltainsert (x, _vl) -> "delta_ins_"^ x
    | Deltadelete (x, _vl) -> "delta_del_"^ x

let is_rule_of_predname predname (h, _b) =  (String.compare (get_rterm_predname h)  predname == 0)

(** Delete all rules of a predname. *)
let delete_rule_of_predname predname expr = { expr with rules= List.filter (fun x -> not (is_rule_of_predname predname x)) expr.rules }

let is_fact_of_predname predname h = (String.compare (get_rterm_predname h)  predname == 0)

(** Delete all rules of a predname. *)
let delete_fact_of_predname predname expr = { expr with facts = List.filter (fun x -> not (is_fact_of_predname predname x)) expr.facts }

(** Change view schemas to source schemas. *)
let view_schema_to_source_schema expr = { expr with 
  view = None; 
  sources = match expr.view with
      | Some v -> v :: expr.sources 
      | None -> expr.sources
}

(** Check whether a predicate is defined in the program .*)
let is_defined_pred predname expr = 
  List.length (List.filter (fun x -> (is_rule_of_predname predname x)) expr.rules) > 0

let vterm2var vt = match vt with 
    Const c -> ConstVar c 
    | Var v -> v 
    | _ -> invalid_arg "function vterm2var called without var or constant"

(** Get the arity of an rterm. *)
let get_arity rterm = match rterm with
    | Pred (_x, vl) -> List.length vl
    | Deltainsert (_x, vl) -> List.length vl
    | Deltadelete (_x, vl) -> List.length vl

(** Get the arity of a rule. *)
let get_rule_arity (h, _b) =  get_arity h

(** Get the predicate name of a term. *)
let get_predname t = match t with
    | Rel r            -> get_rterm_predname r
    | _                -> invalid_arg "function get_predname called without a relation"

(** Get a rule's head predicate name. *)
let get_rule_predname (h, _t) = get_rterm_predname h

(** Get a rule's head pred. *)
let rule_head (h, _) = h

(** Get a rule's body list of terms. *)
let rule_body (_, t) = t

(** Get rterm varlist. *)
let get_rterm_varlist t = match t with
    | Pred (_x, vl) -> vl
    | Deltainsert (_x, vl) -> vl
    | Deltadelete (_x, vl) -> vl

let rec get_vterm_varlist e = match e with 
    | Const _const -> []
    | Var var ->  [var]
    | BinaryOp (_op, ae1, ae2) -> (get_vterm_varlist ae1) @ (get_vterm_varlist ae2) 
    | UnaryOp (_op, ae) -> get_vterm_varlist ae

(** Get the list of variables of a term. *)
let get_term_varlist = function
  | Rel r -> get_rterm_varlist r
  | Equat (Equation (_op, e1, e2)) -> (get_vterm_varlist e1) @ (get_vterm_varlist e2)
  | Noneq (Equation (_op,e1, e2))  -> (get_vterm_varlist e1) @ (get_vterm_varlist e2)
  | Not r -> get_rterm_varlist r
  | ConstTerm _ -> []

(** Given a schema declaration (source and view), returns the rterm that is defined inside. *)
let get_schema_rterm (name, lst) =  Pred(name, (List.map (fun (col, _typ) -> NamedVar col) lst))

(** Given a schema declaration (source and view), returns the attribute list. *)
let get_schema_attrs (_name, lst) = List.map (fun (col, _typ) ->  col) lst

(** Given a schema declaration (source and view), returns the list of column:typ  *)
let get_schema_col_typs (_name, lst) =  lst

(** Given a schema declaration (source and view), returns the schema name.  *)
let get_schema_name (name, _lst) = name

(** Given program return all schema statement. *)
let get_schema_stts expr = match expr.view with
      | Some v -> v :: expr.sources 
      | None -> expr.sources

(** Given program return all source statement. *)
let get_source_stts expr = expr.sources

(** Given a rule, returns all the positive and negative rterms *)
let get_all_rule_rterms (_, t) = 
  let extract_rterm acc = function
            | Rel x -> x::acc
            | Not x -> x::acc
            | _ -> acc in
        List.fold_left extract_rterm [] t

(** Given a rule, returns all the negative rterms. *)
let get_all_negative_rule_rterms (_, t) = 
  let extract_rterm acc = function
            | Not x -> x::acc
            | _ -> acc in
        List.fold_left extract_rterm [] t

(** Given a rule, returns all the negative rterms. *)
let get_all_positive_rule_rterms (_, t) = 
  let extract_rterm acc = function
            | Rel x -> x::acc
            | _ -> acc in
        List.fold_left extract_rterm [] t

(** Given an equality, returns the (var,const) tuple that defines it. *)
let extract_eq_tuple = function
  | Equat (Equation ("=",v,c)) -> (v,c)
  | Noneq (Equation ("<>",v,c)) -> (v,c)
  | _ -> invalid_arg "function extract_eq_tuple called without an equality"

(** Given an equation, returns the equivalent negative form of it. *)
let negate_eq = function
    | Equation ("=", v,c) -> Equation ("<>",v,c)
    | Equation ("<>",v,c) -> Equation ("=",v,c)
    | Equation ("<",v,c) -> Equation (">=",v,c)
    | Equation (">",v,c) -> Equation ("<=",v,c)
    | Equation ("<=",v,c) -> Equation (">",v,c)
    | Equation (">=",v,c) -> Equation ("<",v,c)
    | _ -> invalid_arg "function negate_eq called without an equation"

(** Given an inequality, returns the (op,var,const) tuple that defines it. *)
let extract_ineq_tuple = function
  | Equat (Equation ("=", _v, _c)) -> invalid_arg "function extract_ineq_tuple called without an inequality"
  | Noneq (Equation ("<>", _v, _c)) -> invalid_arg "function extract_ineq_tuple called without an inequality"
  | Equat (Equation (s,v,c)) -> (s,v,c)
  | Noneq (Equation (s,v,c)) -> 
    let et = negate_eq (Equation (s,v,c)) in 
    (match et with 
    (Equation (s,v,c)) -> (s,v,c))
  | _ -> invalid_arg "function extract_ineq_tuple called without an inequality"

(** Given an aggregated variable, returns the (function_name,var_name) tuple that defines it. *)
let extract_aggvar_tuple = function
    | AggVar (fn,vn) -> (fn,vn)
    | _ -> invalid_arg "function extract_aggvar_tuple called without an aggregated var"

(* Given an term, returns the equivalent negative form of it. *)
let negate_term = function
  | Rel rt -> Not rt
  | Not rt -> Rel rt
  | Equat e -> Noneq e
  | Noneq e -> Equat e
  | ConstTerm b -> ConstTerm (not b)

(** Return true if the provided argument is an aggregate variable. *)
let is_aggvar = function
  | AggVar _ -> true
  | _ -> false

(** Return true if the provided argument is an anonymous variable. *)
let is_anon = function
  | AnonVar -> true
  | _ -> false

(** Return true if the provided argument is an equality involving an
aggregate function. *)
let is_agg_equality = function
  | Equat (Equation ("=", e1, e2))
  | Noneq (Equation ("<>", e1, e2)) -> (List.length (List.filter is_aggvar ((get_vterm_varlist e1) @ (get_vterm_varlist e2)))) > 0
  | _ -> invalid_arg "function is_agg_equality called without an equality"

(** Return true if the provided argument is an inequality involving an
aggregate function. *)
let is_agg_inequality = function
  | Equat (Equation ("=", _e1, _e2))
  | Noneq (Equation ("<>", _e1, _e2)) -> invalid_arg "function is_agg_inequality called without an equality"
  | Equat (Equation (_ , e1, e2))
  | Noneq (Equation (_ , e1, e2)) -> (List.length (List.filter is_aggvar ((get_vterm_varlist e1) @ (get_vterm_varlist e2)))) > 0
  | _ -> invalid_arg "function is_agg_inequality called without an equality"

(****************************************************
 *
 *  String operations
 *
 ****************************************************)

(** support function for smart stringify of the AST - see to_string below *)
let string_of_const t = match t with 
    | Int x -> string_of_int x 
    | Real x -> if (x = floor x) then (string_of_float x)^"0" else (string_of_float x)
    | String x -> x  (* include single quote characters ' *)
    | Bool x -> string_of_bool x
    | Null -> "null"

(** convert the var type into a string *)
let string_of_var r = match r with
    | NamedVar x -> x
    | NumberedVar x -> "_" ^ string_of_int x
    | AnonVar    -> "_"
    | ConstVar x -> string_of_const x
    | AggVar (fn,vn) -> fn^"("^vn^")"

(** support function for smart stringify of the AST - see to_string below *)
let string_of_rterm r = match r with     
    | Pred (pn,vars) -> pn^"("^String.concat ", " (List.map string_of_var vars)^")"
    | Deltainsert (pn,vars) -> "+"^ pn^"("^String.concat ", " (List.map string_of_var vars)^")"
    | Deltadelete (pn,vars) -> "-"^pn^"("^String.concat ", " (List.map string_of_var vars)^")"

(** convert the vterm type into a string *)
let string_of_vterm ae =
    (* open and close parentthesis in current expresion by using the priority of the expression containing the current expression 
    opem_paren and close_paren take two priorities of previous operator and current operator
    higher priority means earlier evaluation
    "+" give the same priority of 0 to its two terms, and priority of 0 for the term contain this "+"
    "-" give the priority 0 to the first term and priority 1 to the second term (the first term should be evaluated first), and priority of 0 for the term contain this "-"
    similarly, "*" give its two terms the priority 2, "/" give its two terms the priority 2 and 3
    "-" which is minus sign has highest priority 
     *)
  let open_paren prec op_prec = 
    if prec > op_prec then  "(" else "" in 
  let close_paren prec op_prec = 
    if prec > op_prec then  ")" else "" in
  let rec str_of prec aexp = 
    match aexp with 
      Const c -> string_of_const c
    | Var v -> string_of_var v
    | BinaryOp("+", f,g) -> (open_paren prec 0)^ (str_of 0 f) ^ "+" ^ (str_of 0 g) ^ (close_paren prec 0)
    | BinaryOp("-", f,g) -> (open_paren prec 0) ^ (str_of 0 f) ^  "-" ^ (str_of 1 g) ^ (close_paren prec 0)
    | BinaryOp("*", f,g) -> (open_paren prec 2) ^ (str_of 2 f) ^  "*" ^ (str_of 2 g) ^ (close_paren prec 2)
    | BinaryOp("/", f,g) -> (open_paren prec 2)^ (str_of 2 f) ^ "/" ^ (str_of 3 g) ^ (close_paren prec 2)
    | UnaryOp ("-", e) ->  (open_paren prec 4)^ "-" ^ (str_of 5 e)^(close_paren prec 4)
    | BinaryOp("^", f,g) -> (open_paren prec 0)^ (str_of 0 f) ^ "^" ^ (str_of 0 g) ^ (close_paren prec 0)
    | BinaryOp(op, _,_) | UnaryOp (op, _) -> invalid_arg "Function str_of is called without a supported arithematic expression of "^op
    (* | BoolAnd (f,g) -> (open_paren prec 2) ^ (str_of 2 f) ^  "*" ^ (str_of 2 g) ^ (close_paren prec 2)
    | BoolOr (f,g) -> (open_paren prec 0)^ (str_of 0 f) ^ "+" ^ (str_of 0 g) ^ (close_paren prec 0)
    | BoolNot e ->  (open_paren prec 4)^ "-" ^ (str_of 5 e)^(close_paren prec 4) *)
  in str_of 0 ae

let string_of_eterm r = match r with     
    | Equation (op, e1,e2) -> (string_of_vterm e1) ^ " " ^ op ^ " " ^ (string_of_vterm e2)

(** support function for smart stringify of the AST - see to_string below *)
let string_of_term = function
  | Rel r -> string_of_rterm r
  | Equat e -> string_of_eterm e
  | Noneq e -> "not " ^ string_of_eterm e
  | Not rt -> "not " ^ string_of_rterm rt
  | ConstTerm b -> Bool.to_string b

let string_of_stype t = match t with 
    Sint -> "int"
    | Sreal -> "real"
    | Sstring -> "string"
    | Sbool -> "bool"

(** support function for smart stringify of the AST - see to_string below *)
(* let string_of_stt st = match st with
    | Rule (p, tel)        -> string_of_rterm p ^ " :- " ^ 
                             String.concat " , " (List.map string_of_term tel) ^ ".\n"
    | Query r            -> "?- " ^ string_of_rterm r ^ ".\n"
    | Source (name, lst)   -> "source " ^ name ^ "(" ^ String.concat ", " (List.map (fun (col,typ) -> "'"^col^"':"^ (string_of_stype typ)) lst) ^ ").\n" 
    | View (name, lst)    -> "view " ^ name ^ "(" ^ String.concat ", " (List.map (fun (col,typ) -> "'"^col^"':"^ (string_of_stype typ)) lst) ^ ").\n"
    | Constraint (p, tel)        -> string_of_rterm p ^ " :- " ^ 
                             String.concat " , " (List.map string_of_term tel) ^ ".\n"
    | Pk(relname, attrlst) -> "PK(" ^ relname ^ ", [" ^ String.concat ", " (List.map (fun att -> "'"^att^"'") attrlst) ^ "]).\n" 
    | Fact(rt) -> string_of_rterm rt ^ ".\n"
 *)

let string_of_source (name, lst) = "source " ^ name ^ "(" ^ String.concat ", " (List.map (fun (col,typ) -> "'"^col^"':"^ (string_of_stype typ)) lst) ^ ").\n" 

let string_of_view (name, lst) = "view " ^ name ^ "(" ^ String.concat ", " (List.map (fun (col,typ) -> "'"^col^"':"^ (string_of_stype typ)) lst) ^ ").\n"

let string_of_query r = "?- " ^ string_of_rterm r ^ ".\n"

let string_of_constraint (p, tel) = string_of_rterm p ^ " :- " ^ String.concat " , " (List.map string_of_term tel) ^ ".\n"

let string_of_pk (relname, attrlst) = "PK(" ^ relname ^ ", [" ^ String.concat ", " (List.map (fun att -> "'"^att^"'") attrlst) ^ "]).\n" 

let string_of_fact (rt) = string_of_rterm rt ^ ".\n"

let string_of_rule (p, tel) = string_of_rterm p ^ " :- " ^ String.concat " , " (List.map string_of_term tel) ^ ".\n"

(** smart stringify for AST *)
let to_string { rules; facts; query; sources; view; constraints; primary_keys; } = 
  (List.fold_right (^) (List.map string_of_source sources) "") ^
  (match view with
      | Some v -> string_of_view v
      | None -> "")  ^
  (match query with
      | Some v -> string_of_query v
      | None -> "")  ^
  (List.fold_right (^) (List.map string_of_pk primary_keys) "") ^
  (List.fold_right (^) (List.map string_of_constraint constraints) "") ^
  (List.fold_right (^) (List.map string_of_fact facts) "") ^
  (List.fold_right (^) (List.map string_of_rule rules) "")

let str_to_namedvar = function str -> NamedVar str

let stringlist_to_varlist strlst = List.map str_to_namedvar strlst 

(** convert datalog program to string  *)
let string_of_prog expr = to_string expr

let stype_of_const c = match c with 
    Int _ -> Sint 
    | Real _ -> Sreal 
    | String _ -> Sstring 
    | Bool _ -> Sbool
    | Null -> invalid_arg "Null does not have type"
