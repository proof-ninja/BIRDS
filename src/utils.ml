(**  Data structure definitions and operations for other modules.
*)
<<<<<<< HEAD
open Expr
=======
open List
open Expr2
>>>>>>> d96beeb (introduce files)
open Parsing
open Lexing
open Printf

(** Semantic error  *)
<<<<<<< HEAD
exception SemErr of string

(** Verification error  *)
exception ChkErr of string

(** Environment error  *)
exception EnvErr of string
=======
exception SemErr of string 

(** Verification fail  *)
exception ChkErr of string 
>>>>>>> d96beeb (introduce files)

(** Grammar error  *)
exception ParseErr of string

(** Lexing error  *)
exception LexErr of string

(** get a concrete message (file, possition of error, error message) for parsing or lexing error  *)
<<<<<<< HEAD
let spec_error msg start finish  =
  Printf.sprintf "File \"%s\", line %d, characters %d-%d: '%s'" start.pos_fname start.pos_lnum
=======
let spec_error msg start finish  = 
  Printf.sprintf "File \"%s\", line %d, characters %d-%d: '%s'" start.pos_fname start.pos_lnum 
>>>>>>> d96beeb (introduce files)
    (start.pos_cnum  -start.pos_bol) (finish.pos_cnum  - finish.pos_bol) msg

(** raise a parsing error *)
let spec_parse_error msg nterm =
  raise ( ParseErr (spec_error msg (rhs_start_pos nterm) (rhs_end_pos nterm)))

(** raise a lexing error *)
<<<<<<< HEAD
let spec_lex_error lexbuf =
=======
let spec_lex_error lexbuf = 
>>>>>>> d96beeb (introduce files)
  raise ( LexErr (spec_error (lexeme lexbuf) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)))

(***********************************************************
 *  Symtable
 *********************************************************)

(** This type defines a symtable: a hash table that stores
    the set of rules that define a program. The keys
    are rule-name & rule-arity tuples and the values are lists of
    rules' AST specifications.
*)
<<<<<<< HEAD
type symtkey = string * int (* string is predicate name, int is the arity of literal*)
=======
type symtkey = (string*int) (* string is predicate name, int is the arity of literal*)
>>>>>>> d96beeb (introduce files)
type symtable = (symtkey, (rterm * term list) list) Hashtbl.t (* each row of a symtable is all the rules which has the same literal in head*)

(* let hash_max_size = ref 500 *)

(** Prints a symtable
*)
let print_symtable (st:symtable) =
  let print_el s = Printf.printf "%s" (string_of_rule s) in
  let print_lst _ lst = List.iter print_el lst in
  Hashtbl.iter print_lst st

(** string of a symtable
*)
let string_of_symtable (st:symtable) =
  let p_el str s = str ^ (string_of_rule s) in
  let p_lst _ lst str = (List.fold_left p_el "" lst)^str in
  Hashtbl.fold p_lst st ""

(** Receives a rterm and generates its hash key for the
    symtable

    a rterm is identified by it predicate name and number of argument (arity)
*)
let symtkey_of_rterm rt : symtkey = (get_rterm_predname rt, get_arity rt)

(** Receives a rule and generates its hash key for the  symtable
*)
let symtkey_of_rule (h, b) : symtkey =  symtkey_of_rterm h

(** Inserts a rule in the symtable *)
let symt_insert (st:symtable) rule =
    let key = symtkey_of_rule rule in
<<<<<<< HEAD
    if Hashtbl.mem st key then
=======
    if Hashtbl.mem st key then  
>>>>>>> d96beeb (introduce files)
      Hashtbl.replace st key ((Hashtbl.find st key)@[rule]) (* add new rule into the list of rules of this key *)
    else
      Hashtbl.add st key [rule]

(** remove all rules of a key in the symtable if the symtable have this key *)
<<<<<<< HEAD
let symt_remove (st:symtable) key =
    if Hashtbl.mem st key then
=======
let symt_remove (st:symtable) key = 
    if Hashtbl.mem st key then  
>>>>>>> d96beeb (introduce files)
      Hashtbl.remove st key

(** Compares two keys for ordering *)
let key_comp ((k1_n,k1_a):symtkey) ((k2_n,k2_a):symtkey) =
  let comp = String.compare k1_n k2_n in
  if comp != 0 then comp
  else k1_a - k2_a

(** Given a list of keys, remove repetitions *)
let remove_repeated_keys k_lst =
  let no_rep key = function
    | [] -> [key]
    | (hd::tl) ->
      if (key_comp key hd) == 0 then (hd::tl)
      else (key::hd::tl) in
  let sorted = List.sort key_comp k_lst in
  List.fold_right no_rep sorted []

(** Given a key, returns the predicate name that belongs to the key *)
let get_symtkey_predname ((n,_):symtkey) = n

(** Given a key, returns the predicate arity that belongs to the key *)
let get_symtkey_arity ((_,a):symtkey) = a

let string_of_symtkey ((n,a):symtkey) =
  n^"/"^(string_of_int a)

(** return alias for a symtkey, just append the string with arity of the symtkey  *)
let alias_of_symtkey ((n,a):symtkey) =
  n^"_a"^(string_of_int a)

(** this type defines a table for storing sql translated from datalog for each predicate  *)
type sqltable = (symtkey, (rterm * term list) list) Hashtbl.t (* each row of a symtable is all the rules which has the same literal in head*)


(**Takes a program and extracts all rules and places them in a symtable*)
<<<<<<< HEAD
let extract_idb expr =
    let idb:symtable = Hashtbl.create (2 * (List.length expr.rules)) in
=======
let extract_idb expr = 
    let idb:symtable = Hashtbl.create (2 * (List.length expr.rules)) in           
>>>>>>> d96beeb (introduce files)
    List.iter (symt_insert idb) expr.rules;
    idb

(** conbine idb and a query in a AST  *)
<<<<<<< HEAD
let idb_query_to_ast (idb:symtable) (query:rterm) =
=======
let idb_query_to_ast (idb:symtable) (query:rterm) = 
>>>>>>> d96beeb (introduce files)
  let p_lst _ lst sttlst = lst @ sttlst in
  {get_empty_expr with rules = Hashtbl.fold p_lst idb []; query = Some query}

(** Takes a program and extracts all schema declarations and places them in a symtable*)
<<<<<<< HEAD
let extract_edb expr =
    let edb:symtable = Hashtbl.create (2 * (List.length expr.sources) + 2) in
=======
let extract_edb expr = 
    let edb:symtable = Hashtbl.create (2 * (List.length expr.sources) + 2) in          
>>>>>>> d96beeb (introduce files)
    List.iter (fun x -> symt_insert edb (get_schema_rterm x,[])) expr.sources;
    edb

(***********************************************************
 *  kset (SymtkeySet)
 *********************************************************)

(**This structure defines a set of symtable keys*)
<<<<<<< HEAD
module SymtkeySet = Set.Make(
=======
module SymtkeySet = Set.Make( 
>>>>>>> d96beeb (introduce files)
  struct
    let compare = key_comp
    type t = symtkey
  end
  )

type kset = SymtkeySet.t

(***********************************************************
 *  Colnamtab
 *********************************************************)

(** This type defines a colnamtab, which is a dictionnary that
    contains for each predicate (edb & idb) a list with the name of
    all of its columns in order. The information is stored in a
    hash table using as a keys the keys from symtables.*)
type colnamtab = (symtkey, (string list)) Hashtbl.t

(*Extracts from the edb and idb their column names and
 * stores them in a colnamtab, places them in order*)
let build_colnamtab (edb:symtable) (idb:symtable) =
  let hs:colnamtab = Hashtbl.create (2*(Hashtbl.length edb + Hashtbl.length idb)) in
  let e_cols key rules =
    let rule = List.hd rules in
    let varlist = List.map string_of_var (get_rterm_varlist (rule_head rule)) in
    Hashtbl.add hs key varlist in
  Hashtbl.iter e_cols edb;
  let i_cols key rules =
    let rec cols ind n =
<<<<<<< HEAD
      if ind<n then ("COL"^(string_of_int ind))::(cols (ind+1) n)
=======
      if ind<n then ("COL"^(string_of_int ind))::(cols (ind+1) n) 
>>>>>>> d96beeb (introduce files)
      else [] in
    if not (Hashtbl.mem hs key) then
      Hashtbl.add hs key (cols 0 (get_symtkey_arity key))
    else
      ()
  in
  Hashtbl.iter i_cols idb;
  hs

(** set for rterm  *)
module RtermSet = Set.Make(struct
    type t = rterm
    let compare rt1 rt2 = key_comp (symtkey_of_rterm rt1) (symtkey_of_rterm rt2)
  end)

type rtermset = RtermSet.t

(** Compares two variables for ordering *)
let var_comp var1 var2 = String.compare (string_of_var var1) (string_of_var var2)

(** set for variable  *)
module VarSet = Set.Make(struct
    type t = var
    let compare = var_comp
  end)

type varset = VarSet.t

(** Get the set of variables of a term list (maybe a rule body). *)
<<<<<<< HEAD
let rec get_termlst_varset terms =
    let lst = List.fold_right (@) (List.map get_term_varlist terms) [] in
=======
let rec get_termlst_varset terms = 
    let lst = List.fold_right (@) (List.map get_term_varlist terms) [] in 
>>>>>>> d96beeb (introduce files)
    VarSet.of_list lst


(** Get the list of variables of a term list (maybe a rule body). *)
<<<<<<< HEAD
let rec get_termlst_vars terms =
=======
let rec get_termlst_vars terms = 
>>>>>>> d96beeb (introduce files)
    let lst = List.fold_right (@) (List.map get_term_varlist terms) [] in lst


(** Get the list of variables of a rterm list (maybe a rule body). *)
<<<<<<< HEAD
let rec get_rtermlst_vars rterms =
=======
let rec get_rtermlst_vars rterms = 
>>>>>>> d96beeb (introduce files)
    let lst = List.fold_right (@) (List.map get_rterm_varlist rterms) [] in lst


(***********************************************************
 *  Vartab
 *********************************************************)

(** This type defines a 'vartable', it belongs to a rule and
    it is a dictionary with variable names as key, these variables
    are those that appear in the body/head of the rule.
    The value for each key is a list of variable-appearances:
    references to predicates in the rule's body where the variable is
    mentioned.
    A variable appearence is simply a string denoting
    a column of a relation in the way Table.column*)
type vartab = (string, string list) Hashtbl.t

(* Insert in a vartab the provided var_app, initializing a list in the hash if necessary. *)
let vt_insert (vt:vartab) vname va =
  if Hashtbl.mem vt vname then
    let ap_lst = Hashtbl.find vt vname in
    Hashtbl.replace vt vname (va::ap_lst)
  else
    Hashtbl.add vt vname [va]

(*Prints a vartab*)
let vt_print (vt:vartab) =
  let print_el vn alst =
    let ap_str = "["^(String.concat ", " alst)^"]" in
    Printf.printf "%s: %s\n" vn ap_str in
  Hashtbl.iter print_el vt


(** Builds a vartab out of a list of rterms and with the colnamtab.*)
let build_vartab (col_names:colnamtab) rterms =
  let vt:vartab = Hashtbl.create (2*(List.length (get_rtermlst_vars rterms ))) in
  let in_rt n rterm =
    let pname = get_rterm_predname rterm in
    let vlst = get_rterm_varlist rterm in
    let arity = get_arity rterm in
    let key = symtkey_of_rterm rterm in
    if not (Hashtbl.mem col_names key) then raise (SemErr ("not found edb or idb predicate "^string_of_symtkey key)) else
<<<<<<< HEAD
    let cols =
    try Hashtbl.find col_names key
=======
    let cols = 
    try Hashtbl.find col_names key  
>>>>>>> d96beeb (introduce files)
        with Not_found -> print_endline ("Not_found in col_names the key "^string_of_symtkey key); exit 0;
    in
    let in_v cn v =
      let comp_cn =
        pname^"_a"^(string_of_int arity)^
        "_"^(string_of_int n)^"."^cn
      in
      match v with
        NamedVar _ | NumberedVar _ ->
        vt_insert vt (string_of_var v) comp_cn
      | AggVar _ -> raise (SemErr (
          "Goal "^(string_of_symtkey key)^
          " contains an aggregate function as a variable, "^
          "which is only allowed in rule heads"
        ))
      | _ -> ()
    in
    List.iter2 in_v cols vlst;
    n+1
  in
  let _ = List.fold_left in_rt 0 rterms in
  vt

(** Build a vartab (use numbers to refer to the tables and columns, used in rosette code) out of a list of rterms and with the colnamtab. *)
let build_num_vartab (col_names:colnamtab) rterms =
  let vt:vartab = Hashtbl.create (2*(List.length (get_rtermlst_vars rterms ))) in
  let in_rt n rterm =
    let pname = get_rterm_predname rterm in
    let vlst = get_rterm_varlist rterm in
    let arity = get_arity rterm in
    let key = symtkey_of_rterm rterm in
    if not (Hashtbl.mem col_names key) then raise (SemErr ("not found edb or idb predicate "^string_of_symtkey key)) else
    let rec gen_nums ind n =
<<<<<<< HEAD
      if ind<n then ((string_of_int ind))::(gen_nums (ind+1) n)
=======
      if ind<n then ((string_of_int ind))::(gen_nums (ind+1) n) 
>>>>>>> d96beeb (introduce files)
      else [] in
    let cols = gen_nums 0 arity in
    let in_v cn v =
      let comp_cn =
        "(list-ref (list-ref tuplelst "^(string_of_int n)^") "^cn^")"
      in
      match v with
        NamedVar _ | NumberedVar _ ->
        vt_insert vt (string_of_var v) comp_cn
      | AggVar _ -> raise (SemErr (
          "Goal "^(string_of_symtkey key)^
          " contains an aggregate function as a variable, "^
          "which is only allowed in rule heads"
        ))
      | _ -> ()
    in
    List.iter2 in_v cols vlst;
    n+1
  in
  let _ = List.fold_left in_rt 0 rterms in
  vt

(***********************************************************
 *  Eqtab
 *********************************************************)

(** This type defines a eqtab, it belongs to a rule and it is
    a dictionary with variable names as
    keys and constants as values. They represent equalities that
<<<<<<< HEAD
    must be satisfied by the variables*)
=======
    must be satisfied by the variables*) 
>>>>>>> d96beeb (introduce files)
type eqtab = (vterm,vterm) Hashtbl.t

(** Given a list of equality ASTs, returns an eqtab with
    the equality relations as var = value.
    PRECONDITION: There should not be aggregate equalities
    in the provided list.*)
let build_eqtab eqs =
  let tuples = List.map extract_eq_tuple eqs in
  let hs:eqtab = Hashtbl.create (2*(List.length eqs)) in
<<<<<<< HEAD
  let add_rel (e1,e2) = if ((List.length ((get_vterm_varlist e1) @ (get_vterm_varlist e2))) > 0)
=======
  let add_rel (e1,e2) = if ((List.length ((get_vterm_varlist e1) @ (get_vterm_varlist e2))) > 0) 
>>>>>>> d96beeb (introduce files)
    then Hashtbl.add hs e1 e2
    else invalid_arg "Trying to build_eqtab with equalities containing no varialbe " in
  List.iter add_rel tuples;
  hs

(** Given a var name, returns the value and removes it from the eqtab. *)
<<<<<<< HEAD
let eqt_extract (eqt : eqtab) (e1 : vterm) : vterm =
=======
let eqt_extract eqt e1 =
>>>>>>> d96beeb (introduce files)
  let lst = Hashtbl.find_all eqt e1 in
  if ((List.length lst) <> 1) then raise (SemErr ("Ambiguity of the assigments of variable "^(string_of_vterm e1)));
  let c = List.hd lst in
  (* Hashtbl.remove eqt e1;  *)
  c

<<<<<<< HEAD
(** Get the query expression,
    check if there is one query, or more than one
    @param get_query takes input assumed to be query
    @return true if there is query, otherwise error statements *)
let get_query expr =
=======
(** Get the query expression, 
    check if there is one query, or more than one 
    @param get_query takes input assumed to be query 
    @return true if there is query, otherwise error statements *)
let get_query expr = 
>>>>>>> d96beeb (introduce files)
  match expr.query with
    | Some v -> v
    | None -> raise (SemErr "The program has no query")

(** Return true if there is a query, otherwise error statements. *)
<<<<<<< HEAD
let has_query expr =
=======
let has_query expr = 
>>>>>>> d96beeb (introduce files)
  match expr.query with
    | Some v -> true
    | None -> false

(** Takes a list of terms and splits them into positive rterms, negative terms, equalities, and inequalities. *)
let split_terms terms =
  let rec split t (pos,neg,eq,inq) = match t with
    | Rel rt -> (rt::pos,neg,eq,inq)
    | Not rt -> (pos,rt::neg,eq,inq)
<<<<<<< HEAD
    | Equat (Equation ("=",_,_)) -> (pos,neg,t::eq,inq)
    | Noneq (Equation ("<>",ae1,ae2)) -> (pos,neg,Equat (Equation ("=",ae1,ae2))::eq,inq)
=======
    | Equat (Equation ("=",_,_)) -> (pos,neg,t::eq,inq) 
    | Noneq (Equation ("<>",ae1,ae2)) -> (pos,neg,Equat (Equation ("=",ae1,ae2))::eq,inq) 
>>>>>>> d96beeb (introduce files)
    | _ -> (pos,neg,eq,t::inq) in
  List.fold_right split terms ([],[],[],[])

(** get the statement of view schema  *)
let get_view expr =
  match expr.view with
    | Some v -> v
    | None -> raise (SemErr "The program has no view")

let get_view_rterm e = get_schema_rterm (get_view e)

(** Generate a column name list [col0, col1,....]  *)
let rec gen_cols ind n =
<<<<<<< HEAD
  if ind<n then ( "COL"^(string_of_int ind))::(gen_cols (ind+1) n)
=======
  if ind<n then ( "COL"^(string_of_int ind))::(gen_cols (ind+1) n) 
>>>>>>> d96beeb (introduce files)
  else []

(** Generate a var list [COL0, COL1,....]  *)
let rec gen_vars ind n =
<<<<<<< HEAD
  if ind<n then (NamedVar ("COL"^(string_of_int ind)))::(gen_vars (ind+1) n)
=======
  if ind<n then (NamedVar ("COL"^(string_of_int ind)))::(gen_vars (ind+1) n) 
>>>>>>> d96beeb (introduce files)
  else []

(** Given a rterm, return this rterm as a literal of all variables *)
let variablize_rterm(rt:rterm) = match rt with
  | Pred (x, vl) -> Pred (x, (gen_vars 0 (List.length vl)))
  | Deltainsert (x, vl) -> Deltainsert (x, (gen_vars 0 (List.length vl)))
  | Deltadelete (x, vl) -> Deltadelete (x, (gen_vars 0 (List.length vl)))

(** Given a predicate name, return a new temporary name*)
<<<<<<< HEAD
let get_temp_name (name:string) = "__tmp_"^name

(** Given a rterm, return a new temporary rterm*)
let get_temp_rterm (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred ("__tmp_"^x, vl)
  | Deltainsert (x, vl) -> Deltainsert ("__tmp_"^x, vl)
  | Deltadelete (x, vl) -> Deltadelete ("__tmp_"^x, vl)

(** Given a rterm, return a new temporary delta of insertion of rterm*)
let get_temp_delta_insertion_rterm (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred ("__tmp_delta_ins_"^x, vl)
=======
let get_temp_name (name:string) = "__temp__"^name

(** Given a rterm, return a new temporary rterm*)
let get_temp_rterm (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred ("__temp__"^x, vl)
  | Deltainsert (x, vl) -> Deltainsert ("__temp__"^x, vl)
  | Deltadelete (x, vl) -> Deltadelete ("__temp__"^x, vl)

(** Given a rterm, return a new temporary delta of insertion of rterm*)
let get_temp_delta_insertion_rterm (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred ("__temp__delta_ins_"^x, vl)
>>>>>>> d96beeb (introduce files)
  | _ -> invalid_arg "function get_temp_delta_insertion_rterm called with not a Pred"

(** Given a rterm, returns a new temporary delta of deletion of rterm*)
let get_temp_delta_deletion_rterm (rt:rterm) = match rt with
<<<<<<< HEAD
  | Pred (x, vl) -> Pred ("__tmp_delta_del_"^x, vl)
=======
  | Pred (x, vl) -> Pred ("__temp__delta_del_"^x, vl)
>>>>>>> d96beeb (introduce files)
  | _ -> invalid_arg "function get_temp_delta_deletion_rterm called with not a Pred"

(** Given a rterm, returns a materialized of rterm*)
let get_materializied_rterm (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred ("__dummy__materialized_"^x, vl)
  | _ -> invalid_arg "function get_materializied_rterm called with not a Pred"

<<<<<<< HEAD
(** Given a rterm, rename it by adding to its name a prefix*)
=======
(** Given a rterm, rename it by adding its name a prefix*)
>>>>>>> d96beeb (introduce files)
let rename_rterm (prefix:string) (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred (prefix^x, vl)
  | Deltainsert (x, vl) -> Deltainsert (prefix^x, vl)
  | Deltadelete (x, vl) -> Deltadelete (prefix^x, vl)

<<<<<<< HEAD
(** Given a rterm, rename it by adding to its name a postfix*)
=======
(** Given a rterm, rename it by adding its name a prefix*)
>>>>>>> d96beeb (introduce files)
let rename2_rterm (postfix:string) (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred (x^postfix, vl)
  | Deltainsert (x, vl) -> Deltainsert (x^postfix, vl)
  | Deltadelete (x, vl) -> Deltadelete (x^postfix, vl)

(** Given a rterm, change its var list*)
<<<<<<< HEAD
let change_vars rt vs = match rt with
=======
let change_vars rt vs = match rt with 
>>>>>>> d96beeb (introduce files)
            Pred (n,_) -> Pred (n,vs)
            | Deltadelete (n,_) -> Deltadelete (n,vs)
            | Deltainsert (n,_) -> Deltainsert (n,vs)

<<<<<<< HEAD
let rename_term prefix t = match t with
=======
let rename_term prefix t = match t with 
>>>>>>> d96beeb (introduce files)
  | Rel r             -> Rel (rename_rterm prefix r)
  | Not r            -> Not (rename_rterm prefix r)
  | _ -> t

let rename_rule prefix (p, body) = (rename_rterm prefix p, List.map (rename_term prefix) body)

let rename_fact prefix rt = rename_rterm prefix rt

<<<<<<< HEAD
let rename_rules prefix rules =
=======
let rename_rules prefix rules = 
>>>>>>> d96beeb (introduce files)
  List.map (rename_rule prefix) rules

let str_contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

(** Cut a substring starting with a word *)
let cut_str_by_word s1 word =
  let re = Str.regexp_string word in
  let start = try(Str.search_forward re s1 0)
<<<<<<< HEAD
    with Not_found -> String.length s1 in
  String.sub s1 start ((String.length s1)-start)

(** print a delta predicate list  *)
let print_deltas dlst =
  let print_el s = Printf.printf "%s, " (string_of_rterm s) in
  List.iter print_el dlst

(** get the delta predicates,
    check if there is no update
=======
    with Not_found -> String.length s1 in 
  String.sub s1 start ((String.length s1)-start)

(** print a delta predicate list  *)
let print_deltas dlst = 
  let print_el s = Printf.printf "%s, " (string_of_rterm s) in
  List.iter print_el dlst

(** get the delta predicates, 
    check if there is no update 
>>>>>>> d96beeb (introduce files)
    @return true if there are more than one updates, otherwise error statements *)
let get_delta_rterms expr =
    let add_delta (rtset:rtermset) = function
      | (head, lst) -> (
<<<<<<< HEAD
        match head with
          Pred _ -> rtset
          | Deltainsert _ -> RtermSet.add (variablize_rterm head) rtset
=======
        match head with 
          Pred _ -> rtset 
          | Deltainsert _ -> RtermSet.add (variablize_rterm head) rtset 
>>>>>>> d96beeb (introduce files)
          | Deltadelete _ -> RtermSet.add (variablize_rterm head) rtset)
    in
    let delta_lst: rterm list = RtermSet.elements (List.fold_left add_delta RtermSet.empty expr.rules) in
    (* print_endline "____delta____";
       print_deltas delta_lst; *)
<<<<<<< HEAD
    match delta_lst with
=======
    match delta_lst with 
>>>>>>> d96beeb (introduce files)
    | []     -> raise (SemErr "The program has no update")
    | _::tail    -> delta_lst

(** get all source predicates *)
let get_source_rterms expr =
    let add_source (rtset:rtermset) src= RtermSet.add (get_schema_rterm src) rtset
    in
    let source_lst: rterm list = RtermSet.elements (List.fold_left add_source RtermSet.empty expr.sources) in
    source_lst

(** check if a variable is free or not, a variable is free if it is not in positive predicates*)
<<<<<<< HEAD
let is_free_var (vt:vartab) (vexp:vterm) = match vexp with
  | Var variable -> if Hashtbl.mem vt (string_of_var variable) then false else true
  | _ -> false

let is_delta_pair rt1 rt2 = match (rt1, rt2) with
    (Deltainsert (pred1, varlst1) , Deltadelete (pred2, varlst2)) -> if (String.compare pred1 pred2 = 0) && ((List.length varlst1) = (List.length varlst2)) then true else false
    | _ -> false

let is_delta_or_empty rt = match rt with
  Pred (n,vs) -> (String.compare n (get_rterm_predname (get_empty_pred))) == 0
  | Deltadelete _ -> true
=======
let is_free_var (vt:vartab) (vexp:vterm) = match vexp with 
  | Var variable -> if Hashtbl.mem vt (string_of_var variable) then false else true
  | _ -> false

let is_delta_pair rt1 rt2 = match (rt1, rt2) with 
    (Deltainsert (pred1, varlst1) , Deltadelete (pred2, varlst2)) -> if (String.compare pred1 pred2 = 0) && ((List.length varlst1) = (List.length varlst2)) then true else false 
    | _ -> false

let is_delta_or_empty rt = match rt with 
  Pred (n,vs) -> (String.compare n (get_rterm_predname (get_empty_pred))) == 0
  | Deltadelete _ -> true 
>>>>>>> d96beeb (introduce files)
  | Deltainsert _ -> true

let rules_of_symt symt = Hashtbl.fold (fun k rules lst -> rules@lst) symt []

<<<<<<< HEAD
let read_file filename =
=======
let read_file filename = 
>>>>>>> d96beeb (introduce files)
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
<<<<<<< HEAD
    List.rev !lines

let exe_command command =
  let tmp_file = Filename.temp_file "" ".txt" in
  let status = Sys.command @@ command ^" > " ^ tmp_file ^" 2>> " ^ tmp_file in
  let message = String.concat "\n" @@ read_file tmp_file in
  status, message

let check_command_version command =
  let status, message = exe_command @@ command ^ " --version" in
  if not (status = 0) then raise (EnvErr (command ^ " is required but not installed yet! Be sure "^command ^ " can be called in the terminal."))
  else message

let verify_fo_lean debug timeout sentence =
=======
    List.rev !lines 

let exe_command command = 
  let tmp_file = Filename.temp_file "" ".txt" in
  let status = Sys.command @@ command ^" > " ^ tmp_file ^" 2>> " ^ tmp_file in
  let message = String.concat "\n" @@ read_file tmp_file in 
  status, message

let verify_fo_lean debug timeout sentence = 
>>>>>>> d96beeb (introduce files)
  if  debug then (
    print_endline @@"==> verifying by Lean";
    print_endline "--------------";
    print_endline "Lean script:\n";
    print_endline sentence;
    print_endline "--------------";
    flush stdout;
  ) else ();
  let tmp_file = Filename.temp_file "" ".lean" in
<<<<<<< HEAD
  let ol =  open_out tmp_file in
  fprintf ol "%s\n" sentence;
  close_out ol;
  ignore (check_command_version "lean");
  (* check_lean_path *)
  let tmp_chklib_file = Filename.temp_file "" ".lean" in
  let chklib =  open_out tmp_chklib_file in
  fprintf chklib "%s\n" "import bx";
  close_out chklib;
  let leanstatus, leanmessage = exe_command @@ "lean "^tmp_chklib_file in
  if not (leanstatus = 0) then
    raise (EnvErr ("Lean paths to BIRDS's verification folder are not configured correctly! Please change the Lean path configuration in ~/.lean/leanpkg.path and check by 'lean --path'. More details at https://github.com/dangtv/BIRDS"))
  else ();
  ignore (check_command_version "z3");
  let status, message = exe_command @@ "timeout "^ (string_of_int timeout) ^" lean "^tmp_file in
  if (debug && (status = 0)) then print_endline @@">>> verified by lean: correct";
  status, message

let check_ros_prog debug timeout sentence =
=======
  let ol =  open_out tmp_file in  
  fprintf ol "%s\n" sentence;
  close_out ol;
  let status, message = exe_command @@ "timeout "^ (string_of_int timeout) ^" lean "^tmp_file in 
  if (debug && (status = 0)) then print_endline @@">>> verified by lean: correct";
  status, message

let check_ros_prog debug timeout sentence = 
>>>>>>> d96beeb (introduce files)
  if  debug then (
    print_endline @@"==> generating a counterexample by Rosette";
    print_endline "--------------";
    print_endline "Racket code:\n";
    print_endline sentence;
    print_endline "--------------";
    flush stdout;
  ) else ();
  let tmp_file = Filename.temp_file "" ".rkt" in
<<<<<<< HEAD
  let ol =  open_out tmp_file in
  fprintf ol "%s\n" sentence;
  close_out ol;
  ignore (check_command_version "racket");
  (* check racket lib *)
  let tmp_chklib_file = Filename.temp_file "" ".rkt" in
  let chklib =  open_out tmp_chklib_file in
  fprintf chklib "%s\n" "#lang rosette";
  close_out chklib;
  let racketstatus, racketmessage = exe_command @@ "racket "^tmp_chklib_file in
  if not (racketstatus = 0) then
    raise (EnvErr ("Package rosette is requried but not installed correctly. Please install by 'raco pkg install rosette'. More details at https://github.com/dangtv/BIRDS"))
  else ();
=======
  let ol =  open_out tmp_file in  
  fprintf ol "%s\n" sentence;
  close_out ol;
>>>>>>> d96beeb (introduce files)
  let status, message = exe_command @@ "timeout "^ (string_of_int timeout) ^" racket "^tmp_file in
  if (debug && (status = 0)) then print_endline @@">>> Checked by Rosette";
  status, message

let constraint2rule expr =
<<<<<<< HEAD
    let trans_pk (relname, attrlst) lst=
        (* generate datalog rules for a primary key *)
        let schema_stt =
          try (List.find (fun x -> relname = (get_schema_name x)) (get_schema_stts expr) )
          with Not_found -> raise (SemErr ("Not found the relation "^relname^ " in the primary key constraint \n"^ string_of_pk (relname, attrlst)))
        in
        let allattrlst = get_schema_attrs schema_stt in
        let allattrlst2 = List.map (fun x -> if (List.mem x attrlst) then x else x^"2") allattrlst in
        let nonkeyattrlst = List.filter (fun x -> not (List.mem x attrlst)) allattrlst in
        (List.map (fun x -> (get_empty_pred, [Rel (Pred(relname, List.map (fun t -> NamedVar t) allattrlst)); Rel (Pred(relname, List.map (fun t -> NamedVar t) allattrlst2)); Equat (Equation("<>", Var (NamedVar x), Var (NamedVar (x^"2"))))] )) nonkeyattrlst )@lst in
    { expr with rules = (List.fold_right trans_pk expr.primary_keys expr.constraints)@expr.rules}

(*
=======
    let trans_pk (relname, attrlst) lst= 
        (* generate datalog rules for a primary key *)
        let schema_stt = 
          try (List.find (fun x -> relname = (get_schema_name x)) (get_schema_stts expr) )
          with Not_found -> raise (SemErr ("Not found the relation "^relname^ " in the primary key constraint \n"^ string_of_pk (relname, attrlst)))
        in
        let allattrlst = get_schema_attrs schema_stt in 
        let allattrlst2 = List.map (fun x -> if (List.mem x attrlst) then x else x^"2") allattrlst in
        let nonkeyattrlst = List.filter (fun x -> not (List.mem x attrlst)) allattrlst in 
        (List.map (fun x -> (get_empty_pred, [Rel (Pred(relname, List.map (fun t -> NamedVar t) allattrlst)); Rel (Pred(relname, List.map (fun t -> NamedVar t) allattrlst2)); Equat (Equation("<>", Var (NamedVar x), Var (NamedVar (x^"2"))))] )) nonkeyattrlst )@lst in 
    { expr with rules = (List.fold_right trans_pk expr.primary_keys expr.constraints)@expr.rules}     

(* 
>>>>>>> d96beeb (introduce files)
Color	Code
Black	0;30
Blue	0;34
Green	0;32
Cyan	0;36
Red	0;31
Purple	0;35
Brown	0;33
Blue	0;34
Green	0;32
Cyan	0;36
Red	0;31
Purple	0;35
Brown	0;33 *)
<<<<<<< HEAD
let colored_string color str = match color with
=======
let colored_string color str = match color with 
>>>>>>> d96beeb (introduce files)
    "red" -> "\027[31m"^str^"\027[0m"
    | "black"	 -> "\027[30m"^str^"\027[0m"
    | "blue"	 -> "\027[34m"^str^"\027[0m"
    | "green"	 -> "\027[32m"^str^"\027[0m"
    | "cyan"	 -> "\027[36m"^str^"\027[0m"
    | "purple" -> "\027[35m"^str^"\027[0m"
    | "brown"	 -> "\027[33m"^str^"\027[0m"
    | _ -> str
<<<<<<< HEAD
=======


(*
From here several utilities used for operation-based view update
*)

let fresh, init =
  let c = ref 0 in
  (fun() -> incr c; !c),
  (fun() -> c := 0)

let extract_get_puts ((vn,vs):view) (e:expr) : (rule list*rule list) = 
  let rec ex_gp (vn:string) (rs:rule list) ((gs,ps):(rule list*rule list)) =
  begin match rs with
  | [] -> (gs,ps)
  | ((Pred(vname,_),_) as g)::rs -> 
     if vn=vname then 
       ex_gp vn rs (gs@[g],ps)
     else
       ex_gp vn rs (gs,ps@[g])
  | r::rs  -> ex_gp vn rs (gs,ps@[r])
  end 
  in ex_gp vn e.rules ([],[])


let rec simplification (rs:rule list) :rule list = 
  let (rs':rule list)= map anonvarlize rs in 
  let (rs0:rule list)= map sort_body rs' in
  let (rs1:rule list)= rm_contradict rs0 in
  let (rs2:rule list)= rm_contradict_equation rs1 in 
  let (rs3:rule list)= map rm_dup_t rs2 in
  let (rs4:rule list)= map rm_redundant_t rs3 in
  let (rs5:rule list)= rm_dup_r rs4 in
  (* let (rs6:rule list)= rm_redundant_r rs5 in *)
  let (rs7:rule list)= map rm_redundant_equation rs5 in
  (* let (rs8:rule list)= rm_redundant_r rs7 in *)
  (* let (rs9:rule list)= map rm_anovar_equation rs7 in *)
  let (rs10:rule list)= rm_redundant_r rs7 in
  let (rs11:rule list)= rm_dup_r rs10 in
  let (rs12:rule list)= map sort_body rs11 in
  rs12

and anonvarlize ((h,ts):rule) : rule =
  (h, map (set_anonvar (get_rterm_varlist h) (vars_in_body (h,ts))) ts)
and vars_in_body ((h,ts):rule) : var list =
  fold_right (@) (map get_term_varlist ts) [] 
and vars_in_head ((h,ts):rule) : var list =
  get_rterm_varlist h
and set_anonvar (hvs:var list) (bvs:var list) (t:term) =
  match t with
  | Rel(Pred(s,vs)) -> Rel(Pred(s, map (set_ano hvs bvs) vs)) 
  | Rel(Deltainsert(s,vs)) -> Rel(Deltainsert(s, map (set_ano hvs bvs) vs)) 
  | Rel(Deltadelete(s,vs)) -> Rel(Deltadelete(s, map (set_ano hvs bvs) vs)) 
  | Not(Pred(s,vs)) -> Not(Pred(s, map (set_ano hvs bvs) vs)) 
  | Not(Deltainsert(s,vs)) -> Not(Deltainsert(s, map (set_ano hvs bvs) vs)) 
  | Not(Deltadelete(s,vs)) -> Not(Deltadelete(s, map (set_ano hvs bvs) vs)) 
  (* | Equat (Equation (op, e1, e2)) -> (get_vterm_varlist e1) @ (get_vterm_varlist e2)
   * | Noneq (Equation (op,e1, e2))  -> (get_vterm_varlist e1) @ (get_vterm_varlist e2)
   * | Not r            -> get_rterm_varlist r *)
  | _ -> t
and set_ano hvs bvs v : var  =
  if (is_var_in_once (Var(v)) bvs)&& not(is_var_in_head (Var(v)) hvs) then
    AnonVar
  else
    v

and rm_anovar_equation ((h,ts):rule) : rule =
  let (newts:term list) = 
  let rec rmAno (ts:term list) : term list =
    match ts with
    | [] -> []
    | (Equat(Equation(_,Var(AnonVar),_)))::ts' -> 
       rmAno ts'
    | (Noneq(Equation(_,Var(AnonVar),_)))::ts' -> 
       rmAno ts'
    | t::ts' -> t::(rmAno ts')
  in rmAno ts in
  (h,newts)
and rm_dup_t ((h,ts):rule) = 
  (h, (rm_dup_body ts))
and rm_dup_body (ts:term list) : term list = 
    match ts with
    | [] -> []
    | t::ts1 -> if contain_t t ts1 then
                  rm_dup_body ts1
                else
                  [t]@rm_dup_body ts1

and contain_t (t:term) (ts:term list) : bool = 
  fold_right (||) (map (fun t1 -> eq_t t1 t) ts) false
and eq_t (t1:term) (t2:term) : bool =
  match (t1,t2) with
  | (Rel(rt1),Rel(rt2)) | (Not(rt1),Not(rt2)) ->
     eq_rt rt1 rt2
  | (Equat(et1), Equat(et2)) | (Noneq(et1), Noneq(et2)) ->
     eq_et et1 et2
  | _ -> false
and eq_rt (rt1:rterm) (rt2:rterm) : bool =
  match (rt1,rt2) with
  | (Pred(s1,vs1), Pred(s2,vs2)) 
    | (Deltainsert(s1,vs1), Deltainsert(s2,vs2))
    | (Deltadelete(s1,vs1), Deltadelete(s2,vs2)) ->
     (s1=s2) && ((length vs1)=(length vs2)) &&  fold_right (&&) (map (fun (v1,v2) -> eq_v v1 v2) (combine vs1 vs2)) true
  | _ -> false
and eq_et (et1:eterm) (et2:eterm) : bool =
  match (et1,et2) with
  | (Equation(s1,vt11,vt12), Equation(s2,vt21,vt22)) ->
     (s1=s2) && (eq_vt vt11 vt21) && (eq_vt vt12 vt22)
and eq_vt (vt1:vterm) (vt2:vterm) : bool =
  match (vt1,vt2) with
  | (Const(c1), Const(c2)) ->
     eq_c c1 c2
  | (Var(v1), Var(v2)) ->
     eq_v v1 v2
  | (BinaryOp(s1,vt11,vt12), BinaryOp(s2,vt21,vt22)) ->
     (s1=s2) && (eq_vt vt11 vt21) && (eq_vt vt12 vt22)
  | (UnaryOp(s1,vt1), UnaryOp(s2,vt2)) ->
     (s1=s2) && (eq_vt vt1 vt2)
  | _ -> false
and eq_c (c1:const) (c2:const) : bool =
  match (c1,c2) with
  | (Int(i1),Int(i2)) -> i1=i2
  | (Real(f1),Real(f2)) -> f1=f2
  | (String(s1),String(s2)) -> s1=s2
  | (Bool(b1),Bool(b2)) -> b1=b2
  | (Null,Null) -> true
  | _ -> false
and eq_v (v1:var) (v2:var) : bool =
  match (v1,v2) with
  | (NamedVar(s1),NamedVar(s2)) -> s1=s2
  | (NumberedVar(n1),NumberedVar(n2)) -> n1=n2
  | (ConstVar(c1),ConstVar(c2)) -> eq_c c1 c2
  | (AnonVar,AnonVar) -> true
  | (AggVar(s11,s12),AggVar(s21,s22)) -> (s11=s21) && (s12=s22)
  | _ -> false
and sort_body ((h,ts):rule) : rule = (h, (sort cmp_t ts))
  
and rm_redundant_r (rs:rule list) : rule list = 
  let bs=map (fun r -> is_contain r rs) rs in
  let rbs=combine rs bs in
  let rec rgen (rbs:(rule*bool)list) : rule list =
    match rbs with
    | [] -> []
    | (r,b)::rbs' -> if b then 
                       rgen rbs'
                     else
                       [r]@rgen rbs'
  in rgen rbs
and is_contain (r:rule) (rs:rule list) : bool =
  fold_right (||) (map (fun r1 -> is_con r r1) rs) false
and is_con ((h1,ts1):rule) ((h2,ts2):rule) : bool = 
  (eq_rt h1 h2) && ((length ts1)>(length ts2)) &&
    contain_ts ts1 ts2
and contain_ts (ts1:term list) (ts2:term list) : bool =
    let ts1''=take ts1 (length ts2) in
    eq_ts ts1'' ts2
and take ls n =
  if n<=0 || ls=[] then []
  else hd ls :: take (tl ls) (n-1)

and rm_dup_r (rs:rule list) : rule list =
  match rs with
  | [] -> []
  | r::rs' -> if contain r rs'  then
                rm_dup_r rs'
              else
                [r]@rm_dup_r rs'
and contain (r:rule) (rs:rule list) : bool =
  fold_right (||) (map (fun r1 -> eq_r r1 r) rs) false
and eq_r ((h1,ts1):rule) ((h2,ts2):rule) : bool = 
  (eq_rt h1 h2) && eq_ts ts1 ts2
and eq_ts (ts1:term list) (ts2:term list) : bool =
  if length ts1 = length ts2 then 
    (* fold_right (&&) (map (fun (t1, t2) -> t1=t2) (combine (sort cmp_t ts1) (sort cmp_t ts2))) true *)
    fold_right (&&) (map (fun (t1, t2) -> eq_t t1 t2) (combine ts1 ts2)) true
  else
    false
and cmp_t (t1:term) (t2:term) : int =
  match (t1,t2) with
  | (Not(Pred(_,_)), Not(Deltainsert(_,_))) ->  -2  (* not R, not +R *)
  | (Not(Deltainsert(_,_)), Not(Pred(_,_))) ->  2   (* not R, not +R *)
  | (Not(Pred(_,_)), Not(Deltadelete(_,_))) ->  -2  (* not R, not -R *)
  | (Not(Deltadelete(_,_)), Not(Pred(_,_))) ->  2   (* not R, not -R *)
  | (Not(Deltadelete(_,_)), Not(Deltainsert(_,_))) ->  1 (* not +R, not -R *)
  | (Rel(rt1), Rel(rt2)) | (Not(rt1), Not(rt2)) ->
     comp_rt rt1 rt2
  | (Equat(Equation(s1,_,_)), Equat(Equation(s2,_,_))) | (Noneq(Equation(s1,_,_)), Noneq(Equation(s2,_,_)))  ->
     String.compare s1 s2
  | (Rel(_),Not(_)) -> -3       (* R, not +-R *)
  | (Rel(_),Equat(_)) -> -1     (* R, v=c *)
  | (Rel(_),Noneq(_)) -> -1     (* R, v!=c *)
  | (Not(_),Rel(_)) -> 2        (* R, not R *)
  | (Not(_),Equat(_)) -> 2      (* v=c, not R *)
  | (Not(_),Noneq(_)) -> 2      
  | (Equat(_), Rel(_)) -> 3
  | (Equat(_), Not(_)) -> -1
  | (Equat(_), Noneq(_)) -> -1
  | (Noneq(_), Not(_)) -> -1
  | (Noneq(_), Rel(_)) -> 1
  | (Noneq(_), Equat(_)) -> 1

and comp_rt (rt1:rterm) (rt2:rterm) : int =
  match (rt1,rt2) with
  | (Pred(s1,_), Pred(s2,_)) | (Deltainsert(s1,_),Deltainsert(s2,_)) | (Deltadelete(s1,_),Deltadelete(s2,_)) ->
     String.compare s1 s2
  | (Pred(_,_), _) -> -2
  | (Deltainsert(_,_),_) -> -1
  | (Deltadelete(_,_),_) -> 1
and rm_contradict (rs:rule list) : rule list =
    begin match rs with
    | [] -> []
    | ((h,ts) as r)::rs' ->
       if is_contradict ts then
         rm_contradict rs'
       else
         [r]@(rm_contradict rs')
    end 
and rm_redundant_t ((h,ts):rule) : rule = 
  (h,(rm_red ts))
and rm_red (ts:term list) : term list =
  let (bss:bool list list) = map (fun t1->(map (fun t2-> is_redundant t1 t2) ts)) ts in
  let (bs:bool list) = map (fun v-> (fold_right (||) v false)) bss in
  let rec r_red (bs:bool list) (ts:term list) : term list =
    match (bs,ts) with
    | ([],[]) -> []
    | ((b::bs'),(t::ts')) -> 
       if b then
         r_red bs' ts'
       else
         [t]@(r_red bs' ts')
    | (_,_) -> failwith "in r_red"
  in r_red bs ts
and is_redundant (t1:term) (t2:term) : bool =
  if eq_t t1 t2 then
    false
  else 
    begin match (t1, t2) with
    | (Rel(Pred(s1,vs1)),Rel(Pred(s2,vs2))) ->
       if s1=s2 then 
         is_red vs1 vs2
       else 
         false
    | (Not(Pred(s1,vs1)),Not(Pred(s2,vs2))) ->
       if s1=s2 then
         is_red vs1 vs2
       else
         false
    |(t1',t2') -> false
    end
and is_red (vs1:var list) (vs2:var list) : bool =
  match (vs1,vs2) with
  | ([],[]) -> false
  | (v1::vs1', v2::vs2') -> 
     if eq_v v1 v2 then
       is_red vs1' vs2' 
     else
       v1=AnonVar
  | (_,_) -> false

and is_contradict (ts:term list) : bool = 
  fold_right (||) (map (fun t-> cnt t ts) ts) false
and cnt (t:term) (ts:term list) : bool =
  begin match t with
  | Rel(Pred(s,vs)) ->
      let (c:term) = Not(Pred(s,vs)) in
      cn c ts 
  | Not(Pred(s,vs)) ->
     let (c:term) = Rel(Pred(s,vs)) in
     cn c ts
  | Rel(Deltainsert(s,vs)) ->
     let (c:term) = Not(Deltainsert(s,vs)) in
     cn c ts
  | Rel(Deltadelete(s,vs)) ->
     let (c:term) = Not(Deltadelete(s,vs)) in
     cn c ts
  | Equat(et) -> 
     let (c:term) = Noneq(et) in
     cn c ts
  | Noneq(et) ->
     let (c:term) = Equat(et) in
     cn c ts
  | _ -> false
  end 
and cn (c:term) (ts:term list) : bool =
  fold_right (||) (map (eq_t c) ts) false 

and rm_contradict_equation (rs:rule list) : rule list =
  match rs with
  | [] -> []
  | ((h,ts) as r)::rs' ->
     if is_contradict_equation ts then
       rm_contradict_equation rs'
     else
       r::(rm_contradict_equation rs')
and is_contradict_equation (ts:term list) : bool =
  fold_right (||) ((map (coneq ts)) ts) false
and coneq (ts:term list) (t:term) : bool =
  fold_right (||) (map (contradict_equation t) ts) false
and contradict_equation (t1:term) (t2:term) : bool =
  match (t1, t2) with
  | (Equat(Equation("=", v11, v12)), Equat(Equation(("=", v21, v22))))
    -> (eq_vt v11 v21) && (not (eq_vt v12 v22))
  | _ -> false

and rm_redundant_equation ((h,ts1):rule) : rule = 
  let (vs1:var list) = fold_right (@) (map get_term_varlist ts1) [] in
  let (vs2:var list) = get_rterm_varlist h in 
  let (newts:term list)  = 
  let rec red_equat (vs1:var list) (vs2:var list) (ts:term list) : term list =
    match ts with
    | [] -> []
    | ((Equat(Equation("=",v,_))) as t::ts') -> if (is_var_in_once v vs1) && not(is_var_in_head v vs2) then
                                             red_equat vs1 vs2 ts'
                                           else 
                                             t::(red_equat vs1 vs2 ts')
    | ((Noneq(Equation("=",v,_))) as t::ts') -> if (is_var_in_once v vs1) && not(is_var_in_head v vs2)then
                                             red_equat vs1 vs2 ts'
                                           else 
                                             t::(red_equat vs1 vs2 ts')
    | (t::ts') -> t::(red_equat vs1 vs2 ts')
  in red_equat vs1 vs2 ts1 in
  (h,newts)
and is_var_in_head (v:vterm) (vs:var list) : bool = 
  match v with
  | Var(v1) ->
     fold_right (||) (map (eq_v v1) vs) false
  | _ -> false
and is_var_in_once (v:vterm) (vs:var list) : bool = 
  match v with
  | Var(v1) ->
     (fold_right (+) (map (fun v2 -> if eq_v v1 v2 then 1 else 0) vs) 0) = 1
  | _ -> false

let rec resolV (vars: (var*var) list) (v:var) : var =
  try (assoc v vars)
  with Not_found -> v 
  

and repone (vars:(var*var) list) (b:term) : term = 
  begin match b with 
  | (Rel(Pred(s,vs'))) -> (Rel(Pred(s,(map (resolV vars) vs'))))
  | (Not(Pred(s,vs'))) -> (Not(Pred(s,(map (resolV vars) vs'))))
  | (Rel(Deltainsert(s,vs'))) -> (Rel(Deltainsert(s,map (resolV vars) vs')))
  | (Not(Deltainsert(s,vs'))) -> (Not(Deltainsert(s,map (resolV vars) vs')))
  | (Rel(Deltadelete(s,vs'))) -> (Rel(Deltadelete(s,map (resolV vars) vs')))
  | (Not(Deltadelete(s,vs'))) -> (Not(Deltadelete(s,map (resolV vars) vs')))
  | (Equat(Equation(s,v1,v2))) ->
     begin match (v1,v2) with
     | (Var(s1),Var(s2)) -> (Equat(Equation(s,Var(resolV vars s1),Var(resolV vars s2))))
     | (Var(s1),_)      -> (Equat(Equation(s,Var(resolV vars s1),v2)))
     | (_,Var(s2))      -> (Equat(Equation(s,v1,Var(resolV vars s2))))
     | _ ->                   b
     end
  | (Noneq(Equation(s,v1,v2))) -> 
     begin match (v1,v2) with
     | (Var(s1),Var(s2)) -> (Noneq(Equation(s,Var(resolV vars s1),Var(resolV vars s2))))
     | (Var(s1),_)      -> (Noneq(Equation(s,Var(resolV vars s1),v2)))
     | (_,Var(s2))      -> (Noneq(Equation(s,v1,Var(resolV vars s2))))
     | _ ->                   b
     end
  end

let var_resolv (vs:var list) (vs1:var list) : (var*var) list = 
  let newvs1=map (fun v -> 
                 if v = AnonVar then 
                   NamedVar("V" ^ string_of_int(fresh())) 
                 else
                   v) vs1 in
  (combine vs newvs1)@[(AnonVar,AnonVar)]

let rec repVbyBody (vs1:var list) (vd:rule) : term list =
  match vd with
  | (Pred(vname, vs),bs) | (Deltainsert(vname, vs),bs) | (Deltadelete(vname, vs),bs) 
    ->
     let (vars1 : (var*var) list) = var_resolv vs vs1 in
     let (vars:(var*var) list)= resolvAno vars1 bs in
     map (repone vars) bs
and resolvAno (vars:(var*var) list) (ts:term list) : (var*var) list = 
  let (bs:bool list) = map (isAno vars) ts in
  let (bts:(bool*term)list) = combine bs ts in
  let rec newv bts vars : (var*var) list =
    match bts with
    | [] -> vars
    | (true, Equat(Equation(_,Var(v),_)))::bts' ->
       newv bts' (remove_assoc v vars)@[(v,NamedVar("V" ^ string_of_int(fresh())))]
    | (true, Noneq(Equation(_,Var(v),_)))::bts' ->
       newv bts' (remove_assoc v vars)@[(v,NamedVar("V" ^ string_of_int(fresh())))]
    | _::bts' -> newv bts' vars 
  in newv bts vars

and isAno (vars:(var*var) list) (t:term) : bool =
  match t with
  | Equat(Equation(_,Var(v),_)) | Noneq(Equation(_,Var(v),_))
    -> (v <> AnonVar) && (resolV vars v) = AnonVar
  | _ -> false

let rec pview_unfold (rs:rule list) (gs:rule list) : rule list =
  (* rs@[(Pred("abc",[]),[])]@gs *)
  (* failwith (fold_right (^) ((map string_of_rule) (rs@gs)) "") *)
  fold_right (@) (map (fun r -> (vuf gs r)) rs) []
and vuf (gs:rule list) (r:rule) : rule list =
  map (fun vdef -> (vu r vdef)) gs
and vu ((h,bs):rule) (vd:rule) : rule =
  (* failwith (fold_right (^) ((map string_of_rule) ([(h,bs);vd])) "") *)
  let (ts:term list) = fold_right (@) (map (fun t -> (repTerm t vd)) bs) []
  in (h,ts)
and repTerm (t:term) ((h,bs):rule) : term list =
  match (t,h) with
  | ((Rel(Pred(s,vs))), (Pred(vname, vs1))) 
  | ((Rel(Deltainsert(s,vs))),(Deltainsert(vname,vs1))) 
  | ((Rel(Deltadelete(s,vs))),(Deltadelete(vname,vs1))) ->
     if vname=s then
       repVbyBody vs (h,bs)
     else
       [t]
  | (_,_) -> [t]

let rec nview_unfold (rs:rule list) (gs:rule list) : rule list = 
  let (ngs:rule list) = map (fun (h,bs) -> (h, map (fun b -> neg b) bs)) gs in
  let new_gs=fold_right (@) (map (fun (h,bs) -> (map (fun b -> (h,[b])) bs)) ngs) [] in
  fold_right (@) (map (fun vdef -> (nvuf rs vdef)) new_gs) []
and nvuf (rs:rule list) (vdef:rule) : rule list =
  map (fun r -> (nvu r vdef)) rs
and nvu ((h,bs):rule) (vd:rule) : rule =
  let (ts:term list) = fold_right (@) (map (fun t -> (repNTerm t vd)) bs) []
  in (h,ts)
and repNTerm (t:term) ((h,bs):rule) : term list =
  match (t,h) with
  | ((Not(Pred(s,vs))), (Pred(vname, vs1))) 
  | ((Not(Deltainsert(s,vs))),(Deltainsert(vname,vs1))) 
  | ((Not(Deltadelete(s,vs))),(Deltadelete(vname,vs1))) ->
     if vname=s then
       repVbyBody vs (h,bs)
     else
       [t]
  | (_,_) -> [t]

and neg (b:term) : term =
    begin match b with 
  | Rel(t) -> Not(t)
  | Not(t) -> Rel(t)
  | Equat(t) -> Noneq(t)
  | Noneq(t) -> Equat(t)
  end

let view_unfold (rs:rule list) (gs:rule list) : rule list = 
  let rs1 = pview_unfold (rs:rule list) (gs:rule list) in
  let rs2 = nview_unfold rs1 gs in
  (* let rs3 = simplification rs2 in
   * rs3 *)
  rs2

let rec flatten_dl (rs:rule list) : rule list = 
  let (rsp:(rule list* rule list)) = split_rs rs in
  let (gss:rule list list)= split_gs (fst rsp) [] in 
  let (rs:rule list) =  apply_unfold gss (snd rsp) in
  let (rs1:rule list)= simplification rs in
  (fst rsp)@rs1

and apply_unfold (gss:rule list list) (ps:rule list) : rule list =
  match gss with
  | [] -> ps
  | gs::gss' -> apply_unfold gss' (view_unfold ps gs) 

and split_gs (gs:rule list) (gss:rule list list) : rule list list =
  match gs with
  | [] -> gss
  | g::gs' -> split_gs gs' (shut g gss)
and shut (g:rule) (gss:rule list list) : rule list list =
  if (fold_right (||) (map (fun gs -> same_hd (hd gs) g) gss) false) then
    map (fun gs-> insert g gs) gss
  else gss@[[g]]
and same_hd ((h1,ts1):rule) ((h2,ts2):rule) : bool =
  h1=h2
and insert ((h1,ts):rule) (gs:rule list) : rule list =
  match (hd gs) with
    (h2,_) -> if h1=h2 then
                gs@[(h1,ts)]
              else
                gs

and split_rs (rs:rule list) : (rule list*rule list) =
    let (bs:bool list) =  map (fun (h,_) -> h_in_rules h rs) rs in
    let rec out (bs:bool list) (rs:rule list) (os:rule list*rule list) : (rule list*rule list) = 
      match (bs,rs) with
      | ([],[]) -> os 
      | (false::bs', r::rs') -> (fst (out bs' rs' os), (snd (out bs' rs' os))@[r])
      | (true::bs', r::rs') -> ((fst (out bs' rs' os))@[r], snd (out bs' rs' os))
      | (_, _) -> failwith "fail in split_rs"
    in out bs rs ([],[])
and h_in_rules (h:rterm) (rs:rule list) : bool = 
  fold_right (||) (map (fun (_,ts) -> h_in_b h ts) rs) false
and h_in_b (h:rterm) (ts:term list) : bool =
  fold_right (||) (map (fun t -> eq_h h t) ts) false
and eq_h (h:rterm) (t:term) : bool =
  match (h,t) with
  | (Pred(s1,_), Rel(Pred(s2,_))) 
    | (Pred(s1,_), Not(Pred(s2,_))) 
    | (Deltainsert(s1,_), Rel(Deltainsert(s2,_))) 
    | (Deltainsert(s1,_), Not(Deltainsert(s2,_)))
    | (Deltadelete(s1,_), Rel(Deltadelete(s2,_))) 
    | (Deltadelete(s1,_), Not(Deltadelete(s2,_))) ->
     (s1=s2) 
  | (_,_) -> false

let rec rep_delta_pred (vname:string) (ac:term list list) (bs:term list) : term list list =
  begin match bs with
  | [] -> ac
  | (Rel(Pred(s,vs)) as b)::bs' ->
     if vname=s then
       let (plsV : term list) = [(Rel(Deltainsert(s,vs)))] in
       let (vnot : term list) = [b; (Not(Deltadelete(s,vs)))] in
       let (ac' : term list list) = 
         begin match ac with
         | [[]] -> [plsV; vnot]
         | ass -> let a1 = map (fun aa -> aa@plsV) ass in
                  let a2 = map (fun aa -> aa@vnot) ass in
                  a1@a2
         end
       in rep_delta_pred vname ac' bs'
     else rep_delta_pred vname (map (fun a-> a@[b]) ac) bs'
  | (Not(Pred(s,vs)) as b)::bs' ->
     if vname=s then 
       let (plsV : term list) = [(Not(Deltainsert(s,vs)))] in
       let (not1 : term list) = [Not(Pred(s,vs))] in
       let (not2 : term list) = [(Rel(Deltadelete(s,vs)))] in
       let (ac' : term list list) = 
         begin match ac with
         | [[]] -> [plsV; not1; not2]
         | ass -> let a1 = map (fun aa -> aa@plsV) ass in
                  let a2 = map (fun aa -> aa@not1) ass in
                  let a3 = map (fun aa -> aa@not2) ass in
                  a1@a2@a3
         end
       in rep_delta_pred vname ac' bs'
     else rep_delta_pred vname (map (fun a-> a@[b]) ac) bs'
  | b::bs' ->
     rep_delta_pred vname (map (fun a-> a@[b]) ac) bs'
  end

let g_method ((h,b):rule) ((vname,ss):view) (gs:rule list) : rule list = 
  
    let (bss : term list list) = rep_delta_pred vname [[]] b in
    let (rs:rule list) =  map (fun bs -> (h,bs)) bss in 
    rs


let incrementalization ((vname,ss):view) ((h,b):rule) : rule list =
  let rec inc (vname:string) (body:term list) : term list =
    match body with
    | [] -> []
    | (Rel(Pred(s,vs)) as b')::bs ->
       if vname=s then [(Rel(Deltainsert(s,vs)))] @ bs
       else [b'] @(inc vname bs)
    | (Not(Pred(s,vs)) as b')::bs ->
       if vname=s then [(Rel(Deltadelete(s,vs)))] @ bs
       else [b'] @ (inc vname bs)
    | b'::bs -> [b'] @ (inc vname bs)
  in [(h,(inc vname b))]


let rec isLVGN ((vname,ss):view) ((head, body) :rule) : bool =
  let (num_of_view_in_body:int) = 
    fold_right (+) (map (eq_name vname) body) 0
  in (num_of_view_in_body=1)
and eq_name (vname:string) (t:term) : int =
  match t with
  | Rel(Pred(s,_)) | Not(Pred(s,_))
    -> if vname=s then 1
       else 0
  | _ -> 0

let updated_view_rs ((h,ts):rule) : rule list =
  match h with
  | Pred(vname,vs) ->
     let (r1:rule) = (h,[Rel(Deltainsert(vname,vs))]) in           (* v(A) :- +v(A) *)
     let (r2:rule) = (h,[Rel(h);Not(Deltadelete(vname,vs))]) in    (* V(A) :- v(A), not -v(A) *)
     [r1;r2]
  | _ -> failwith "error in updated_view_rs"

let opb_rules (gs:rule list) (ps:rule list) (v:view) : rule list =
  let rec opb_rule (gs:rule list) (p:rule) (v:view) : rule list =
    if isLVGN v p then
      incrementalization v p 
    (* else g_method p v gs *)
    else
      let (rs1:rule list) = updated_view_rs (hd gs) in
      view_unfold [p] rs1
  in
  let (rs:rule list)= fold_right (@) (map (fun r -> opb_rule gs r v) ps) [] in
  let (rs1 : rule list) = view_unfold rs gs in
  let (rs2:rule list) = simplification rs1 in
  let (rs3:rule list) = flatten_dl rs2 in
  rs3
  (* gs@rs3 *)


let opb (e:expr) : rule list  = 
  match e.view with
  | None -> failwith "view is not specified."
  | Some v' ->
     let (v:view) = v' in
     let (gs,ps) = extract_get_puts v e in
     if gs=[] then
       failwith "View definition is not specified"
     else 
       opb_rules gs ps v
>>>>>>> d96beeb (introduce files)
