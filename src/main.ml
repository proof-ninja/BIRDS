<<<<<<< HEAD
(*
@author: Vandang Tran
*)

(**  main file, Execution from command line, connection info, help printouts for command line usage etc.
*)
open Lib
open Formulas
open Fol
open Skolem
open Fol_ex
open Lexer
exception Eof
open Printf
open Postgresql
open Conn_ops
open Utils
open Derivation
open Arg
open Ast2theorem
open Ast2fol
open Bx
open Expr

(** check for options of command line*)
(*default values*)
let host = ref "192.168.56.101"
let port = ref 5432
let user = ref "postgres"
let dejima_user = ref "dejima"
let password = ref "12345678"
let dbname = ref "datalogdb"
let environment = ref false
let log = ref false
let debug = ref false
let explain = ref false
let print_version = ref false
let connectdb = ref false
let importschema = ref false
let dejima_ud = ref false
let verification = ref false
let cex_generation = ref false
let cex_max = ref 5
let inc = ref false
let optimize = ref false
let speedup = ref false
let inputf = ref ""
let inputshell = ref ""
let outputf = ref ""
let outputlean = ref ""
let dbschema = ref "public"
let timeout = ref 180

let usage = "usage: " ^ Sys.argv.(0) ^ " [OPTIONS]"
let speclist = [
  ("--version", Arg.Unit (fun () -> print_version := true),  " Print version");
  ("--environment", Arg.Unit (fun () -> environment := true),      " Check external commands and configurations");
  ("--log", Arg.Unit (fun () -> log := true),  " Print running information");
  ("--debug", Arg.Unit (fun () -> debug := true),  " Enable debugging mode");
  ("--explain", Arg.Unit (fun () -> explain := true),  " Show only explanations in the debugging mode");
  ("-f", Arg.String (fun s -> inputf := s),   "<file> Input program file, if not chosen, read from stdin");
  ("-b", Arg.String (fun s -> inputshell := s),   "<file> Shell script file specifying the action, which will be executed when there is an update on the view, if not chosen, execute nothing");
  ("-o", Arg.String (fun s -> outputf := s),   "<file> Output SQL file, if not chosen, print to stdout");
  ("-l", Arg.String (fun s -> outputlean := s),   "<file> Output verification file (optional)");
  ("-s", Arg.String (fun s -> dbschema := s),  "<schema> Database schema name to connect to (default: public)");
  ("-h", Arg.String (fun s -> host := s),      "<host> Database server host (default: \"localhost\")");
  ("-c", Arg.Unit (fun () -> connectdb := true),      " Connect and run the generated SQL on the database server");
  ("--import", Arg.Unit (fun () -> importschema := true),      " Connect and import the data schema from the database server");
  ("-v", Arg.Unit (fun () -> verification := true),      " Enable verifications");
  ("--verification", Arg.Unit (fun () -> verification := true),      " The same as -v");
  ("-x", Arg.Int (fun d -> cex_generation := true; cex_max := d),      "<size> Get a counterexample with the maximum size if the program is not well-behaved");
  ("--counterexample", Arg.Int (fun d -> cex_generation := true; cex_max := d),      "<size> The same as -x");
  ("-i", Arg.Unit (fun () -> inc := true),      " Optimize the update propagation by incremental rewriting rules");
  ("--incrementalization", Arg.Unit (fun () -> inc := true),      " The same as -i");
  ("-e", Arg.Unit (fun () -> optimize := true),      " Optimize datalog rules");
  ("--optimization", Arg.Unit (fun () -> optimize := true),      " The same as -e");
  ("-u", Arg.Unit (fun () -> speedup := true),      " Speed up the verifications");
  ("--speedup", Arg.Unit (fun () -> speedup := true),      " The same as -u");
  ("-p", Arg.Int    (fun d -> port := d),      "<port> Database server port (default: \"5432\")");
  ("-U", Arg.String (fun s -> user := s),      "<user> Database user (default: \"postgres\")");
  ("-g", Arg.String (fun s -> dejima_user := s),      "<user> The special user for global dejima synchronization (default: \"dejima\")");
  ("--dejima", Arg.Unit (fun () -> dejima_ud := true),      " Detect updates on dejima views to perform predefined actions in the shell script file");
  ("-w", Arg.String (fun s -> password := s),  "<password> Database user password (default: 12345678)");
  ("-d", Arg.String (fun s -> dbname := s),    "<dbname> Database name to connect to (default: \"datalogdb\")");
  ("-t", Arg.Int (fun d -> timeout := d),    "<timeout> Timeout (second) (default: 120s)");
=======
(* 
@Authox: Vandang Tran
*)

(**  main file, Execution from command line, connection info, help printouts for command line usage etc. 
*)
(* open Lib
 * open Formulas
 * open Fol
 * open Skolem
 * open Fol_ex *)
open String
open Lexer 
exception Eof
open List
open Printf
open Postgresql
(* open Conn_ops *)
open Utils
(* open Derivation *)
open Ast2sql
open Arg
(* open Ast2theorem
 * open Ast2fol
 * open Bx *)
open Expr2

(** check for options of command line*)
(*default values*)
let inputf = ref ""

let usage = "usage: " ^ Sys.argv.(0) ^ " [OPTIONS]"
let speclist = [
    ("-f", Arg.String (fun s -> inputf := s),   "<file> Input program file, if not chosen, read from stdin");
>>>>>>> d96beeb (introduce files)
]

let () =
  (* Read the arguments *)
  Arg.parse
    (Arg.align speclist)
    (fun x ->
       raise (Arg.Bad ("Bad argument : " ^ x))
    )
    usage

<<<<<<< HEAD
(** assign postgreSQL connection parameters to conninfo variable *)
let conninfo =
  sprintf "host=%s port=%d user=%s password=%s dbname=%s"
    !host !port !user !password !dbname

(* pretty print connection informations *)
let print_conn_info conn =
  print_endline "------Connected to:--------";
  printf "dbname    = %s\n" conn#db;
  printf "user      = %s\n" conn#user;
  printf "password  = %s\n" conn#pass;
  printf "host      = %s\n" conn#host;
  printf "port      = %s\n" conn#port;
  printf "tty       = %s\n" conn#tty;
  printf "options   = %s\n" conn#options;
  printf "pid       = %i\n" conn#backend_pid;
  print_endline "--------------\n"

let main () =
  if (!print_version) then ((print_endline "BIRDS version 0.0.6"); exit 0);
  if (!environment) then (
    print_endline "...Checking external tools (this may take a few minutes)...";
    (try
      ignore (check_command_version "timeout");
    with
      | EnvErr msg -> fprintf stderr "%s\n" "WARNING: Command 'timeout' can not be called! Verifications and counterexample generation will not be performed correctly, and hence Datalog rules of the view definition must be explicitly written.";
      | e -> prerr_endline (Printexc.to_string e));

    (try
      let lean_version = check_command_version "lean" in
      print_endline ("INFO: The current version of Lean is: \'"^lean_version^"\'. Please be sure it is greater than or equal to 3.4.2 to avoid unexpected errors of verification.");
      (* check lean paths *)
      let tmp_chklib_file = Filename.temp_file "" ".lean" in
      let chklib =  open_out tmp_chklib_file in
      fprintf chklib "%s\n" "import bx";
      close_out chklib;
      let leanstatus, leanmessage = exe_command @@ "lean "^tmp_chklib_file in
      if not (leanstatus = 0) then
        fprintf stderr "%s\n" ("WARNING: Lean paths to BIRDS's verification folder are not configured correctly! Verifications will not be performed correctly, and hence Datalog rules of the view definition must be explicitly written. Please change the Lean path configuration in ~/.lean/leanpkg.path and check by 'lean --path'. More details at https://github.com/dangtv/BIRDS.")
      else ();
    with
      | EnvErr msg -> fprintf stderr "%s\n" "WARNING: Command 'lean' can not be called! Verifications will not be performed correctly, and hence Datalog rules of the view definition must be explicitly written.";
      | e -> prerr_endline (Printexc.to_string e));

    (try
      let z3_version = check_command_version "z3" in
      print_endline ("INFO: The current version of Z3 is: \'"^z3_version^"\'. Please be sure it is greater than or equal to 4.8.7 to avoid unexpected errors of verification.");
    with
      | EnvErr msg -> fprintf stderr "%s\n" "WARNING: Command 'z3' can not be called! Verifications will not be performed correctly, and hence Datalog rules of the view definition must be explicitly written.";
      | e -> prerr_endline (Printexc.to_string e));

    (try
      ignore (check_command_version "racket");
      (* check racket libs *)
      let tmp_chklib_file = Filename.temp_file "" ".rkt" in
      let chklib =  open_out tmp_chklib_file in
      fprintf chklib "%s\n" "#lang rosette";
      close_out chklib;
      let racketstatus, racketmessage = exe_command @@ "racket "^tmp_chklib_file in
      if not (racketstatus = 0) then
        fprintf stderr ("WARNING: Package rosette is not installed correctly! Counterexamples generators will not work. Please install by 'raco pkg install rosette'. More details at https://github.com/dangtv/BIRDS.")
      else ();
    with
      | EnvErr msg -> fprintf stderr "%s\n" "WARNING: Command 'racket' can not be called! Counterexamples generators will not work.";
      | e -> prerr_endline (Printexc.to_string e));

    print_endline "...Checking is completed (\'birds\' can be fully used if there is no WARNING)...";
    exit 0
    );
  if (!importschema) then
    (if !log then print_endline "importing schema from the database";
    let c = new connection ~conninfo () in
    if !log then (
      print_conn_info c;
      flush stdout
    ) else ();
    let schema = import_dbschema c (!dbschema) in
    c#finish;
    if (!log) then (
      print_endline "------imported DB Schema:--------";
      print_endline @@ Expr.string_of_prog {get_empty_expr with sources = schema};
      print_endline "--------------\n";
      flush stdout;
      );
    let oc =if !outputf = "" then stdout else open_out !outputf  in
    fprintf oc "%s\n" (Expr.string_of_prog {get_empty_expr with sources = schema});
    close_out oc;
    exit 0;
    );

  let chan = if !inputf = "" then stdin else open_in !inputf in
  let lexbuf = Lexing.from_channel chan in
  (* add information of the file name to lexbuf *)
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname =  (if !inputf = "" then "stdin" else !inputf)};
  (* while true do *)
  let new_ast = Parser.main Lexer.token lexbuf in
  (* let original_ast = Conversion.expr_of_expr2 new_ast in  *)
  let original_ast = new_ast in
  let shell_script = if !inputshell = "" then "#!/bin/sh\necho \"true\"" else (String.concat "\n" @@ read_file (!inputshell)) in
  let ast =  original_ast in
  let edb = extract_edb ast in

  if !log then (
    print_endline "------DB Schema:--------";
    print_endline @@ string_of_prog {get_empty_expr with view = ast.view; sources = ast.sources} ;
    (* print_symtable edb; *)
    print_endline "--------------\n";
    flush stdout;
  ) else ();
  let idb = extract_idb ast in
  if !log then (
    print_endline "------Rules:--------";
    print_symtable idb;
    print_endline "--------------\n";
  ) else ();
  let has_get = is_defined_pred (get_rterm_predname (get_schema_rterm (get_view ast))) ast in

  if !debug then
    (if !log then print_endline ">> Start debugging";
    if has_get then
      if !explain then
        (
        print_endline "==========Checking constraints=============" ;
        if (Debugger.check_constraints !log ast) then print_endline "Constraints are satisfied"
        else print_endline "Constraints are not satisfied";
        print_endline "===========================================\n" ;
        print_endline "==========Testing the delta disjointness property w.r.t the given tuples=============" ;
        Debugger.explain_disdelta !log ast;
        print_endline "==============================================================\n" ;
        print_endline "===============Testing the getput property w.r.t the given tuples================";
        Debugger.explain_getput !log ast;
        print_endline "==========================================================\n" ;
        print_endline "===============Testing the putget property w.r.t the given tuples================";
        Debugger.explain_putget !log ast;
        print_endline "==========================================================\n" ;
        )
      else (
        Debugger.debug_disdelta !log ast;
        Debugger.debug_getput !log ast;
        Debugger.debug_putget !log ast
      )
    )
  else
    (let ast2, view_rules_string =
      if has_get then (
        (* verification *)
        if (!verification || !cex_generation) then
          (
          let constr_ast = constraint2rule ast in
          let disdelta_thm = lean_simp_theorem_of_disjoint_delta (!log) constr_ast in
          let getput_thm = lean_simp_theorem_of_getput (!log) constr_ast in
          let putget_thm = lean_simp_theorem_of_putget (!log) constr_ast in
          if(!speedup) then
            (
            (* verify all properties *)
            if (!log) then print_endline "==> verifying all properties";
            let lean_code_all_thms = gen_lean_code_for_theorems [ disdelta_thm; getput_thm; putget_thm ] in
              let exitcode, message = verify_fo_lean (!log) (!timeout) lean_code_all_thms in
              if not (exitcode=0) then
                if (exitcode = 124) then (raise (ChkErr ("Stop verifying: Timeout")))
                else
                  if str_contains message "type mismatch at application" then
                  (* type error *)
                  (raise (ChkErr ("Invalidity: Type mismatch: \n"^ cut_str_by_word message "error"
                    ^ (if (!log) then "\nError messange: "^ message else ""))))
                  else
                    (raise (ChkErr ("Invalidity: Well-behavedness is not validated \nExit code: " ^ string_of_int exitcode
                    ^ (if (!log) then "\nError messange: "^ message else ""))));
            )
          else (
            (* verify the delta disjointness property *)
            let satisfying_getput = ref false in
            let satisfying_putget = ref false in
            let satisfying_deltadis = ref false in
            let verification_mess = ref "" in
            let counterexample_mess = ref "" in
            if (!log) then print_endline "==> Verifying the delta disjointness property";
            let lean_code_disdelta = gen_lean_code_for_theorems [ disdelta_thm ] in
              let exitcode, message = verify_fo_lean (!log) (!timeout) lean_code_disdelta in
              if not (exitcode=0) then
                if (exitcode = 124) then (
                  if (!log) then print_endline "Stop verifying the delta disjointness property: Timeout";
                  verification_mess :=  (!verification_mess)^ "\n\n" ^"Stop verifying the delta disjointness property: Timeout")
                else
                  if str_contains message "type mismatch at application" then
                  (* type error *)
                  (let m = "Invalidity of delta disjointness: Type mismatch: \n"^ cut_str_by_word message "error"
                    ^ (if (!log) then "\nError messange: "^ message else "") in
                    if(!log) then print_endline m;
                    verification_mess :=  (!verification_mess)^ "\n\n" ^m)
                  else
                    if (exitcode=1 && !cex_generation) then
                      (let error, counterexample = Debugger.gen_counterexample !log "disdelta" !cex_max !timeout constr_ast in
                      let m = if (error = "") then ("% Invalidity: The following counterexample shows that deltas in the datalog program are not disjoint:\n" ^ (string_of_prog {get_empty_expr with facts = counterexample}) )
                              else "% Fail to generate a couterexample of delta disjointness: " ^ error in
                      if (!log) then print_endline m;
                      counterexample_mess := (!counterexample_mess) ^ "\n\n" ^ m
                        (* exit 0 *))
                    else
                      (let m = ("Invalidity: Deltas in the datalog program are not disjoint \nExit code: " ^ string_of_int exitcode
                      ^ (if (!log) then "\nError messange: "^ message else "") ^"\nHint: use option --counterexample to generate a counterexample") in
                      if (!log) then print_endline m;
                      verification_mess := (!verification_mess) ^ "\n\n" ^ m)
              else satisfying_deltadis := true;
            (* verify getput property *)
            if (!log) then print_endline "==> verifying the getput property";
            let lean_code_getput = gen_lean_code_for_theorems [ getput_thm ] in
              let exitcode, message = verify_fo_lean (!log) (!timeout) lean_code_getput in
              if not (exitcode=0) then
                if (exitcode = 124) then (
                  if (!log) then print_endline "Stop verifying the getput property: Timeout";
                  verification_mess :=  (!verification_mess)^ "\n\n" ^ "Stop verifying the getput property: Timeout")
                else
                  if str_contains message "type mismatch at application" then
                  (* type error *)
                  (let m = ("Invalidity of getput: Type mismatch: \n"^ cut_str_by_word message "error"
                    ^ (if (!log) then "\nError messange: "^ message else "")) in
                    if (!log) then print_endline m;
                    verification_mess :=  (!verification_mess)^ "\n\n" ^ m)
                  else
                    if (exitcode=1 && !cex_generation) then
                      (let error, counterexample = Debugger.gen_counterexample !log "getput" !cex_max !timeout constr_ast in
                      let m = if (error = "") then ("% Invalidity: The following counterexample shows that getput is not satisfied:\n" ^ string_of_prog {get_empty_expr with facts = counterexample} )
                              else "% Fail to generate a counterexample of getput: " ^error in
                      if (!log) then print_endline m;
                        counterexample_mess := (!counterexample_mess) ^ "\n\n" ^ m
                      (* exit 0 *)
                      )
                    else
                      (let m = ("Invalidity: Property getput is not validated \nExit code: " ^ string_of_int exitcode
                      ^ (if (!log) then "\nError messange: "^ message else "")^"\nHint: use option --counterexample to generate a counterexample") in
                      if (!log) then print_endline m;
                      verification_mess := (!verification_mess) ^ "\n\n" ^ m)
              else satisfying_getput := true;
            (* verify putget *)
            if (!log) then print_endline "==> Verifying the putget property";
            let lean_code_putget = gen_lean_code_for_theorems [ putget_thm ] in
              let exitcode, message = verify_fo_lean (!log) (!timeout) lean_code_putget in
              if not (exitcode=0) then
                if (exitcode = 124) then (
                  if (!log) then print_endline "Stop verifying the getput property: Timeout";
                  verification_mess := (!verification_mess) ^ "\n\n" ^ ("Stop verifying the getput property: Timeout"))
                else
                  if str_contains message "Type mismatch at application" then
                  (* type error *)
                  (let m = ("Invalidity of putget: Type mismatch: \n"^ cut_str_by_word message "error"
                    ^ (if (!log) then "\nError messange: "^ message else "")) in
                  verification_mess := (!verification_mess) ^ "\n\n" ^ m)
                  else
                    if (exitcode=1 && !cex_generation) then
                      (let error, counterexample = Debugger.gen_counterexample !log "putget" !cex_max !timeout constr_ast in
                      let m = if (error = "") then ("% Invalidity: The following counterexample shows that putget is not satisfied:\n" ^ string_of_prog {get_empty_expr with facts = counterexample})
                              else "% Fail to generate a counterexample of putget: " ^error in
                      if (!log) then print_endline m;
                      counterexample_mess := (!counterexample_mess) ^ "\n\n" ^ m
                        (* exit 0 *))
                    else
                      (let m = ("Invalidity: Property putget is not validated \nExit code: " ^ string_of_int exitcode
                      ^ (if (!log) then "\nError messange: "^ message else "")^"\nHint: use option --counterexample to generate a counterexample") in
                      if (!log) then print_endline m;
                      verification_mess := (!verification_mess) ^ "\n\n" ^ m)
              else satisfying_putget := true;
              (* conclusion *)
              if(!satisfying_deltadis && !satisfying_getput && !satisfying_putget) then (
                if (!log) then print_endline "The program satisfies all delta disjointness, getput and putget"
              )
              else (
                if (!cex_generation) then (print_endline !counterexample_mess; exit 0)
                else (raise (ChkErr !verification_mess))
              )
            );
          );
        ast, "")
      else (
        let get_ast = Bx.derive_get_datalog (!log) (!speedup) (!timeout) ast in
        (* let get_rules exp = fst (Rule_preprocess.seperate_rules exp) in  *)
        let bi_prog = Expr.add_rules get_ast.rules ast in
        if !log then (
          print_endline "_____get&put (bidirectional) datalog program_______";
          print_string (Expr.string_of_prog  bi_prog);
          print_endline "______________\n";
              ) else ();
        bi_prog, (Expr.string_of_prog {get_empty_expr with rules = get_ast.rules})) in
    let lean_code =
    (
      if has_get then validity_lean_code_of_bidirectional_datalog (!log) (constraint2rule ast2) else
        gen_lean_code_for_theorems [ (lean_simp_theorem_of_putget (!log) (constraint2rule ast2)) ]
    ) in
    if not (!outputlean = "") then
      (let ol =  open_out (!outputlean)  in
      fprintf ol "%s\n" lean_code;
      close_out ol;);

    if (!verification) then print_endline @@ "-- Program is validated --";
    let oc =if !outputf = "" then stdout else open_out !outputf  in
    if (not has_get) then fprintf oc "\n/*view definition (get):\n%s*/\n\n" view_rules_string;
    let sql = Ast2sql.unfold_view_sql (!dbschema) (!log) ast2 in
    fprintf oc "%s\n" sql;
    let trigger_sql = Ast2sql.unfold_delta_trigger_stt (!dbschema) (!log) (!dejima_ud) shell_script (!dejima_user) (!inc) (!optimize) (constraint2rule ast2) in
    fprintf oc "%s\n" trigger_sql;

    if (!connectdb) then
      let c = new connection ~conninfo () in
      if !log then (
        print_conn_info c;
        flush stdout
      ) else ();
      c#set_notice_processor (fun s -> eprintf "postgresql error [%s]\n" s);
    print_creating_sql c (sql^trigger_sql);
    close_out oc;
    c#finish)

let test() =
    let log = true in
    let inputf = "tests/debug.dl" in
    let outputf = "tests/debug.out" in
    let outputlean = "tests/debug.lean" in
    let chan = if  inputf = "" then stdin else open_in  inputf in
    let lexbuf = Lexing.from_channel chan in
    let dbschema = "public" in
    (* add information of the file name to lexbuf *)
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname =  (if  inputf = "" then "stdin" else  inputf)};
    (* while true do *)
    try
      let new_ast = Parser.main Lexer.token lexbuf in
      (* let ast1 = Conversion.expr_of_expr2 new_ast in *)
      let ast1 = new_ast in
      let ast = (constraint2rule ast1) in
      (* let edb = import_dbschema c ( dbschema) in  *)
      let edb = extract_edb ast in
      (* edb is set of rules whose head is tablename with column, and its body is none*)
      if  log then (
        print_endline "--------------";
        print_endline "DB Schema:\n";
        print_symtable edb;
        print_endline "--------------";
        flush stdout;
      ) else ();

      (* let idb = extract_idb ast in
      if  log then (
                    print_symtable idb;print_endline "______________\n";
                ) else (); *)
      if  log then (
        print_endline (Expr.string_of_prog ast);
                    print_endline "______________\n";
                ) else ();
      let oc =if  outputf = "" then stdout else open_out  outputf  in
      let ol =if  outputf = "" then stdout else open_out  outputlean  in
      (* fprintf oc "/*";
        fprintf oc "_____ datalog program _______\n";
        fprintf oc "%s\n" (Expr.string_of_prog  ast);
        fprintf oc "______________";
        fprintf oc "*/\n\n"; *)

      (* let lambda = Ast2lambda.disjoint_delta_theorem_of_stt ( log) edb ast in
      fprintf oc "%s\n" lambda; *)
      (* fprintf oc "%s\n\n" (Ast2theorem.z3_assert_of_disjoint_delta ( log) ast); *)
      (* fprintf ol "%s\n\n" (gen_lean_code_for_theorems [(Ast2theorem.lean_simp_theorem_of_disjoint_delta ( log) ast);  *)
      (* (Ast2theorem.lean_simp_theorem_of_getput ( log) ast);(Ast2theorem.lean_simp_theorem_of_putget ( log) ast)]); *)
      (* let _,fm = fol_of_program_query ( log) ast in
      fprintf oc "%s\n" (lean_string_of_fol_formula ( fm));
      fprintf oc "%s\n" (z3_string_of_fol_formula ( fm)); *)
      (* fprintf oc "%s\n" (lean_string_of_fol_formula (srnf fm)); *)
      (* fprintf oc "%s\n" (string_of_fol_formula (ranf fm)); *)
      (* let constructed_prog = ( Expr.add_stts (Expr.get_schema_stts ast) (fol2datalog (fv fm) fm)) in *)
      (* let constructed_prog = fol2datalog (get_query ast) (Expr.get_schema_stts ast) (fv fm) fm in *)
      (* let constructed_prog = optimize_query_datalog (log) ast in *)
      (* fprintf oc "%s\n" (Expr.string_of_prog constructed_prog); *)
      (* fprintf oc "%s\n" (Ast2sql.unfold_program_query "public" false constructed_prog); *)
      (* fprintf oc "%s\n" (lean_string_of_fol_formula (srnf (fol_of_stt ( log) edb (fol2datalog (fv fm) fm)))); *)
      (* if Fol_ex.is_safe_range fm then fprintf oc "OK === \n"; *)

      (* fprintf oc "%s\n" (Expr.string_of_prog (Expr.add_stts (Derivation.datalog_of_new_view log ast) (Expr.add_stts (Derivation.datalog_of_delta_appliation ( log) ast) ast))); *)
      (* let trigger_sql = Ast2sql.unfold_delta_trigger_stt "public" log true true ast in
        fprintf oc "%s\n" trigger_sql; *)
      (* let constr_sql = Ast2sql.view_constraint_sql_of_stt "public" log false ast in *)
         (* fprintf oc "%s\n" constr_sql; *)



      (* let get_ast = derive_get_datalog log false 500 ast in
      fprintf oc "view datalog: %s \n" (Expr.string_of_prog get_ast); *)

      let error, counterexample = Debugger.gen_counterexample log "getput" 5 120 ast in
      fprintf oc "\n===== getput counterexample ===== \n" ;
      let buggy_prog = {ast with facts = counterexample} in
      fprintf oc "%s \n" (string_of_prog buggy_prog);
      let resulted_facts, explanation = Evaluation.eval log (get_delta_rterms buggy_prog) buggy_prog in
      fprintf oc "\n===== results ===== \n%s \n" (string_of_prog {get_empty_expr with facts = resulted_facts} );
      fprintf oc "%s" ( String.concat "\n" (List.map (Debugger.string_of_explanation) explanation));

      (* Debugger.debug_getput log ast; *)
      (* Debugger.debug_putget log ast; *)
      Debugger.debug_disdelta log ast;


      close_out oc;
      close_out ol;
    with
      SemErr exp -> print_endline ("Semantic error: "^exp)
    | ChkErr msg -> print_endline ("Verification error: "^msg)
    | EnvErr msg -> print_endline ("Environment error: "^msg)
    | Parsing.Parse_error ->  print_endline "Syntax error"
    | LexErr msg -> print_endline (msg^":\nError: Illegal characters")
    | ParseErr msg -> print_endline (msg^":\nError: Syntax error")

let small_test() =
  let l = ["a4"; "a2"; "a3"] in
  let sortlst = Lib.sort (fun a b -> a < b) l in
  List.iter (fun a -> print_endline a) sortlst
=======
let initu (e:expr) : update =
  match e.update with
  | None -> failwith "update is not specified"
  | Some u -> u

let initv (e:expr) : view =
  match e.view with
  | None -> failwith "view is not specified"
  | Some v -> v

let rec update2dl (u:update) (v:view) (vs:var list) : rule list =
  let (attrs:string list) = get_schema_attrs v in
  let (avs:(string*var) list) = combine attrs vs in
  match u with
  | Update (t,us,cs) -> 
     let (dlt:rule) = (Deltadelete(t, vs), Rel(Pred(t, vs))::(map (c2t t avs)) cs) in
     let (ad:rule) = (Deltainsert(t,vs), (((map (uset2t avs)) us)@[Rel(Deltadelete(t, newvs us avs vs))])) in
     [dlt;(vu ad dlt)]
     (* [dlt;ad] *)
  | Insert1 (t,cts) -> 
     let (vcs:(var*const) list) = combine vs cts in 
     [(Deltainsert(t,vs), (map ct2t) vcs)]
  | Insert2 (t,sql) ->
     failwith "Insert2 is not implemented yet in update2dl"
  | Delete (t,cs) ->
     [(Deltadelete(t, vs), Rel(Pred(t, vs))::(map (c2t t avs)) cs)]
and newvs (us:uset list) avs (vs:var list) : var list =
  match vs with
  | [] -> []
  | v::vs' -> if contv v avs us then
                (NamedVar("V" ^ string_of_int(fresh())))::(newvs us avs vs')
              else 
                v::(newvs us avs vs')
and contv (v:var) avs (us:uset list) : bool = 
  fold_right (||) (map (ctv v avs) us) false 
and ctv (v:var) avs ((s,_):uset) : bool =
  match v with
  | NamedVar(s') -> (assoc s avs)=v
  | _ -> failwith "var should be NamedVar"
and uset2t avs ((s,v):uset) : term =
   Equat(Equation("=", Var(assoc s avs), v))
and ct2t ((v,c):var*const) : term =
   Equat(Equation("=", Var(v), Const(c)))
and c2t (t:string) avs (c:condition) : term =
  match c with
  | Condition(op, s, v) -> 
       Equat (Equation (op, Var(assoc s avs), v))
  | _ -> failwith "EXISTS and NOT EXISTS are not implemented yet in delete2datalo"

let rec dl2update (d:rule) (v:view) : update =
  match d with
  | (Deltainsert(t,vs), ts) -> Insert2(t,ts2sql ts)
  | (Deltadelete(t,vs), ts) -> Delete(t,ts2cs ts)
  | _ -> failwith "normal predicates in dl2update"
and ts2sql (ts:term list) : sql =
  Select([],[],[])
and ts2cs (ts:term list) : condition list =
  []

let rec utrans (ps:rule list) (us:rule list) : rule list =
  fold_right (@) (map (tran ps) us) []
and tran (ps:rule list) ((h,ts):rule) : rule list =
  (* let rs = extract_effective_e ps h in *)
  let rs = map (rep_dlt_by_bool h) ps in
  (* let rs1 = map rm_true rs in *)
  let rs2 = rm_false_rule rs in
  (* failwith (fold_right (^) ((map string_of_rule) ((h,ts)::rs)) "") *)
  view_unfold rs2 [(h,ts)]
and rm_true ((h,ts):rule) : rule =
  let ts1 = 
  let rec rm_t (ts:term list) : term list =
    match ts with
    | [] -> ts
    | (Bool(true)::ts') -> rm_t ts'
    | t::ts' -> t::rm_t ts'
  in rm_t ts
  in (h,ts1)
and rm_false_rule (rs:rule list) : rule list =
  match rs with
  | [] -> []
  | (h,ts)::rs' -> if false_in ts then
                     rm_false_rule rs'
                   else
                     (h,ts)::(rm_false_rule rs')
and false_in (ts:term list) : bool =
  fold_right (||) (map (fun t -> t=Bool(false))ts) false

and rep_dlt_by_bool (h1:rterm) ((h2,ts):rule) : rule =
  (h2,(map (rep_d_by_b h1) ts ))
and rep_d_by_b (h1:rterm) (t:term) : term =
  match (h1,t) with
  | (Deltainsert(_), Rel(Deltadelete(_))) -> Bool(false)
  | (Deltadelete(_), Rel(Deltainsert(_))) -> Bool(false)
  | (Deltainsert(_), Not(Deltadelete(_))) -> Bool(false)
  | (Deltadelete(_), Not(Deltainsert(_))) -> Bool(false)
  | _ -> t

and extract_effective_e (ps:rule list) (rt:rterm) : rule list =
  let rs = 
  match ps with
  | [] -> []
  | (r,ts')::ps' -> if fold_right (||) (map (eq_name rt) ts') false then
                      (r,ts')::(extract_effective_e ps' rt)
                    else
                      (extract_effective_e ps' rt)
  in
  let rec non_effect_e (rs:rule list) (h:rterm) : rule list = 
  match rs with
  | [] -> []
  | (r,ts')::ps' -> if fold_right (||) (map (non_effct h) ts') false then
                      (non_effect_e ps' h)
                    else
                      (r,ts')::(non_effect_e ps' h)
  in non_effect_e rs rt
and non_effct (rt:rterm) (t:term) : bool =
  match (rt,t) with
  | (Deltainsert(s1,_), Rel(Deltadelete(s2,_)))
    | (Deltainsert(s1,_), Not(Deltadelete(s2,_)))
    | (Deltadelete(s1,_), Rel(Deltainsert(s2,_)))
    | (Deltadelete(s1,_), Not(Deltainsert(s2,_)))
    -> s1=s2
  | _ -> false

and eq_name (rt:rterm) (t:term) : bool =
  match (rt,t) with
  | (rt1, Rel(rt2)) -> 
     eq_rt_name rt1 rt2
  | _ -> false
and eq_rt_name (rt1:rterm) (rt2:rterm) : bool =
  match (rt1,rt2) with 
  | (Pred(s1,_), Pred(s2,_)) 
    | (Deltainsert(s1,_), Deltainsert(s2,_))
    | (Deltadelete(s1,_), Deltadelete(s2,_)) ->
     s1=s2
  | _ -> false

let extract_view_vars ((h,_):rule) : var list =
  match h with
  | Pred(_,vs) | Deltainsert(_,vs) | Deltadelete(_,vs) -> vs

let view_update_translation (e:expr) = 
  let (u:update) = initu e in
  let (v:view) = initv e in
  let (gs,ps) = extract_get_puts v e in
  let ps = opb e in
  let (vs:var list) = extract_view_vars (hd gs) in 
  let (rs0:rule list) = update2dl u v vs in
  (* failwith (fold_right (^) (map string_of_rule gs) "") *)
  let (rs1:rule list) =pview_unfold rs0 gs in
  let (rs2:rule list) = utrans ps rs1 in
  let (rs3:rule list) = map sort_body rs2 in
  (* let (rs3:rule list) = simplification rs2 in *)
  (* let (out_prog:expr) = {get_empty_expr with rules=rs3; sources=e.sources; query=Some (rule_head (hd rs3))} in
   * let (edb:symtable) = extract_edb out_prog in
   * unfold_query_sql_stt "public" false edb out_prog 
   * (\* ps@[(Pred("abc",[]),[])]@rs1 *\) *)
  rs3
let main () =
  let chan = if !inputf = "" then stdin else open_in !inputf in
  let lexbuf = Lexing.from_channel chan in 
  (* add information of the file name to lexbuf *)
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname =  (if !inputf = "" then "stdin" else !inputf)};
  (* while true do *)
  let (input_ast:Expr2.expr) = Parser.main Lexer.token lexbuf in 
  let (out_put:rule list) = view_update_translation input_ast in
  (* let (out_put:string) = view_update_translation input_ast in *)
  print_endline (fold_right (^) ((map string_of_rule) out_put) "")
  (* print_endline o out_put *)

>>>>>>> d96beeb (introduce files)

(** mainly a call to the above main function *)
let _ =
  try main () with
  (* try small_test () with *)
  (* try test () with *)
  | Error e -> prerr_endline (string_of_error e)
  | SemErr exp -> fprintf stderr "%s\n" ("Semantic error: "^exp); exit 1;
  | Parsing.Parse_error ->  fprintf stderr "%s\n"  "Syntax error"; exit 1;
  | LexErr msg -> fprintf stderr "%s\n"  (msg^":\nError: Illegal characters"); exit 1;
  | ChkErr msg -> fprintf stderr "%s\n"  ("Verification Error: "^msg); exit 1;
<<<<<<< HEAD
  | EnvErr msg -> fprintf stderr "%s\n"  ("Environment Error: "^msg); exit 1;
=======
>>>>>>> d96beeb (introduce files)
  | ParseErr msg -> fprintf stderr "%s\n"  (msg^":\nError: Syntax error"); exit 1;
  | Failure msg -> fprintf stderr "%s\n"  msg; exit 1;
  | Invalid_argument msg -> fprintf stderr "%s\n"  msg; exit 1;
  | Eof -> fprintf stderr "%s\n"  "Lexer.Eof"; exit 1;
  | e -> prerr_endline (Printexc.to_string e)
<<<<<<< HEAD
=======

>>>>>>> d96beeb (introduce files)
