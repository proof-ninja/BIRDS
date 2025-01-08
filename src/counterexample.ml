(*******************************************************)
(** Functions for debugging a bidirectional Datalog program
*)
(********************************************************)
(*
@author: Vandang Tran
*)

open Logic
open Expr
open Utils

type property = Getput | Putget | Disdelta

let string_of_property (property: property) = match property with
    | Getput -> "getput"
    | Putget -> "putget"
    | Disdelta -> "disdelta"

let init_vocabulary = [
    "'..aa..'";
    "'..bb..'";
    "'..cc..'";
    "'..dd..'";
    "'..ee..'";
    "'..ff..'";
    "'..gg..'";
    "'..hh..'";
    "'..ii..'";
    "'..jj..'";
    "'..kk..'";
    "'..ll..'";
    "'..mm..'";
    "'..nn..'";
    "'..oo..'";
    "'..pp..'";
    "'..qq..'";
    "'..rr..'";
    "'..ss..'";
    "'..tt..'";
    "'..uu..'";
    "'..vv..'";
    "'..ww..'";
    "'..xx..'";
    "'..yy..'";
    "'..zz..'"
]

(** interatively generate a counterexample for a given property *)
let gen_counterexample (log:bool) (property:property) (maxsize:int) (timeout:int) (iprog: expr) =
  let error_mess = ref "" in
  (* let all_rules, non_rules = Rule_preprocess.seperate_rules iprog in *)
  let string_adom = init_vocabulary@(Rule_preprocess.extract_rules_string_adom iprog.rules) in
  let string_adom = Lib.sort (fun a b -> a < b) string_adom in
  let adom_tbl = Hashtbl.create (List.length string_adom) in
  List.iteri (fun i x -> Hashtbl.add adom_tbl x i) string_adom;
  let string_to_int str =
      if Hashtbl.mem adom_tbl str then
          Hashtbl.find adom_tbl str
      else invalid_arg "function string_to_int called with an unkown string" in
  let prog = {iprog with rules = Rule_preprocess.string2int_rules string_to_int iprog.rules} in
  if log then (
      print_endline "______string-mapped program______";
      print_endline @@"String active domain in the program is: " ^ (String.concat ", " string_adom);
      print_endline (string_of_prog prog);
      print_endline "__________________________________"
      );
  let vocsize = List.length string_adom in
  let rec gen_ctex i maxsize =
      let exitcode, mes = check_ros_prog log timeout (
          match property with
          | Getput -> if log then print_endline "==> generating a counterexample for getput"; Ast2ros.ros_check_getput_of_stt log i vocsize prog
          | Putget -> if log then print_endline "==> generating a counterexample for putget"; Ast2ros.ros_check_putget_of_stt log i vocsize prog
          | Disdelta -> if log then print_endline "==> generating a counterexample for delta disjointness"; Ast2ros.ros_check_disdelta_of_stt log i vocsize prog
          ) in
      if not (exitcode=0) then
          if (exitcode = 124) then (error_mess := "Stop generating a counterexample of "^ string_of_property property ^": Timeout"; i,"")
          else
              (error_mess := "Stop generating a counterexample of "^ string_of_property property ^": \nExit code: " ^ string_of_int exitcode
                  ^ (if (log) then "\nError messange: "^ mes else ""); i,"")
      else
          if (!error_mess = "" && mes = "(unsat)" && i < maxsize) then
              gen_ctex (i+1) maxsize
          else
              i,mes in
    let size, message = gen_ctex 1 maxsize in
    if (!error_mess = "") then
      (if log then (print_endline "________Model from rosette______"; print_endline message; print_endline "_____________________");
      if (message = "(unsat)") then error_mess := "there is no counter example of the specified size for "^ string_of_property property;
      let model_data = Ast2ros.parse_ros_models log message in
      let relation_lst = (match property with
              | Getput -> get_source_stts prog
              | Putget -> get_schema_stts prog
              | Disdelta -> get_schema_stts prog
        ) in
      let table_lst = List.map (Ast2ros.instantiate_relation size string_adom model_data) relation_lst in
      let facts = List.concat table_lst in
      if log then (print_endline ("\n________"^ string_of_property property^" counterexample________ ") ;
          print_endline (string_of_prog {get_empty_expr with facts = facts} );
          print_endline "________________");
      !error_mess, facts)
  else !error_mess, []
