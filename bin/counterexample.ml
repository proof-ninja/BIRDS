open Birds
open Expr


let log = false
let cex_max = 5
let timeout = 180

let parse_property (property: string) =
  match property with
  | "getput" -> Result.Ok Counterexample.Getput
  | "putget" -> Result.Ok Counterexample.Putget
  | "disdelta" -> Result.Ok Counterexample.Disdelta
  | p -> Result.Error ("function gen_counterexample called with an unkown property: " ^ p)
 

let _ =
  if Array.length Sys.argv < 3 then
    print_endline "Invalid arguments. File name must be passed."
  else begin
    let filename = Sys.argv.(2) in
    match parse_property Sys.argv.(1) with
    | Result.Error err ->
      print_endline @@ err
    | Result.Ok property ->
      let chan = open_in filename in
      let lexbuf = Lexing.from_channel chan in
      let ast = Parser.main Lexer.token lexbuf in
      let constr_ast = Utils.constraint2rule ast in
      let error, counterexample = Counterexample.gen_counterexample log property cex_max timeout constr_ast in
      let m = match property with
      | Counterexample.Getput ->
          if (error = "") then ("% Invalidity: The following counterexample shows that getput is not satisfied:\n" ^ string_of_prog {get_empty_expr with facts = counterexample} )
          else "% Fail to generate a counterexample of getput: " ^error
      | Counterexample.Putget ->
        if (error = "") then ("% Invalidity: The following counterexample shows that putget is not satisfied:\n" ^ string_of_prog {get_empty_expr with facts = counterexample})
        else "% Fail to generate a counterexample of putget: " ^error
      | Counterexample.Disdelta ->
        if (error = "") then ("% Invalidity: The following counterexample shows that deltas in the datalog program are not disjoint:\n" ^ (string_of_prog {get_empty_expr with facts = counterexample}) )
        else "% Fail to generate a couterexample of delta disjointness: " ^ error
      in
      print_endline m
  end
