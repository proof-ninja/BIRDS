open Birds
open Expr

let parse_args =
  let log = ref false in
  let cex_max = ref 5 in
  let timeout = ref 180 in
  let inputs = ref [] in
  let anon_fun input = inputs := input :: !inputs in
  let usage = "usage: dune exec counterexample [getput|putget|disdelta] [filename]" in
  let speclist = [
    ("--log", Arg.Unit (fun () -> log := true), " Print running information");
    ("-x", Arg.Int (fun d -> cex_max := d), "<size> Get a counterexample with the maximum size if the program is not well-behaved");
    ("--counterexample", Arg.Int (fun d -> cex_max := d), "<size> The same as -x");
    ("-t", Arg.Int (fun d -> timeout := d), "<timeout> Timeout (second) (default: 180s)");
    ("--timeout", Arg.Int (fun d -> timeout := d), "<timeout> The same as -t");
  ] in
  let _ = Arg.parse (Arg.align speclist) anon_fun usage in
  (!log, !cex_max, !timeout, !inputs |> List.rev |> Array.of_list)

let parse_property (property: string) =
  match property with
  | "getput" -> Result.Ok Counterexample.Getput
  | "putget" -> Result.Ok Counterexample.Putget
  | "disdelta" -> Result.Ok Counterexample.Disdelta
  | p -> Result.Error ("function gen_counterexample called with an unkown property: " ^ p)
 
let _ =
  let (log, cex_max, timeout, inputs) = parse_args in
  if Array.length inputs < 2 then
    print_endline "Invalid arguments. File name must be passed."
  else begin
    let _ = Array.iter print_endline inputs in
    let filename = inputs.(1) in
    match parse_property inputs.(0) with
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
