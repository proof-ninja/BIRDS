open Birds
open Utils

type test_case = {
  title : string;
  input : (Expr.rule list) * (Expr.rule list);
  expected : Expr.rule list
}

type failed_body = { expected : string; actual : string }

type test_result =
  | Pass
  | Failed of failed_body

let run_test {input; expected; _} =
  let open ResultMonad in
  let string_of_rules rules =
    rules
      |> List.map Expr.string_of_rule
      |> String.concat "; "
  in
  let (xs, ys) = input in
  let xs = Expr.RuleSet.of_list xs in
  let ys = Expr.RuleSet.of_list ys in
  let actual = Expr.RuleSet.diff xs ys in
  let actual_str = string_of_rules @@ List.of_seq @@ Expr.RuleSet.to_seq actual in
  let expected_str = string_of_rules expected in
  if String.equal actual_str expected_str then
    return Pass
  else
    return (Failed { expected= expected_str; actual= actual_str })

let run_tests (test_cases : test_case list) : bool =
  test_cases |> List.fold_left (fun has_failed test_case ->
    let title = test_case.title in
    match run_test test_case with
    | Ok Pass ->
        Printf.printf "- %s: OK\n" title;
        has_failed

    | Ok (Failed { expected; actual }) ->
        Printf.printf "! %s: FAILED\n" title;
        Printf.printf "expected:\n\"%s\"\n" expected;
        Printf.printf "actual:\n\"%s\"\n" actual;
        true

    | Error error ->
        Printf.printf "! %s: FAILED (%s)\n" title @@ Sql2ast.string_of_error error;
        true
  ) false

let main () =
  let open Expr in
  run_tests [
    {
      title = "Diff sample";
      (*
       * left:
       *   a(X) :- X = 1.
       *   b(X) :- X = 1.
       *   c(X) :- X = 1.
       * right:
       *   a(X) :- X = 1.
       *   b(Y) :- Y = 1.
       *   c(X) :- X = 2.
       *
       * expected:
       *  c(X) :- X.
       *)
      input =
        [
          (Pred ("a", [NamedVar "X"]), [Equat (Equation ("=", Var (NamedVar "X"), Const (Int 1)))]);
          (Pred ("b", [NamedVar "X"]), [Equat (Equation ("=", Var (NamedVar "X"), Const (Int 1)))]);
          (Pred ("c", [NamedVar "X"]), [Equat (Equation ("=", Var (NamedVar "X"), Const (Int 1)))]);
        ],
        [
          (Pred ("a", [NamedVar "X"]), [Equat (Equation ("=", Var (NamedVar "X"), Const (Int 1)))]);
          (Pred ("b", [NamedVar "Y"]), [Equat (Equation ("=", Var (NamedVar "Y"), Const (Int 1)))]);
          (Pred ("c", [NamedVar "X"]), [Equat (Equation ("=", Var (NamedVar "X"), Const (Int 2)))]);
        ];
      expected = [
        (Pred ("c", [NamedVar "X"]), [Equat (Equation ("=", Var (NamedVar "X"), Const (Int 1)))]);
      ]
    };
    {
      title = "To sort body";
      (*
       * left:
       *   f(X, Y) :- X = 1, Y = 2.
       *   g(X, Y) :- X = 1, Y = 1.
       * right:
       *   f(X, Y) :- Y = 2, X = 1.
       *   g(X, Y) :- Y = 2, X = 2.
       *
       * expected:
       *   g(X, Y) :- X = 1, Y = 1.
       *)
      input =
        [
          (Pred ("f", [NamedVar "X"; NamedVar "Y"]), [Equat (Equation ("=", Var (NamedVar "X"), Const (Int 1))); Equat (Equation ("=", Var (NamedVar "Y"), Const (Int 2)))]);
          (Pred ("g", [NamedVar "X"; NamedVar "Y"]), [Equat (Equation ("=", Var (NamedVar "X"), Const (Int 1))); Equat (Equation ("=", Var (NamedVar "Y"), Const (Int 1)))]);
        ],
        [
          (Pred ("f", [NamedVar "X"; NamedVar "Y"]), [Equat (Equation ("=", Var (NamedVar "Y"), Const (Int 2))); Equat (Equation ("=", Var (NamedVar "X"), Const (Int 1)))]);
          (Pred ("g", [NamedVar "Y"; NamedVar "Y"]), [Equat (Equation ("=", Var (NamedVar "Y"), Const (Int 2))); Equat (Equation ("=", Var (NamedVar "X"), Const (Int 2)))]);
        ];
      expected = [
        (Pred ("g", [NamedVar "X"; NamedVar "Y"]), [Equat (Equation ("=", Var (NamedVar "X"), Const (Int 1))); Equat (Equation ("=", Var (NamedVar "Y"), Const (Int 1)))]);
      ]
    }
  ]
