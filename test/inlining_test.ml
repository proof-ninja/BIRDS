open Birds
open Utils
open Expr


type test_case = {
  title    : string;
  input    : rule list;
  mode     : Inlining.inlining_mode;
  expected : string;
}

type test_result =
  | Pass
  | Fail of { expected : string; got : string }


let run_test (test_case : test_case) : (test_result, Inlining.error) result =
  let open ResultMonad in
  let expected = test_case.expected in

  Inlining.inline_rules test_case.mode test_case.input >>= fun rules_output ->
  let got = rules_output |> List.map string_of_rule |> String.concat "" in

  if String.equal got test_case.expected then
    return Pass
  else
    return (Fail { expected; got })


(* Runs all the test cases in the given list, prints every result,
   and returns whether a failure has occurred. *)
let run_tests (test_cases : test_case list) : bool =
  test_cases |> List.fold_left (fun has_failed test_case ->
    let title = test_case.title in
    match run_test test_case with
    | Ok Pass ->
        Printf.printf "- %s: OK\n" title;
        has_failed

    | Ok (Fail { expected; got }) ->
        Printf.printf "! %s: FAILED\n" title;
        Printf.printf "expected:\n\"%s\"\n" expected;
        Printf.printf "got:\n\"%s\"\n" got;
        true

    | Error e ->
        Printf.printf "! %s: FAILED (%s)\n" title (Inlining.string_of_error e);
        true
  ) false


let make_lines ss =
  ss |> List.map (fun s -> s ^ "\n") |> String.concat ""


let ( !: ) p xs = Pred (p, xs |> List.map (fun x -> NamedVar x))
let ( !+ ) p xs = Deltainsert (p, xs |> List.map (fun x -> NamedVar x))
let ( !- ) p xs = Deltadelete (p, xs |> List.map (fun x -> NamedVar x))


let main () =
  let test_cases =
    [
      {
        title = "inlining the empty program";
        input = [];
        mode = Inlining.All;
        expected = "";
      };
      {
        title = "minimal inlining";
        input = [
          (!+ "foo" [ "X" ], [ Rel (Pred ("bar", [ NamedVar "X" ])) ]);
          (!: "bar" [ "Y" ], [ Rel (Pred ("qux", [ NamedVar "Y" ])) ]);
        ];
        (* Input:
             +foo(X) :- bar(X).
             bar(Y) :- qux(Y). *)
        mode = Inlining.All;
        expected =
          make_lines [
            "+foo(X) :- qux(X).";
            "bar(Y) :- qux(Y).";
          ];
      };
      {
        title = "inlining rules with anonymous variables (1)";
        input = [
          (!+ "foo" [ "X" ], [ Rel (Pred ("bar", [ NamedVar "X"; AnonVar ])) ]);
          (!: "bar" [ "A"; "B" ], [ Rel (Pred ("qux", [ NamedVar "A"; NamedVar "B"; AnonVar ])) ]);
        ];
        (* Input:
            +foo(X) :- bar(X, _).
            bar(A, B) :- qux(A, B, _). *)
        mode = Inlining.All;
        expected =
          make_lines [
            "+foo(X) :- qux(X, GENV1, GENV3).";
            "bar(A, B) :- qux(A, B, GENV2).";
          ];
      };
      {
        title = "inlining rules with anonymous variables (2)";
        input = [
          (!+ "foo" [ "X"; "Y" ], [
            Rel (Pred ("bar", [ NamedVar "X"; AnonVar ]));
            Rel (Pred ("bar", [ NamedVar "Y"; AnonVar ]));
          ]);
          (!: "bar" [ "A"; "B" ], [ Rel (Pred ("qux", [ NamedVar "A"; NamedVar "B"; AnonVar ])) ]);
        ];
        (* Input:
            +foo(X, Y) :- bar(X, _), bar(Y, _).
            bar(A, B) :- qux(A, B, _). *)
        mode = Inlining.All;
        expected =
          make_lines [
            "+foo(X, Y) :- qux(X, GENV1, GENV4) , qux(Y, GENV2, GENV5).";
            "bar(A, B) :- qux(A, B, GENV3).";
          ];
      };
      {
        title = "inlining multiple disjunctive rules";
        input = [
          (!+ "foo" [ "X" ], [ Rel (Pred ("bar", [ NamedVar "X" ])) ]);
          (!: "bar" [ "A" ], [ Rel (Pred ("qux", [ NamedVar "A"; AnonVar ])) ]);
          (!: "bar" [ "B" ], [ Rel (Pred ("thud", [ AnonVar; NamedVar "B" ])) ]);
        ];
        (* Input:
            +foo(X) :- bar(X).
            bar(A) :- qux(A, _).
            bar(B) :- thud(_, B). *)
        mode = Inlining.All;
        expected =
          make_lines [
            "+foo(X) :- qux(X, GENV3).";
            "+foo(X) :- thud(GENV4, X).";
            "bar(A) :- qux(A, GENV1).";
            "bar(B) :- thud(GENV2, B).";
          ];
      };
      {
        title = "inlining rules that contain equations";
        input = [
          (!+ "foo" [ "X" ], [ Rel (Pred ("bar", [ NamedVar "X" ])) ]);
          (!: "bar" [ "B" ], [
            Rel (Pred ("qux", [ NamedVar "A"; NamedVar "B" ]));
            Equat (Equation ("=", Var (NamedVar "A"), Const (Int 42)));
          ]);
        ];
        (* Input:
             +foo(X) :- bar(X).
             bar(B) :- qux(A, B), A = 42. *)
        mode = Inlining.All;
        expected =
          make_lines [
            "+foo(X) :- qux(GENV1, X) , GENV1 = 42.";
            "bar(B) :- qux(A, B) , A = 42.";
          ];
      };
      {
        title = "inlining rules applied with constants";
        input = [
          (!+ "foo" [ "X" ], [ Rel (Pred ("bar", [ NamedVar "X"; ConstVar (Int 42) ])) ]);
          (!: "bar" [ "A"; "B" ], [ Rel (Pred ("qux", [ NamedVar "A"; NamedVar "B"; ConstVar (Int 57) ])) ]);
        ];
        (* Input:
             +foo(X) :- bar(X, 42).
             bar(A, B) :- qux(A, B, 57). *)
        mode = Inlining.All;
        expected =
          make_lines [
            "+foo(X) :- qux(X, 42, 57).";
            "bar(A, B) :- qux(A, B, 57).";
          ];
      };
      {
        title = "inlining rules with boolean in bodies";
        input = [
          (!+ "f" ["X"], [ Rel (Pred ("g", [ NamedVar "X" ])) ]);
          (!: "g" ["Y"], [
            Equat (Equation ("=", Var (NamedVar "Y"), Const (Int 42)));
            ConstTerm true
          ])
        ];
        (* Input:
         *   +f(X) :- g(X).
         *   g(Y) :- Y = 42, true.
         *)
        mode = Inlining.All;
        expected =
          make_lines [
            "+f(X) :- X = 42 , true.";
            "g(Y) :- Y = 42 , true."
          ]
      };
      {
        title = "negative delta predicate";
        input = [
          (!: "cp_v" [], [
            Rel (Deltainsert ("v", [ NamedVar "N"; NamedVar "T"]));
            Equat (Equation ("<>", Var (NamedVar "T"), Const (String "'A'")));
            Equat (Equation ("<>", Var (NamedVar "T"), Const (String "'B'")));
          ]);
          (!+ "a" ["N"], [
            Rel (Deltainsert ("v", [ NamedVar "N"; NamedVar "T"]));
            Not (Pred ("a", [ NamedVar "N" ]));
            Equat (Equation ("=", Var (NamedVar "T"), Const (String "'A'")));
            Not (Pred ("cp_v", []))
          ])
        ];
        (* Input:
         *   cp_v() :- +v(N,T), T <> 'A', T <> 'B'.
         *   +a(N) :- +v(N,T), not a(N), T='A', not cp_v().
         *)
        mode = Inlining.All;
        expected = make_lines [
          "+a(N) :- +v(N, T) , not a(N) , T = 'A' , not cp_v().";
          "cp_v() :- +v(N, T) , T <> 'A' , T <> 'B'."
        ]
      };
    ]
  in
  run_tests test_cases
