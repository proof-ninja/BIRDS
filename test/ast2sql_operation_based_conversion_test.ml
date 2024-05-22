open Birds
open Utils
open Expr


type test_case = {
  title    : string;
  expr     : expr;
  expected : string;
}

type test_result =
  | Pass
  | Fail of { expected : string; got : string }


let run_test (test_case : test_case) : (test_result, Ast2sql.error) result =
  let open ResultMonad in
  let expr = test_case.expr in
  let expected = test_case.expected in

  Ast2sql.convert_expr_to_operation_based_sql expr >>= fun sql_operations ->
  let got = sql_operations |> List.map Ast2sql.stringify_sql_operation |> String.concat " " in

  if String.equal got expected then
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

    | Error err ->
        Printf.printf "! %s: FAILED\n" title;
        Printf.printf "error: %s\n" (Ast2sql.show_error err);
        true
  ) false


let main () =
  let test_cases =
    [
      {
        title =
          "ed and eed";
        expr =
          {
            rules = begin
              let rule1 =
                (* "+eed(E, D) :- ed(E, D), D = 'A', E != 'Joe', ¬eed(E, D)." *)
                Deltainsert ("eed", [ NamedVar "E"; NamedVar "D" ]), [
                  Rel (Pred ("ed", [ NamedVar "E"; NamedVar "D" ]));
                  Equat (Equation ("=", Var (NamedVar "D"), Const (String "'A'")));
                  Equat (Equation ("<>", Var (NamedVar "E"), Const (String "'Joe'")));
                  Not (Pred ("eed", [ NamedVar "E"; NamedVar "D" ]));
                ]
              in
              let rule2 =
                (* "-eed(E, D) :- ed(V1, D), eed(E, D), E = 'Joe', D = 'A', V1 != 'Joe', ¬eed(V1, D)." *)
                Deltadelete ("eed", [ NamedVar "E"; NamedVar "D" ]), [
                  Rel (Pred ("ed", [ NamedVar "V1"; NamedVar "D" ]));
                  Rel (Pred ("eed", [ NamedVar "E"; NamedVar "D" ]));
                  Equat (Equation ("=", Var (NamedVar "E"), Const (String "'Joe'")));
                  Equat (Equation ("=", Var (NamedVar "D"), Const (String "'A'")));
                  Equat (Equation ("<>", Var (NamedVar "V1"), Const (String "'Joe'")));
                  Not (Pred ("eed", [ NamedVar "V1"; NamedVar "D" ]));
                ]
              in
              let rule3 =
                (* "+ed(E, D) :- ed(V1, D), E = 'Joe', D = 'A', V1 != 'Joe', ¬ed(E, D), ¬eed(V1, D)." *)
                Deltainsert ("ed", [ NamedVar "E"; NamedVar "D" ]), [
                  Rel (Pred ("ed", [ NamedVar "V1"; NamedVar "D" ]));
                  Equat (Equation ("=", Var (NamedVar "E"), Const (String "'Joe'")));
                  Equat (Equation ("=", Var (NamedVar "D"), Const (String "'A'")));
                  Equat (Equation ("<>", Var (NamedVar "V1"), Const (String "'Joe'")));
                  Not (Pred ("ed", [ NamedVar "E"; NamedVar "D" ]));
                  Not (Pred ("eed", [ NamedVar "V1"; NamedVar "D" ]));
                ]
              in
              [ rule3; rule2; rule1 ] (* `expr` holds its rules in the reversed order *)
            end;
            facts = [];
            query = None;
            sources = [
              ("ed", [ ("emp_name", Sstring); ("dept_name", Sstring) ]);
              ("eed", [ ("emp_name", Sstring); ("dept_name", Sstring) ]);
            ];
            view = None;
            constraints = [];
            primary_keys = [];
          };
        expected =
          let query1 =
            String.concat " " [
              "SELECT ed_0.emp_name AS emp_name, 'A' AS dept_name FROM ed AS ed_0 WHERE";
              "ed_0.dept_name = 'A' AND ed_0.emp_name <> 'Joe' AND";
              "NOT EXISTS ( SELECT * FROM eed AS t WHERE t.emp_name = ed_0.emp_name AND t.dept_name = 'A' )";
            ]
          in
          let query2 =
            String.concat " " [
              "SELECT 'Joe' AS emp_name, 'A' AS dept_name FROM ed AS ed_0, eed AS eed_1 WHERE";
              "ed_0.dept_name = 'A' AND eed_1.dept_name = 'A' AND eed_1.emp_name = 'Joe' AND ed_0.emp_name <> 'Joe' AND";
              "NOT EXISTS ( SELECT * FROM eed AS t WHERE t.emp_name = ed_0.emp_name AND t.dept_name = 'A' )";
            ]
          in
          let query3 =
            String.concat " " [
              "SELECT 'Joe' AS emp_name, 'A' AS dept_name FROM ed AS ed_0 WHERE";
              "ed_0.dept_name = 'A' AND ed_0.emp_name <> 'Joe' AND";
              "NOT EXISTS ( SELECT * FROM ed AS t WHERE t.emp_name = 'Joe' AND t.dept_name = 'A' ) AND";
              "NOT EXISTS ( SELECT * FROM eed AS t WHERE t.emp_name = ed_0.emp_name AND t.dept_name = 'A' )";
            ]
          in
          String.concat " " [
            Printf.sprintf "CREATE TEMPORARY TABLE temp0 AS %s;" query1;
            Printf.sprintf "CREATE TEMPORARY TABLE temp1 AS %s;" query2;
            Printf.sprintf "CREATE TEMPORARY TABLE temp2 AS %s;" query3;
            "INSERT INTO eed SELECT * FROM temp0;";
            "DELETE FROM eed USING temp1 WHERE eed.emp_name = temp1.emp_name AND eed.dept_name = temp1.dept_name;";
            "INSERT INTO ed SELECT * FROM temp2;";
          ]
      };
      {
        title =
          "non LVGN-Datalog";
        (*
          source a('AA':int, 'BB':string).
          source b('BB':string, 'CC':int).

          v(A,B,C) :- a(A,B), b(B,C).

          -v(GENV1, GENV2, GENV3) :- v(GENV1, GENV2, GENV3) , GENV3 = 3 , GENV1 <> 4.
          +v(GENV1, GENV2, GENV3) :- GENV1 = 4 , -v(GENV1_2, GENV2, GENV3).

          uv(A,B,C) :- v(A, B, C), not -v(A,B,C).
          uv(A,B,C) :- +v(A,B,C).

          -a(A, B) :- a(A, B), not uv(A, B, _).
          -b(B, C) :- b(B, C), uv(_, B, _), not uv(_, B, C).
          +a(A, B) :- uv(A, B, _), not a(A, B).
          +b(B, C) :- uv(_, B, C), not b(B, C).
        *)
        expr =
          {
            rules = begin
              let v = Pred ("v", [ NamedVar "A"; NamedVar "B"; NamedVar "C" ]), [
                Rel (Pred ("a", [ NamedVar "A"; NamedVar "B" ]));
                Rel (Pred ("b", [ NamedVar "B"; NamedVar "C" ]));
              ]
              in
              let mv = Deltadelete ("v", [ NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3" ]), [
                Rel (Pred ("v", [ NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3" ]));
                Equat (Equation ("=", Var (NamedVar "GENV3"), Const (Int 3)));
                Equat (Equation ("<>", Var (NamedVar "GENV1"), Const (Int 4)));
              ]
              in
              let pv = Deltainsert ("v", [ NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3" ]), [
                Equat (Equation ("=", Var (NamedVar "GENV1"), Const (Int 4)));
                Rel (Deltadelete ("v", [ NamedVar "GENV1_2"; NamedVar "GENV2"; NamedVar "GENV3" ]));
              ]
              in
              let uv1 = Pred ("uv", [ NamedVar "A"; NamedVar "B"; NamedVar "C" ]), [
                Rel (Pred ("v", [ NamedVar "A"; NamedVar "B"; NamedVar "C" ]));
                Not (Deltadelete ("v", [ NamedVar "A"; NamedVar "B"; NamedVar "C" ]));
              ]
              in
              let uv2 = Pred ("uv", [ NamedVar "A"; NamedVar "B"; NamedVar "C" ]), [
                Rel (Deltainsert ("v", [ NamedVar "A"; NamedVar "B"; NamedVar "C" ]));
              ]
              in
              let ma = Deltadelete ("a", [ NamedVar "A"; NamedVar "B" ]), [
                Rel (Pred ("a", [ NamedVar "A"; NamedVar "B" ]));
                Not (Pred ("uv", [ NamedVar "A"; NamedVar "B"; AnonVar ]));
              ]
              in
              let mb = Deltadelete ("b", [ NamedVar "B"; NamedVar "C" ]), [
                Rel (Pred ("b", [ NamedVar "B"; NamedVar "C" ]));
                Rel (Pred ("uv", [ AnonVar; NamedVar "B"; AnonVar ]));
                Not (Pred ("uv", [ AnonVar; NamedVar "B"; NamedVar "C" ]));
              ]
              in
              let pa = Deltainsert ("a", [ NamedVar "A"; NamedVar "B" ]), [
                Rel (Pred ("uv", [ NamedVar "A"; NamedVar "B"; AnonVar ]));
                Not (Pred ("a", [ NamedVar "A"; NamedVar "B" ]));
              ]
              in
              let pb = Deltainsert ("b", [ NamedVar "B"; NamedVar "C" ]), [
                Rel (Pred ("uv", [ AnonVar; NamedVar "B"; NamedVar "C" ]));
                Not (Pred ("b", [ NamedVar "B"; NamedVar "C" ]));
              ]
              in
              match Inlining.sort_rules [ v; mv; pv; uv1; uv2; ma; mb; pa; pb ] with
              | Error err ->
                  Printf.printf "Error: %s\n" (Inlining.string_of_error err);
                  assert false
              | Ok rules ->
                match Simplification.simplify rules with
                | Error err ->
                    Printf.printf "Error: %s\n" (Simplification.string_of_error err);
                    assert false
                | Ok rules -> rules
            end;
            facts = [];
            query = None;
            sources = [
              ("a", [ ("AA", Sstring); ("BB", Sint) ]);
              ("b", [ ("BB", Sint); ("CC", Sstring) ]);
            ];
            view = None;
            constraints = [];
            primary_keys = [];
          };
        expected = String.concat " " [
          "CREATE TEMPORARY TABLE v AS SELECT a_0.AA AS AA, a_0.BB AS BB, b_1.CC AS CC FROM a AS a_0, b AS b_1 WHERE b_1.BB = a_0.BB;";
          "CREATE TEMPORARY TABLE temp0 AS SELECT v_0.AA AS AA, v_0.BB AS BB, 3 AS CC FROM v AS v_0 WHERE v_0.CC = 3 AND v_0.AA <> 4;";
          "CREATE TEMPORARY TABLE temp1 AS SELECT 4 AS AA, temp0.BB AS BB, temp0.CC AS CC FROM temp0 AS temp0;";
          "CREATE TEMPORARY TABLE uv AS SELECT v_0.AA AS AA, v_0.BB AS BB, v_0.CC AS CC FROM v AS v_0 WHERE NOT EXISTS ( SELECT * FROM temp0 AS t WHERE t.AA = v_0.AA AND t.BB = v_0.BB AND t.CC = v_0.CC ) UNION SELECT temp1.AA AS AA, temp1.BB AS BB, temp1.CC AS CC FROM temp1 AS temp1;";
          "CREATE TEMPORARY TABLE temp2 AS SELECT uv_0.BB AS BB, uv_0.CC AS CC FROM uv AS uv_0 WHERE NOT EXISTS ( SELECT * FROM b AS t WHERE t.BB = uv_0.BB AND t.CC = uv_0.CC );";
          "CREATE TEMPORARY TABLE temp3 AS SELECT uv_0.AA AS AA, uv_0.BB AS BB FROM uv AS uv_0 WHERE NOT EXISTS ( SELECT * FROM a AS t WHERE t.AA = uv_0.AA AND t.BB = uv_0.BB );";
          "CREATE TEMPORARY TABLE temp4 AS SELECT b_0.BB AS BB, b_0.CC AS CC FROM b AS b_0, uv AS uv_1 WHERE uv_1.BB = b_0.BB AND NOT EXISTS ( SELECT * FROM uv AS t WHERE t.BB = b_0.BB AND t.CC = b_0.CC );";
          "CREATE TEMPORARY TABLE temp5 AS SELECT a_0.AA AS AA, a_0.BB AS BB FROM a AS a_0 WHERE NOT EXISTS ( SELECT * FROM uv AS t WHERE t.AA = a_0.AA AND t.BB = a_0.BB );";
          "INSERT INTO b SELECT * FROM temp2;";
          "INSERT INTO a SELECT * FROM temp3;";
          "DELETE FROM b USING temp4 WHERE b.BB = temp4.BB AND b.CC = temp4.CC;";
          "DELETE FROM a USING temp5 WHERE a.AA = temp5.AA AND a.BB = temp5.BB;";
        ]
      }
    ]
  in
  run_tests test_cases
