open Birds
open Utils

module Sql = Sql.Ast

type test_case = {
  title : string;
  input : Sql.statement * Sql.column_name list;
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
  let (update, columns) = input in
  Sql2ast.to_datalog update columns >>= fun actual ->
  let actual_str = string_of_rules actual in
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
      title = "sample";
      (*
       * SQL:
       *   UPDATE
       *     ced
       *   SET
       *     dname = 'R&D'
       *   WHERE
       *     dname = 'Dev'
       *
       * datalog:
       *   -ced(GENV1, GENV2) :- ced(GENV1, GENV2), GENV2 = 'Dev', GENV2 <> 'R&D'.
       *   +ced(GENV1, GENV2) :- GENV2 = 'R&D', -ced(GENV1, GENV2_2)
       *
       *)
      input = (
        Sql.UpdateSet (
          "ced",
          [(None, "dname"), Sql.Const (String "'R&D'")],
          [[
            Sql.Constraint (
              Sql.Column (None, "dname"),
              Sql.RelEqual,
              Sql.Const (String "'Dev'")
            )
          ]]
        ),
        ["ename"; "dname"]
      );
      expected = [
        (
          Deltadelete ("ced", [NamedVar "GENV1"; NamedVar "GENV2"]),
          [
            Rel (Pred ("ced", [NamedVar "GENV1"; NamedVar "GENV2"]));
            Equat (Equation ("=", (Var (NamedVar "GENV2")), (Var (ConstVar (String "'Dev'")))));
            Equat (Equation ("<>", (Var (NamedVar "GENV2")), (Var (ConstVar (String "'R&D'")))))
          ]
        );
        (
          Deltainsert ("ced", [NamedVar "GENV1"; NamedVar "GENV2"]),
          [
            Equat (Equation ("=", (Var (NamedVar "GENV2")), (Var (ConstVar (String "'R&D'")))));
            Rel (Deltadelete ("ced", [NamedVar "GENV1"; NamedVar "GENV2_2"]));
          ]
        )
      ]
    };
    {
      title = "Update multi columns";
      (*
       * SQL:
       *   UPDATE
       *     t
       *   SET
       *     c1 = 'v1',
       *     c3 = 'v3',
       *     c5 = 'v5'
       *   WHERE
       *         c2 = 'v2'
       *     AND c3 = 'v100'
       *
       * datalog:
       *   -t(GENV1, GENV2, GENV3, GENV4, GENV5, GENV6) :- t(GENV1, GENV2, GENV3, GENV4, GENV5, GENV6), GENV2 = 'v2', GENV3 = 'v100', GENV1 <> 'v1'.
       *   -t(GENV1, GENV2, GENV3, GENV4, GENV5, GENV6) :- t(GENV1, GENV2, GENV3, GENV4, GENV5, GENV6), GENV2 = 'v2', GENV3 = 'v100', GENV3 <> 'v3'.
       *   -t(GENV1, GENV2, GENV3, GENV4, GENV5, GENV6) :- t(GENV1, GENV2, GENV3, GENV4, GENV5, GENV6), GENV2 = 'v2', GENV3 = 'v100', GENV5 <> 'v5'.
       *   +t(GENV1, GENV2, GENV3, GENV4, GENV5, GENV6) :- GENV1 = 'v1', GENV3 = 'v3', GENV5 = 'v5', -t(GENV1_2, GENV2, GENV3_2, GENV4, GENV5_2, GENV6).
       *
       *)
      input = (
        Sql.UpdateSet (
          "t",
          [
            (None, "c1"), Sql.Const (String "'v1'");
            (None, "c3"), Sql.Const (String "'v3'");
            (None, "c5"), Sql.Const (String "'v5'")
          ],
          [[
            Sql.Constraint (
              Sql.Column (None, "c2"),
              Sql.RelEqual,
              Sql.Const (String "'v2'")
            );
            Sql.Constraint (
              Sql.Column (None, "c3"),
              Sql.RelEqual,
              Sql.Const (String "'v100'")
            )
          ]]
        ),
        ["c1"; "c2"; "c3"; "c4"; "c5"; "c6"]
      );
      expected = [
        (
          Deltadelete ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"; NamedVar "GENV6"]),
          [
            Rel (Pred ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"; NamedVar "GENV6"]));
            Equat (Equation ("=", (Var (NamedVar "GENV2")), (Var (ConstVar (String "'v2'")))));
            Equat (Equation ("=", (Var (NamedVar "GENV3")), (Var (ConstVar (String "'v100'")))));
            Equat (Equation ("<>", (Var (NamedVar "GENV1")), (Var (ConstVar (String "'v1'")))))
          ]
        );
        (
          Deltadelete ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"; NamedVar "GENV6"]),
          [
            Rel (Pred ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"; NamedVar "GENV6"]));
            Equat (Equation ("=", (Var (NamedVar "GENV2")), (Var (ConstVar (String "'v2'")))));
            Equat (Equation ("=", (Var (NamedVar "GENV3")), (Var (ConstVar (String "'v100'")))));
            Equat (Equation ("<>", (Var (NamedVar "GENV3")), (Var (ConstVar (String "'v3'")))))
          ]
        );
        (
          Deltadelete ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"; NamedVar "GENV6"]),
          [
            Rel (Pred ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"; NamedVar "GENV6"]));
            Equat (Equation ("=", (Var (NamedVar "GENV2")), (Var (ConstVar (String "'v2'")))));
            Equat (Equation ("=", (Var (NamedVar "GENV3")), (Var (ConstVar (String "'v100'")))));
            Equat (Equation ("<>", (Var (NamedVar "GENV5")), (Var (ConstVar (String "'v5'")))))
          ]
        );
        (
          Deltainsert ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"; NamedVar "GENV6"]),
          [
            Equat (Equation ("=", (Var (NamedVar "GENV1")), (Var (ConstVar (String "'v1'")))));
            Equat (Equation ("=", (Var (NamedVar "GENV3")), (Var (ConstVar (String "'v3'")))));
            Equat (Equation ("=", (Var (NamedVar "GENV5")), (Var (ConstVar (String "'v5'")))));
            Rel (Deltadelete ("t", [NamedVar "GENV1_2"; NamedVar "GENV2"; NamedVar "GENV3_2"; NamedVar "GENV4"; NamedVar "GENV5_2"; NamedVar "GENV6"]));
          ]
        )
      ]
    };
    {
      title = "Use other columns";
      (*
       * SQL:
       *   UPDATE
       *     t
       *   SET
       *     c1 = c2,
       *     c2 = c3
       *
       * datalog:
       *   -t(GENV1, GENV2, GENV3, GENV4) :- t(GENV1, GENV2, GENV3, GENV4), GENV1 <> GENV2.
       *   -t(GENV1, GENV2, GENV3, GENV4) :- t(GENV1, GENV2, GENV3, GENV4), GENV2 <> GENV3.
       *   +t(GENV1, GENV2, GENV3, GENV4) :- GENV1 = GENV2_2, GENV2 = GENV3, -t(GENV1_2, GENV2_2, GENV3, GENV4).
       *
       *)
      input = (
        Sql.UpdateSet (
          "t",
          [
            (None, "c1"), Sql.Column (None, "c2");
            (None, "c2"), Sql.Column (None, "c3")
          ],
          [[]]
        ),
        ["c1"; "c2"; "c3"; "c4"]
      );
      expected = [
        (
          Deltadelete ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"]),
          [
            Rel (Pred ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"]));
            Equat (Equation ("<>", (Var (NamedVar "GENV1")), (Var (NamedVar "GENV2"))))
          ]
        );
        (
          Deltadelete ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"]),
          [
            Rel (Pred ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"]));
            Equat (Equation ("<>", (Var (NamedVar "GENV2")), (Var (NamedVar "GENV3"))))
          ]
        );
        (
          Deltainsert ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"]),
          [
            Equat (Equation ("=", (Var (NamedVar "GENV1")), (Var (NamedVar "GENV2_2"))));
            Equat (Equation ("=", (Var (NamedVar "GENV2")), (Var (NamedVar "GENV3"))));
            Rel (Deltadelete ("t", [NamedVar "GENV1_2"; NamedVar "GENV2_2"; NamedVar "GENV3"; NamedVar "GENV4"]));
          ]
        )
      ]
    };
    {
      title = "Use OR condition.";
      (*
       * SQL:
       *   UPDATE t
       *   SET A = 'a', B = 'b'
       *   WHERE C = 'x' AND D = 'y' OR E = 'z';
       *
       * datalog:
       *   -t(GENV1, GENV2, GENV3, GENV4, GENV5) :- t(GENV1, GENV2, GENV3, GENV4, GENV5), GENV3 = 'x', GENV4 = 'y', GENV1 <> 'a'.
       *   -t(GENV1, GENV2, GENV3, GENV4, GENV5) :- t(GENV1, GENV2, GENV3, GENV4, GENV5), GENV5 = 'z', GENV1 <> 'a'.
       *   -t(GENV1, GENV2, GENV3, GENV4, GENV5) :- t(GENV1, GENV2, GENV3, GENV4, GENV5), GENV3 = 'x', GENV4 = 'y', GENV2 <> 'b'.
       *   -t(GENV1, GENV2, GENV3, GENV4, GENV5) :- t(GENV1, GENV2, GENV3, GENV4, GENV5), GENV5 = 'z', GENV2 <> 'b'.
       *   +t(GENV1, GENV2, GENV3, GENV4, GENV5) :- GENV1 = 'a', GENV2 = 'b', -t(GENV1_2, GENV2_2, GENV3, GENV4, GENV5)
       *
       *)
      input = (
        Sql.UpdateSet (
          "t",
          [
            (None, "A"), Sql.Const (String "'a'");
            (None, "B"), Sql.Const (String "'b'");
          ],
          [
            [
              Sql.Constraint (
                Sql.Column (None, "C"),
                Sql.RelEqual,
                Sql.Const (String "'x'")
              );
              Sql.Constraint (
                Sql.Column (None, "D"),
                Sql.RelEqual,
                Sql.Const (String "'y'")
              )
            ];
            [
              Sql.Constraint (
                Sql.Column (None, "E"),
                Sql.RelEqual,
                Sql.Const (String "'z'")
              )
            ]
          ]
        ),
        ["A"; "B"; "C"; "D"; "E"]
      );
      expected = [
        (
          Deltadelete ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"]),
          [
            Rel (Pred ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"]));
            Equat (Equation ("=", (Var (NamedVar "GENV3")), (Var (ConstVar (String "'x'")))));
            Equat (Equation ("=", (Var (NamedVar "GENV4")), (Var (ConstVar (String "'y'")))));
            Equat (Equation ("<>", (Var (NamedVar "GENV1")), (Var (ConstVar (String "'a'")))))
          ]
        );
        (
          Deltadelete ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"]),
          [
            Rel (Pred ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"]));
            Equat (Equation ("=", (Var (NamedVar "GENV5")), (Var (ConstVar (String "'z'")))));
            Equat (Equation ("<>", (Var (NamedVar "GENV1")), (Var (ConstVar (String "'a'")))))
          ]
        );
        (
          Deltadelete ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"]),
          [
            Rel (Pred ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"]));
            Equat (Equation ("=", (Var (NamedVar "GENV3")), (Var (ConstVar (String "'x'")))));
            Equat (Equation ("=", (Var (NamedVar "GENV4")), (Var (ConstVar (String "'y'")))));
            Equat (Equation ("<>", (Var (NamedVar "GENV2")), (Var (ConstVar (String "'b'")))))
          ]
        );
        (
          Deltadelete ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"]),
          [
            Rel (Pred ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"]));
            Equat (Equation ("=", (Var (NamedVar "GENV5")), (Var (ConstVar (String "'z'")))));
            Equat (Equation ("<>", (Var (NamedVar "GENV2")), (Var (ConstVar (String "'b'")))))
          ]
        );
        (
          Deltainsert ("t", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"]),
          [
            Equat (Equation ("=", (Var (NamedVar "GENV1")), (Var (ConstVar (String "'a'")))));
            Equat (Equation ("=", (Var (NamedVar "GENV2")), (Var (ConstVar (String "'b'")))));
            Rel (Deltadelete ("t", [NamedVar "GENV1_2"; NamedVar "GENV2_2"; NamedVar "GENV3"; NamedVar "GENV4"; NamedVar "GENV5"]));
          ]
        )
      ]
    };
    {
      title = "Basic INSERT";
      (*
       * SQL:
       *   INSERT INTO
       *     ced
       *   VALUES
       *   ( 'A', 'X' ),
       *   ( 'B', 'Y' )
       *
       * datalog:
       *   +ced(NAME, DEPT) :- NAME = 'A', DEPT = 'X'.
       *   +ced(NAME, DEPT) :- NAME = 'B', DEPT = 'Y'.
       *)
      input = (
        Sql.InsertInto (
          "ced",
          [
            [Sql.Const (Sql.String "A"); Sql.Const (Sql.String "X")];
            [Sql.Const (Sql.String "B"); Sql.Const (Sql.String "Y")]
          ]
        ),
        ["NAME"; "DEPT"]
      );
      expected = [
        (
          Deltainsert ("ced", [NamedVar "NAME"; NamedVar "DEPT"]),
          [
            Equat (Equation ("=", (Var (NamedVar "NAME")), (Var (ConstVar (String "A")))));
            Equat (Equation ("=", (Var (NamedVar "DEPT")), (Var (ConstVar (String "X")))));
          ]
        );
        (
          Deltainsert ("ced", [NamedVar "NAME"; NamedVar "DEPT"]),
          [
            Equat (Equation ("=", (Var (NamedVar "NAME")), (Var (ConstVar (String "B")))));
            Equat (Equation ("=", (Var (NamedVar "DEPT")), (Var (ConstVar (String "Y")))));
          ]
        )
      ]
    };
    {
      title = "Basic DELETE";
      (*
       * SQL:
       * DELETE FROM
       *   ced
       * WHERE ID = 1
       * AND DEPT_NAME = 'A'
       * OR ID = 2
       * AND DEPT_NAME = 'B'
       * ;
       *
       * datalog:
       *   -ced(GENV1, GENV2) :- ced(GENV1, GENV2), GENV1 = 1, GENV2 = 'A'.
       *   -ced(GENV1, GENV2) :- ced(GENV1, GENV2), GENV1 = 2, GENV2 = 'B'.
       *)
      input = (
        Sql.DeleteFrom (
          "ced",
          [
            [
              Sql.Constraint (
                Sql.Column (None, "ID"),
                Sql.RelEqual,
                Sql.Const (Int 1)
              );
              Sql.Constraint (
                Sql.Column (None, "DEPT_NAME"),
                Sql.RelEqual,
                Sql.Const (String "'A'")
              )
            ];
            [
              Sql.Constraint (
                Sql.Column (None, "ID"),
                Sql.RelEqual,
                Sql.Const (Int 2)
              );
              Sql.Constraint (
                Sql.Column (None, "DEPT_NAME"),
                Sql.RelEqual,
                Sql.Const (String "'B'")
              )
            ]
          ]
        ),
        ["ID"; "DEPT_NAME"]
      );
      expected = [
        (
          Deltadelete ("ced", [NamedVar "GENV1"; NamedVar "GENV2"]),
          [
            Rel (Pred ("ced", [NamedVar "GENV1"; NamedVar "GENV2"]));
            Equat (Equation ("=", (Var (NamedVar "GENV1")), (Var (ConstVar (Int 1)))));
            Equat (Equation ("=", (Var (NamedVar "GENV2")), (Var (ConstVar (String "'A'")))))
          ]
        );
        (
          Deltadelete ("ced", [NamedVar "GENV1"; NamedVar "GENV2"]),
          [
            Rel (Pred ("ced", [NamedVar "GENV1"; NamedVar "GENV2"]));
            Equat (Equation ("=", (Var (NamedVar "GENV1")), (Var (ConstVar (Int 2)))));
            Equat (Equation ("=", (Var (NamedVar "GENV2")), (Var (ConstVar (String "'B'")))))
          ]
        )
      ]
    }
  ]
