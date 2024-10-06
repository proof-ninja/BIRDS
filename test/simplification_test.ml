open Birds
open Utils
open Expr


type test_case = {
  title    : string;
  input    : rule list * view option * source list;
  expected : rule list;
}

type test_result =
  | Pass
  | Fail of { expected : string; got : string }


let run_test (test_case : test_case) =
  let open ResultMonad in
  let rules, view, sources = test_case.input in
  match Inlining.sort_rules rules with
  | Error err ->
      return (Fail { expected = "no error when sorting rules."; got = Inlining.string_of_error err })
  | Ok rules ->
    Simplification.simplify rules view sources >>= fun got ->
    let s_got = got |> List.map string_of_rule |> String.concat "; " in
    let s_expected = test_case.expected |> List.map string_of_rule |> String.concat "; " in
    if String.equal s_got s_expected then
      return Pass
    else
      return (Fail { expected = s_expected; got = s_got })


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

    | Error _ ->
        Printf.printf "! %s: FAILED (error)\n" title;
        true
  ) false


let main () =
  let track = NamedVar "TRACK" in
  let date = NamedVar "DATE" in
  let rating = NamedVar "RATING" in
  let album = NamedVar "ALBUM" in
  let quantity = NamedVar "QUANTITY" in
  run_tests [
    {
      title    = "empty";
      input    = [], None, [];
      expected = [];
    };
    {
      title = "(1)";
      input = [
        (* (1):
          -tracks(TRACK, DATE, RATING, ALBUM) :-
            albums(ALBUM, _),
            albums(ALBUM, V6845),
            tracks(TRACK, DATE, RATING, ALBUM),
            tracks(TRACK, DATE, RATING, ALBUM),
            RATING = 1. *)
        (Deltadelete ("tracks", [ track; date; rating; album ]), [
          Rel (Pred ("albums", [ album; AnonVar ]));
          Rel (Pred ("albums", [ album; NamedVar "V6845" ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Equat (Equation ("=", Var rating, Const (Int 1)));
        ]);
      ],
      None,
      [ "tracks", [] ];
      expected = [
        (* (1) simplified:
          -tracks(TRACK, DATE, RATING, ALBUM) :-
            albums(ALBUM, _),
            tracks(TRACK, DATE, RATING, ALBUM),
            RATING = 1. *)
        (Deltadelete ("tracks", [ track; date; rating; album ]), [
          Rel (Pred ("albums", [ album; AnonVar ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Equat (Equation ("=", Var rating, Const (Int 1)));
        ])
      ];
    };
    {
      title = "(2): erased by contradiction";
      input = [
        (* (2):
          -tracks(TRACK, DATE, RATING, ALBUM) :-
            albums(ALBUM, V34),
            albums(ALBUM, V6846),
            tracks(TRACK, DATE, RATING, ALBUM),
            tracks(V31, V32, V33, ALBUM),
            tracks(TRACK, DATE, RATING, ALBUM),
            RATING = 1,
            not tracks(V31, V32, V33, ALBUM). *)
        (Deltadelete ("tracks", [ track; date; rating; album ]), [
          Rel (Pred ("albums", [ album; NamedVar "V34" ]));
          Rel (Pred ("albums", [ album; NamedVar "V6846" ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Rel (Pred ("tracks", [ NamedVar "V31"; NamedVar "V32"; NamedVar "V33"; album ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Equat (Equation ("=", Var rating, Const (Int 1)));
          Not (Pred ("tracks", [ NamedVar "V31"; NamedVar "V32"; NamedVar "V33"; album ]));
        ]);
      ],
      None,
      [ "tracks", [] ];
      expected = [];
    };
    {
      title = "(7)";
      input = [
        (* (7):
          -albums(ALBUM, QUANTITY) :-
            albums(ALBUM, QUANTITY),
            albums(ALBUM, QUANTITY),
            tracks(_, _, _, ALBUM),
            tracks(V6853, V6854, V6855, ALBUM),
            V6855 = 1. *)
        (Deltadelete ("albums", [ album; quantity ]), [
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("tracks", [ AnonVar; AnonVar; AnonVar; album ]));
          Rel (Pred ("tracks", [ NamedVar "V6853"; NamedVar "V6854"; NamedVar "V6855"; album ]));
          Equat (Equation ("=", Var (NamedVar "V6855"), Const (Int 1)));
        ]);
      ],
      None,
      [ "albums", [] ];
      expected = [
        (* (7) simplified:
          -albums(ALBUM, QUANTITY) :-
            albums(ALBUM, QUANTITY),
            tracks(_, _, V6855, ALBUM),
            V6855 = 1. *)
        (Deltadelete ("albums", [ album; quantity ]), [
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("tracks", [ AnonVar; AnonVar; NamedVar "V6855"; album ]));
          Equat (Equation ("=", Var (NamedVar "V6855"), Const (Int 1)));
        ]);
      ];
    };
    {
      title = "(32)";
      input = [
        (* (32):
          -albums(ALBUM, QUANTITY) :-
            albums(ALBUM, QUANTITY),
            albums(ALBUM, QUANTITY),
            tracks(TRACK, DATE, RATING, ALBUM),
            tracks(V6847, V6848, V6849, ALBUM),
            V6849 = 1,
            not RATING = 1. *)
        (Deltadelete ("albums", [ album; quantity ]), [
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Rel (Pred ("tracks", [ NamedVar "V6847"; NamedVar "V6848"; NamedVar "V6849"; album ]));
          Equat (Equation ("=", Var (NamedVar "V6849"), Const (Int 1)));
          Noneq (Equation ("=", Var rating, Const (Int 1)));
        ]);
      ],
      None,
      [ "albums", [] ];
      expected = [
        (* (32) simplified:
          -albums(ALBUM, QUANTITY) :-
            albums(ALBUM, QUANTITY),
            tracks(_, _, RATING, ALBUM),
            tracks(_, _, V6849, ALBUM),
            not RATING = 1,
            V6849 = 1. *)
        (Deltadelete ("albums", [ album; quantity ]), [
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("tracks", [ AnonVar; AnonVar; rating; album ]));
          Rel (Pred ("tracks", [ AnonVar; AnonVar; NamedVar "V6849"; album ]));
          Noneq (Equation ("=", Var rating, Const (Int 1)));
          Equat (Equation ("=", Var (NamedVar "V6849"), Const (Int 1)));
        ]);
      ];
    };
    {
      title = "Boolean body";
      input = [
        (* Boolean body:
             -eed(E, D) :- false , eed(E, D).
             +ed(E, D) :- false , not ed(E, D).
             +eed(E, D) :- ed(E, D) , ed(E, D) , not eed(E, D) , E = 'Joe' , not eed(E, D).
        *)
        (Deltadelete ("eed", [NamedVar "E"; NamedVar "D"]), [ConstTerm false; Rel (Pred ("eed", [NamedVar "E"; NamedVar "D"]))]);
        (Deltainsert ("ed", [NamedVar "E"; NamedVar "D"]), [ConstTerm false; Not (Pred ("ed", [NamedVar "E"; NamedVar "D"]))]);
        (Deltainsert ("eed", [NamedVar "E"; NamedVar "D"]), [
          Rel (Pred ("ed", [NamedVar "E"; NamedVar "D"]));
          Rel (Pred ("ed", [NamedVar "E"; NamedVar "D"]));
          Not (Pred ("eed", [NamedVar "E"; NamedVar "D"]));
          Equat (Equation ("=", (Var (NamedVar "E")), (Const (String "Joe"))));
          Not (Pred ("eed", [NamedVar "E"; NamedVar "D"]));
        ]);
      ],
      None,
      [ "eed", []; "ed", [] ];
      expected = [
        (* Boolean body simplified:
          +eed(E, D) :- ed(E, D) , not eed(E, D) , E = 'Joe'.
         *)
         (Deltainsert ("eed", [NamedVar "E"; NamedVar "D"]), [
          Rel (Pred ("ed", [NamedVar "E"; NamedVar "D"]));
          Not (Pred ("eed", [NamedVar "E"; NamedVar "D"]));
          Equat (Equation ("=", (Var (NamedVar "E")), (Const (String "Joe"))));
        ]);
      ];
    };
    {
      title = "Unused and undefined rules";
      input = [
        (* Unused and undefined rules:
          f(X) :- X = 1.
          g(X) :- X = 1.
          h(X) :- X = 1.

          +f(X) :- f(X).
          -g(X) :- g(X).

          +a(X) :- n(X), X = 42.
          -b(X) :- NOT n(X), X = 42.
          +c(X) :- +n(X), X = 42.
          -d(X) :- NOT -n(X), X = 42.
         *)
        (Deltadelete ("d", [NamedVar "X"]), [
          Not (Deltadelete ("n", [NamedVar "X"])); Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 42))))
        ]);
        (Deltainsert ("c", [NamedVar "X"]), [
          Rel (Deltainsert ("n", [NamedVar "X"])); Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 42))))
        ]);
        (Deltadelete ("b", [NamedVar "X"]), [
          Not (Pred ("n", [NamedVar "X"])); Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 42))))
        ]);
        (Deltainsert ("a", [NamedVar "X"]), [
          Rel (Pred ("n", [NamedVar "X"])); Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 42))))
        ]);

        (Deltadelete ("g", [NamedVar "X"]), [Rel (Pred ("g", [NamedVar "X"]))]);
        (Deltainsert ("f", [NamedVar "X"]), [Rel (Pred ("f", [NamedVar "X"]))]);

        (Pred ("h", [NamedVar "X"]), [Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 1))))]);
        (Pred ("g", [NamedVar "X"]), [Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 1))))]);
        (Pred ("f", [NamedVar "X"]), [Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 1))))]);
      ],
      None,
      [ "f", []; "g", []; "a", []; "b", []; "c", []; "d", [] ];
      expected = [
        (Deltadelete ("g", [NamedVar "X"]), [Rel (Pred ("g", [NamedVar "X"]))]);
        (Deltainsert ("f", [NamedVar "X"]), [Rel (Pred ("f", [NamedVar "X"]))]);
        (Pred ("g", [NamedVar "X"]), [Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 1))))]);
        (Pred ("f", [NamedVar "X"]), [Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 1))))]);
        (Deltainsert ("a", [NamedVar "X"]), [
          Rel (Pred ("n", [NamedVar "X"])); Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 42))))
        ]);
        (Deltadelete ("d", [NamedVar "X"]), [ Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 42)))) ]);
        (Deltadelete ("b", [NamedVar "X"]), [
          Not (Pred ("n", [NamedVar "X"])); Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 42))))
        ]);
      ];
    };
    {
      title = "anonymous variables in body";
      input = [
        (* anonymous variables in body
          v(A) :- s(A,B).
          -v(GV1) :- v(GV1) , GV1 = 1 , GV1 <> 4.
          +v(GV1) :- GV1 = 4 , -v(V12).
          -s(A,B) :- s(A,B), -v(A).
          +s(A,B) :- +v(A), not s(A,_), not -s(_,_), B=-1.
          +s(A,B) :- +v(A), not s(A,_), -s(_,B).
         *)
          (Deltainsert ("s", [NamedVar "A"; NamedVar "B"]), [
            Rel (Deltainsert ("v", [NamedVar "A"]));
            Not (Pred ("s", [NamedVar "A"; AnonVar]));
            Not (Deltadelete ("s", [AnonVar; NamedVar "B"]))
          ]);
          (Deltainsert ("s", [NamedVar "A"; NamedVar "B"]), [
            Rel (Deltainsert ("v", [NamedVar "A"]));
            Not (Pred ("s", [NamedVar "A"; AnonVar]));
            Not (Deltadelete ("s", [AnonVar; AnonVar]));
            Equat (Equation ("=", (Var (NamedVar "B")), (Const (Int (-1)))))
          ]);
          (Deltadelete ("s", [NamedVar "A"; NamedVar "B"]), [
            Rel (Pred ("s", [NamedVar "A"; NamedVar "B"]));
            Rel (Deltadelete ("v", [NamedVar "A"]))
          ]);
          (Deltainsert ("v", [NamedVar "GV1"]), [
            Equat (Equation ("=", (Var (NamedVar "GV1")), (Const (Int 4))));
            Rel (Deltadelete ("v", [NamedVar "V12"]))
          ]);
          (Deltadelete ("v", [NamedVar "GV1"]), [
            Rel (Pred ("v", [NamedVar "GV1"]));
            Equat (Equation ("=", (Var (NamedVar "GV1")), (Const (Int 1))));
            Equat (Equation ("<>", (Var (NamedVar "GV1")), (Const (Int 4))))
          ]);
          (Pred ("v", [NamedVar "A"]), [Rel (Pred ("s", [NamedVar "A"; NamedVar "B"]))]);
      ],
      None,
      [ "v", []; "s", [] ];
      expected = [
        (Deltainsert ("s", [NamedVar "A"; NamedVar "B"]), [
          Rel (Deltainsert ("v", [NamedVar "A"]));
          Not (Deltadelete ("s", [AnonVar; AnonVar]));
          Not (Pred ("s", [NamedVar "A"; AnonVar]));
          Equat (Equation ("=", (Var (NamedVar "B")), (Const (Int (-1)))))
        ]);
        (Deltainsert ("s", [NamedVar "A"; NamedVar "B"]), [
          Rel (Deltainsert ("v", [NamedVar "A"]));
          Not (Deltadelete ("s", [AnonVar; NamedVar "B"]));
          Not (Pred ("s", [NamedVar "A"; AnonVar]));
        ]);
        (Deltadelete ("s", [NamedVar "A"; NamedVar "B"]), [
         Rel (Deltadelete ("v", [NamedVar "A"]));
          Rel (Pred ("s", [NamedVar "A"; NamedVar "B"]));
        ]);
        (Deltainsert ("v", [NamedVar "GV1"]), [
          Rel (Deltadelete ("v", [AnonVar]));
          Equat (Equation ("=", (Var (NamedVar "GV1")), (Const (Int 4))));
        ]);
        (Deltadelete ("v", [NamedVar "GV1"]), [
          Rel (Pred ("v", [NamedVar "GV1"]));
          Equat (Equation ("=", (Var (NamedVar "GV1")), (Const (Int 1))));
        ]);
        (Pred ("v", [NamedVar "A"]), [Rel (Pred ("s", [NamedVar "A"; AnonVar]))]);
      ]
    };
    {
      title = "removing delta preds that are not assigned to any sources";
      input = [
        (*
        source ps('A':int, 'B':int).
        view pv('A':int).
        -ps(A, B) :- ps(A, B) , ps(A, GENV9) , A = 1 , A <> 4.
        -pv(GENV1) :- ps(GENV1, GENV7) , GENV1 = 1 , GENV1 <> 4.
        +ps(A, B) :- A = 4 , ps(GENV10, GENV11) , GENV10 = 1 , GENV10 <> 4 , not ps(A, GENV1) , ps(GENV2, B) , ps(GENV2, GENV12) , GENV2 = 1 , GENV2 <> 4.
        +ps(A, B) :- A = 4 , ps(GENV13, GENV14) , GENV13 = 1 , GENV13 <> 4 , not ps(A, GENV3) , not -ps(GENV4, GENV5) , B = -100.
        +pv(GENV1) :- GENV1 = 4 , ps(GENV1_2, GENV8) , GENV1_2 = 1 , GENV1_2 <> 4.
        pv(A) :- ps(A, GENV6).
        *)
        (Pred ("pv", [NamedVar "A"]), [Rel (Pred ("ps", [NamedVar "A"; NamedVar "GENV6"]))]);
        (* +pv(GENV1) :- GENV1 = 4 , ps(GENV1_2, GENV8) , GENV1_2 = 1 , GENV1_2 <> 4. *)
        (Deltainsert ("pv", [NamedVar "GENV1"]), [
          (Equat (Equation ("=", Var (NamedVar "GENV1"), Const (Int 4))));
          (Rel (Pred ("ps", [NamedVar "GENV1_2"; NamedVar "GENV8"])));
          (Equat (Equation ("=", Var (NamedVar "GENV1_2"), Const (Int 1))));
          (Equat (Equation ("<>", Var (NamedVar "GENV1_2"), Const (Int 4))));
        ]);
        (* +ps(A, B) :- A = 4 , ps(GENV13, GENV14) , GENV13 = 1 , GENV13 <> 4 , not ps(A, GENV3) , not -ps(GENV4, GENV5) , B = -100. *)
        (Deltainsert ("ps", [NamedVar "A"; NamedVar "B"]), [
          (Equat (Equation ("=", Var (NamedVar "A"), Const (Int 4))));
          (Rel (Pred ("ps", [NamedVar "GENV13"; NamedVar "GENV14"])));
          (Equat (Equation ("=", Var (NamedVar "GENV13"), Const (Int 1))));
          (Equat (Equation ("<>", Var (NamedVar "GENV13"), Const (Int 4))));
          (Not (Pred ("ps", [NamedVar "A"; NamedVar "GENV3"])));
          (Not (Deltadelete ("ps", [NamedVar "GENV4"; NamedVar "GENV5"])));
          (Equat (Equation ("=", Var (NamedVar "B"), Const (Int (-100)))));
        ]);
        (* +ps(A, B) :- A = 4 , ps(GENV10, GENV11) , GENV10 = 1 , GENV10 <> 4 , not ps(A, GENV1) , ps(GENV2, B) , ps(GENV2, GENV12) , GENV2 = 1 , GENV2 <> 4. *)
        (Deltainsert ("ps", [NamedVar "A"; NamedVar "B"]), [
          (Equat (Equation ("=", Var (NamedVar "A"), Const (Int 4))));
          (Rel (Pred ("ps", [NamedVar "GENV10"; NamedVar "GENV11"])));
          (Equat (Equation ("=", Var (NamedVar "GENV10"), Const (Int 1))));
          (Equat (Equation ("<>", Var (NamedVar "GENV10"), Const (Int 4))));
          (Not (Pred ("ps", [NamedVar "A"; NamedVar "GENV1"])));
          (Rel (Pred ("ps", [NamedVar "GENV2"; NamedVar "B"])));
          (Rel (Pred ("ps", [NamedVar "GENV2"; NamedVar "GENV12"])));
          (Equat (Equation ("=", Var (NamedVar "GENV2"), Const (Int 1))));
          (Equat (Equation ("<>", Var (NamedVar "GENV2"), Const (Int 4))));
        ]);
        (* -pv(GENV1) :- ps(GENV1, GENV7) , GENV1 = 1 , GENV1 <> 4. *)
        (Deltadelete ("pv", [NamedVar "GENV1"]), [
          (Rel (Pred ("ps", [NamedVar "GENV1"; NamedVar "GENV7"])));
          (Equat (Equation ("=", Var (NamedVar "GENV1"), Const (Int 1))));
          (Equat (Equation ("<>", Var (NamedVar "GENV1"), Const (Int 4))));
        ]);
        (* -ps(A, B) :- ps(A, B) , ps(A, GENV9) , A = 1 , A <> 4. *)
        (Deltadelete ("ps", [NamedVar "A"; NamedVar "B"]), [
          (Rel (Pred ("ps", [NamedVar "A"; NamedVar "B"])));
          (Rel (Pred ("ps", [NamedVar "A"; NamedVar "GENV9"])));
          (Equat (Equation ("=", Var (NamedVar "A"), Const (Int 1))));
          (Equat (Equation ("<>", Var (NamedVar "A"), Const (Int 4))));
        ]);
      ],
      None,
      ["ps", []];
      expected = [
        (*
        +ps(A, B) :- ps(GENV13, _) , not -ps(_, _) , not ps(A, _) , A = 4 , B = -100 , GENV13 = 1.
        +ps(A, B) :- ps(GENV10, _) , ps(GENV2, B) , not ps(A, _) , A = 4 , GENV10 = 1 , GENV2 = 1.
        -ps(A, B) :- ps(A, B) , A = 1.
        *)
        (Deltainsert ("ps", [NamedVar "A"; NamedVar "B"]), [
          (Rel (Pred ("ps", [NamedVar "GENV13"; AnonVar])));
          (Not (Deltadelete ("ps", [AnonVar; AnonVar])));
          (Not (Pred ("ps", [NamedVar "A"; AnonVar])));
          (Equat (Equation ("=", Var (NamedVar "A"), Const (Int 4))));
          (Equat (Equation ("=", Var (NamedVar "B"), Const (Int (-100)))));
          (Equat (Equation ("=", Var (NamedVar "GENV13"), Const (Int 1))));
        ]);
        (Deltainsert ("ps", [NamedVar "A"; NamedVar "B"]), [
          (Rel (Pred ("ps", [NamedVar "GENV10"; AnonVar])));
          (Rel (Pred ("ps", [NamedVar "GENV2"; NamedVar "B"])));
          (Not (Pred ("ps", [NamedVar "A"; AnonVar])));
          (Equat (Equation ("=", Var (NamedVar "A"), Const (Int 4))));
          (Equat (Equation ("=", Var (NamedVar "GENV10"), Const (Int 1))));
          (Equat (Equation ("=", Var (NamedVar "GENV2"), Const (Int 1))));
        ]);
        (Deltadelete ("ps", [NamedVar "A"; NamedVar "B"]), [
          (Rel (Pred ("ps", [NamedVar "A"; NamedVar "B"])));
          (Equat (Equation ("=", Var (NamedVar "A"), Const (Int 1))));
        ]);
      ]
    };
    {
      title = "leave used delta preds that are not assigned to any sources";
      input = [
        (*
        source b('B':int, 'C':int).
        source a('A':int, 'B':int).
        view v('A':int, 'B':int, 'C':int).
        -a(A, B) :- a(A, B) , not __updated__v(A, B, GENV1).
        -b(B, C) :- b(B, C) , a(GENV2, B) , b(B, GENV3) , not -v(GENV2, B, GENV3) , not __updated__v(GENV4, B, C).
        -b(B, C) :- b(B, C) , +v(GENV2, B, GENV3) , not __updated__v(GENV4, B, C).
        -v(GENV1, GENV2, GENV3) :- a(GENV1, GENV2) , b(GENV2, GENV3) , GENV2 = 30 , GENV2 <> 60.
        +a(A, B) :- a(A, B) , b(B, GENV5) , not -v(A, B, GENV5) , not a(A, B).
        +a(A, B) :- +v(A, B, GENV5) , not a(A, B).
        +b(B, C) :- a(GENV6, B) , b(B, C) , not -v(GENV6, B, C) , not b(B, C).
        +b(B, C) :- +v(GENV6, B, C) , not b(B, C).
        __updated__v(A, B, C) :- a(A, B) , b(B, C) , not -v(A, B, C).
        __updated__v(A, B, C) :- +v(A, B, C).
        v(A, B, C) :- a(A, B) , b(B, C).
        *)
        (Pred ("v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]))
        ]);
        (* __updated__v(A, B, C) :- +v(A, B, C). *)
        (Pred ("__updated__v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]), [
          Rel (Deltainsert ("v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]))
        ]);
        (* __updated__v(A, B, C) :- a(A, B) , b(B, C) , not -v(A, B, C). *)
        (Pred ("__updated__v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Not (Deltadelete ("v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]))
        ]);
        (* +b(B, C) :- +v(GENV6, B, C) , not b(B, C). *)
        (Deltainsert ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Deltainsert ("v", [NamedVar "GENV6"; NamedVar "B"; NamedVar "C"]));
          Not (Pred ("b", [NamedVar "B"; NamedVar "C"]))
        ]);
        (* +b(B, C) :- a(GENV6, B) , b(B, C) , not -v(GENV6, B, C) , not b(B, C). *)
        (Deltainsert ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("a", [NamedVar "GENV6"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Not (Deltadelete ("v", [NamedVar "GENV6"; NamedVar "B"; NamedVar "C"]));
          Not (Pred ("b", [NamedVar "B"; NamedVar "C"]))
        ]);
        (* +a(A, B) :- +v(A, B, GENV5) , not a(A, B). *)
        (Deltainsert ("a", [NamedVar "A"; NamedVar "B"]), [
          Rel (Deltainsert ("v", [NamedVar "A"; NamedVar "B"; NamedVar "GENV5"]));
          Not (Pred ("a", [NamedVar "A"; NamedVar "B"]))
        ]);
        (* +a(A, B) :- a(A, B) , b(B, GENV5) , not -v(A, B, GENV5) , not a(A, B). *)
        (Deltainsert ("a", [NamedVar "A"; NamedVar "B"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "GENV5"]));
          Not (Deltadelete ("v", [NamedVar "A"; NamedVar "B"; NamedVar "GENV5"]));
          Not (Pred ("a", [NamedVar "A"; NamedVar "B"]))
        ]);
        (* -v(GENV1, GENV2, GENV3) :- a(GENV1, GENV2) , b(GENV2, GENV3) , GENV2 = 30 , GENV2 <> 60. *)
        (Deltadelete ("v", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"]), [
          Rel (Pred ("a", [NamedVar "GENV1"; NamedVar "GENV2"]));
          Rel (Pred ("b", [NamedVar "GENV2"; NamedVar "GENV3"]));
          Equat (Equation ("=", Var (NamedVar "GENV2"), Const (Int 30)));
          Equat (Equation ("<>", Var (NamedVar "GENV2"), Const (Int 60)))
        ]);
        (* -b(B, C) :- b(B, C) , +v(GENV2, B, GENV3) , not __updated__v(GENV4, B, C). *)
        (Deltadelete ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Rel (Deltainsert ("v", [NamedVar "GENV2"; NamedVar "B"; NamedVar "GENV3"]));
          Not (Pred ("__updated__v", [NamedVar "GENV4"; NamedVar "B"; NamedVar "C"]))
        ]);
        (* -b(B, C) :- b(B, C) , a(GENV2, B) , b(B, GENV3) , not -v(GENV2, B, GENV3) , not __updated__v(GENV4, B, C). *)
        (Deltadelete ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Rel (Pred ("a", [NamedVar "GENV2"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "GENV3"]));
          Not (Deltadelete ("v", [NamedVar "GENV2"; NamedVar "B"; NamedVar "GENV3"]));
          Not (Pred ("__updated__v", [NamedVar "GENV4"; NamedVar "B"; NamedVar "C"]))
        ]);
        (* -a(A, B) :- a(A, B) , not __updated__v(A, B, GENV1). *)
        (Deltadelete ("a", [NamedVar "A"; NamedVar "B"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Not (Pred ("__updated__v", [NamedVar "A"; NamedVar "B"; NamedVar "GENV1"]))
        ]);
      ],
      None,
      ["a", []; "b", []];
      expected = [
        (*
        source a('A':int, 'B':int).
        source b('B':int, 'C':int).
        view v('A':int, 'B':int, 'C':int).
        -a(A, B) :- a(A, B) , not __updated__v(A, B, _).
        -b(B, C) :- a(GENV2, B) , b(B, C) , b(B, GENV3) , not -v(GENV2, B, GENV3) , not __updated__v(_, B, C).
        __updated__v(A, B, C) :- a(A, B) , b(B, C) , not -v(A, B, C).
        -v(GENV1, GENV2, GENV3) :- a(GENV1, GENV2) , b(GENV2, GENV3) , GENV2 = 30.
        *)
        (Deltadelete ("a", [NamedVar "A"; NamedVar "B"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Not (Pred ("__updated__v", [NamedVar "A"; NamedVar "B"; AnonVar]))
        ]);
        (* -b(B, C) :- a(GENV2, B) , b(B, C) , b(B, GENV3) , not -v(GENV2, B, GENV3) , not __updated__v(_, B, C). *)
        (Deltadelete ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("a", [NamedVar "GENV2"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "GENV3"]));
          Not (Deltadelete ("v", [NamedVar "GENV2"; NamedVar "B"; NamedVar "GENV3"]));
          Not (Pred ("__updated__v", [AnonVar; NamedVar "B"; NamedVar "C"]))
        ]);
        (* __updated__v(A, B, C) :- a(A, B) , b(B, C) , not -v(A, B, C). *)
        (Pred ("__updated__v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Not (Deltadelete ("v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]))
        ]);
        (* -v(GENV1, GENV2, GENV3) :- a(GENV1, GENV2) , b(GENV2, GENV3) , GENV2 = 30. *)
        (Deltadelete ("v", [NamedVar "GENV1"; NamedVar "GENV2"; NamedVar "GENV3"]), [
          Rel (Pred ("a", [NamedVar "GENV1"; NamedVar "GENV2"]));
          Rel (Pred ("b", [NamedVar "GENV2"; NamedVar "GENV3"]));
          Equat (Equation ("=", Var (NamedVar "GENV2"), Const (Int 30)))
        ]);
      ]
    };
    (* --prepare *)
    {
      title = "--prepare option";
      input = [
        (*
        source x('A':int).
        view v('A':int).

        +x(A) :- x(A), v(A), A <> 42.
        -x(A) :- +x(A), -v(A).
        *)
        (Deltainsert ("x", [NamedVar "A"]), [
          Rel (Pred ("x", [NamedVar "A"]));
          Rel (Pred ("v", [NamedVar "A"]));
          Equat (Equation ("<>", Var (NamedVar "A"), Const (Int 42)))
        ]);
        (Deltadelete ("x", [NamedVar "A"]), [
          Rel (Deltainsert ("x", [NamedVar "A"]));
          Rel (Deltadelete ("v", [NamedVar "A"]))
        ]);
      ],
      Some ("v", []),
      ["x", []];
      expected = [
        (*
        -x(A) :- -v(A) , +x(A).
        +x(A) :- v(A) , x(A) , not A = 42.
        *)
        (Deltadelete ("x", [NamedVar "A"]), [
          Rel (Deltadelete ("v", [NamedVar "A"]));
          Rel (Deltainsert ("x", [NamedVar "A"]))
        ]);
        (Deltainsert ("x", [NamedVar "A"]), [
          Rel (Pred ("v", [NamedVar "A"]));
          Rel (Pred ("x", [NamedVar "A"]));
          Noneq (Equation ("=", Var (NamedVar "A"), Const (Int 42)))
        ]);
      ]
    };
    {
      title = "Same input without --prepare option";
      input = [
        (*
        source x('A':int).
        view v('A':int).

        +x(A) :- x(A), v(A), A <> 42.
        -x(A) :- +x(A), -v(A).
        *)
        (Deltainsert ("x", [NamedVar "A"]), [
          Rel (Pred ("x", [NamedVar "A"]));
          Rel (Pred ("v", [NamedVar "A"]));
          Equat (Equation ("<>", Var (NamedVar "A"), Const (Int 42)))
        ]);
        (Deltadelete ("x", [NamedVar "A"]), [
          Rel (Deltainsert ("x", [NamedVar "A"]));
          Rel (Deltadelete ("v", [NamedVar "A"]))
        ]);
      ],
      None,
      ["x", []];
      expected = [
        (*
        +x(A) :- v(A) , x(A) , not A = 42.
        *)
        (Deltainsert ("x", [NamedVar "A"]), [
          Rel (Pred ("v", [NamedVar "A"]));
          Rel (Pred ("x", [NamedVar "A"]));
          Noneq (Equation ("=", Var (NamedVar "A"), Const (Int 42)))
        ]);
      ]
    };
    {
      title = "--prepare option: join";
      input = [
        (*
        source b('B':int, 'C':int).
        source a('A':int, 'B':int).
        view v('A':int, 'B':int, 'C':int).
        -a(A, B) :- a(A, B) , not __updated__v(A, B, GENV1).
        -b(B, C) :- b(B, C) , a(GENV2, B) , b(B, GENV3) , not -v(GENV2, B, GENV3) , not __updated__v(GENV4, B, C).
        -b(B, C) :- b(B, C) , +v(GENV2, B, GENV3) , not __updated__v(GENV4, B, C).
        +a(A, B) :- a(A, B) , b(B, GENV5) , not -v(A, B, GENV5) , not a(A, B).
        +a(A, B) :- +v(A, B, GENV5) , not a(A, B).
        +b(B, C) :- a(GENV6, B) , b(B, C) , not -v(GENV6, B, C) , not b(B, C).
        +b(B, C) :- +v(GENV6, B, C) , not b(B, C).
        __updated__v(A, B, C) :- a(A, B) , b(B, C) , not -v(A, B, C).
        __updated__v(A, B, C) :- +v(A, B, C).
        v(A, B, C) :- a(A, B) , b(B, C).
        *)
        (* -a(A, B) :- a(A, B) , not __updated__v(A, B, GENV1). *)
        (Deltadelete ("a", [NamedVar "A"; NamedVar "B"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Not (Pred ("__updated__v", [NamedVar "A"; NamedVar "B"; NamedVar "GENV1"]))
        ]);
        (* -b(B, C) :- b(B, C) , a(GENV2, B) , b(B, GENV3) , not -v(GENV2, B, GENV3) , not __updated__v(GENV4, B, C). *)
        (Deltadelete ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Rel (Pred ("a", [NamedVar "GENV2"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "GENV3"]));
          Not (Deltadelete ("v", [NamedVar "GENV2"; NamedVar "B"; NamedVar "GENV3"]));
          Not (Pred ("__updated__v", [NamedVar "GENV4"; NamedVar "B"; NamedVar "C"]))
        ]);
        (* -b(B, C) :- b(B, C) , +v(GENV2, B, GENV3) , not __updated__v(GENV4, B, C). *)
        (Deltadelete ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Rel (Deltainsert ("v", [NamedVar "GENV2"; NamedVar "B"; NamedVar "GENV3"]));
          Not (Pred ("__updated__v", [NamedVar "GENV4"; NamedVar "B"; NamedVar "C"]))
        ]);
        (* +a(A, B) :- a(A, B) , b(B, GENV5) , not -v(A, B, GENV5) , not a(A, B). *)
        (Deltainsert ("a", [NamedVar "A"; NamedVar "B"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "GENV5"]));
          Not (Deltadelete ("v", [NamedVar "A"; NamedVar "B"; NamedVar "GENV5"]));
          Not (Pred ("a", [NamedVar "A"; NamedVar "B"]))
        ]);
        (* +a(A, B) :- +v(A, B, GENV5) , not a(A, B). *)
        (Deltainsert ("a", [NamedVar "A"; NamedVar "B"]), [
          Rel (Deltainsert ("v", [NamedVar "A"; NamedVar "B"; NamedVar "GENV5"]));
          Not (Pred ("a", [NamedVar "A"; NamedVar "B"]))
        ]);
        (* +b(B, C) :- a(GENV6, B) , b(B, C) , not -v(GENV6, B, C) , not b(B, C). *)
        (Deltainsert ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("a", [NamedVar "GENV6"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Not (Deltadelete ("v", [NamedVar "GENV6"; NamedVar "B"; NamedVar "C"]));
          Not (Pred ("b", [NamedVar "B"; NamedVar "C"]))
        ]);
        (* +b(B, C) :- +v(GENV6, B, C) , not b(B, C). *)
        (Deltainsert ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Deltainsert ("v", [NamedVar "GENV6"; NamedVar "B"; NamedVar "C"]));
          Not (Pred ("b", [NamedVar "B"; NamedVar "C"]))
        ]);
        (* __updated__v(A, B, C) :- a(A, B) , b(B, C) , not -v(A, B, C). *)
        (Pred ("__updated__v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Not (Deltadelete ("v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]))
        ]);
        (* __updated__v(A, B, C) :- +v(A, B, C). *)
        (Pred ("__updated__v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]), [
          Rel (Deltainsert ("v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]))
        ]);
        (* v(A, B, C) :- a(A, B) , b(B, C). *)
        (Pred ("v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]))
        ]);
      ],
      Some ("v", []),
      ["a", []; "b", []];
      expected = [
        (*
        source a('A':int, 'B':int).
        source b('B':int, 'C':int).
        view v('A':int, 'B':int, 'C':int).
        -a(A, B) :- a(A, B) , not __updated__v(A, B, _).
        -b(B, C) :- +v(_, B, _) , b(B, C) , not __updated__v(_, B, C).
        -b(B, C) :- a(GENV2, B) , b(B, C) , b(B, GENV3) , not -v(GENV2, B, GENV3) , not __updated__v(_, B, C).
        __updated__v(A, B, C) :- +v(A, B, C).
        __updated__v(A, B, C) :- a(A, B) , b(B, C) , not -v(A, B, C).
        +b(B, C) :- +v(_, B, C) , not b(B, C).
        +a(A, B) :- +v(A, B, _) , not a(A, B).
        *)
        (* -a(A, B) :- a(A, B) , not __updated__v(A, B, _). *)
        (Deltadelete ("a", [NamedVar "A"; NamedVar "B"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Not (Pred ("__updated__v", [NamedVar "A"; NamedVar "B"; AnonVar]))
        ]);
        (* -b(B, C) :- +v(_, B, _) , b(B, C) , not __updated__v(_, B, C). *)
        (Deltadelete ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Deltainsert ("v", [AnonVar; NamedVar "B"; AnonVar]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Not (Pred ("__updated__v", [AnonVar; NamedVar "B"; NamedVar "C"]))
        ]);
        (* -b(B, C) :- a(GENV2, B) , b(B, C) , b(B, GENV3) , not -v(GENV2, B, GENV3) , not __updated__v(_, B, C). *)
        (Deltadelete ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("a", [NamedVar "GENV2"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "GENV3"]));
          Not (Deltadelete ("v", [NamedVar "GENV2"; NamedVar "B"; NamedVar "GENV3"]));
          Not (Pred ("__updated__v", [AnonVar; NamedVar "B"; NamedVar "C"]))
        ]);
        (* __updated__v(A, B, C) :- +v(A, B, C). *)
        (Pred ("__updated__v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]), [
          Rel (Deltainsert ("v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]))
        ]);
        (* __updated__v(A, B, C) :- a(A, B) , b(B, C) , not -v(A, B, C). *)
        (Pred ("__updated__v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]), [
          Rel (Pred ("a", [NamedVar "A"; NamedVar "B"]));
          Rel (Pred ("b", [NamedVar "B"; NamedVar "C"]));
          Not (Deltadelete ("v", [NamedVar "A"; NamedVar "B"; NamedVar "C"]))
        ]);
        (* +b(B, C) :- +v(_, B, C) , not b(B, C). *)
        (Deltainsert ("b", [NamedVar "B"; NamedVar "C"]), [
          Rel (Deltainsert ("v", [AnonVar; NamedVar "B"; NamedVar "C"]));
          Not (Pred ("b", [NamedVar "B"; NamedVar "C"]))
        ]);
        (* +a(A, B) :- +v(A, B, _) , not a(A, B). *)
        (Deltainsert ("a", [NamedVar "A"; NamedVar "B"]), [
          Rel (Deltainsert ("v", [NamedVar "A"; NamedVar "B"; AnonVar]));
          Not (Pred ("a", [NamedVar "A"; NamedVar "B"]))
        ]);
      ]
    };
    {
      title = "--prepare option: projection";
      input = [
        (*
        source projs('A':int, 'B':string).
        view projv('A':int).
        -projs(A, B) :- projs(A, B) , -projv(A).
        +projs(A, B) :- projs(A, GENV7) , not -projv(A) , not projs(A, GENV1) , not -projs(GENV2, GENV3) , B = 'a'.
        +projs(A, B) :- projs(A, GENV8) , not -projv(A) , not projs(A, GENV4) , projs(GENV5, B) , -projv(GENV5).
        +projs(A, B) :- +projv(A) , not projs(A, GENV1) , not -projs(GENV2, GENV3) , B = 'a'.
        +projs(A, B) :- +projv(A) , not projs(A, GENV4) , projs(GENV5, B) , -projv(GENV5).
        __updated__projv(A) :- projs(A, GENV6) , not -projv(A).
        __updated__projv(A) :- +projv(A).
        projv(A) :- projs(A, B).
        *)
        (* -projs(A, B) :- projs(A, B) , -projv(A). *)
        (Deltadelete ("projs", [NamedVar "A"; NamedVar "B"]), [
          Rel (Pred ("projs", [NamedVar "A"; NamedVar "B"]));
          Rel (Deltadelete ("projv", [NamedVar "A"]))
        ]);
        (* +projs(A, B) :- projs(A, GENV7) , not -projv(A) , not projs(A, GENV1) , not -projs(GENV2, GENV3) , B = 'a'. *)
        (Deltainsert ("projs", [NamedVar "A"; NamedVar "B"]), [
          Rel (Pred ("projs", [NamedVar "A"; NamedVar "GENV7"]));
          Not (Deltadelete ("projv", [NamedVar "A"]));
          Not (Pred ("projs", [NamedVar "A"; NamedVar "GENV1"]));
          Not (Deltadelete ("projs", [NamedVar "GENV2"; NamedVar "GENV3"]));
          Equat (Equation ("=", Var (NamedVar "B"), Const (String "a")))
        ]);
        (* +projs(A, B) :- projs(A, GENV8) , not -projv(A) , not projs(A, GENV4) , projs(GENV5, B) , -projv(GENV5). *)
        (Deltainsert ("projs", [NamedVar "A"; NamedVar "B"]), [
          Rel (Pred ("projs", [NamedVar "A"; NamedVar "GENV8"]));
          Not (Deltadelete ("projv", [NamedVar "A"]));
          Not (Pred ("projs", [NamedVar "A"; NamedVar "GENV4"]));
          Rel (Pred ("projs", [NamedVar "GENV5"; NamedVar "B"]));
          Rel (Deltadelete ("projv", [NamedVar "GENV5"]))
        ]);
        (* +projs(A, B) :- +projv(A) , not projs(A, GENV1) , not -projs(GENV2, GENV3) , B = 'a'. *)
        (Deltainsert ("projs", [NamedVar "A"; NamedVar "B"]), [
          Rel (Deltainsert ("projv", [NamedVar "A"]));
          Not (Pred ("projs", [NamedVar "A"; NamedVar "GENV1"]));
          Not (Deltadelete ("projs", [NamedVar "GENV2"; NamedVar "GENV3"]));
          Equat (Equation ("=", Var (NamedVar "B"), Const (String "a")))
        ]);
        (* +projs(A, B) :- +projv(A) , not projs(A, GENV4) , projs(GENV5, B) , -projv(GENV5). *)
        (Deltainsert ("projs", [NamedVar "A"; NamedVar "B"]), [
          Rel (Deltainsert ("projv", [NamedVar "A"]));
          Not (Pred ("projs", [NamedVar "A"; NamedVar "GENV4"]));
          Rel (Pred ("projs", [NamedVar "GENV5"; NamedVar "B"]));
          Rel (Deltadelete ("projv", [NamedVar "GENV5"]))
        ]);
        (* __updated__projv(A) :- projs(A, GENV6) , not -projv(A). *)
        (Pred ("__updated__projv", [NamedVar "A"]), [
          Rel (Pred ("projs", [NamedVar "A"; NamedVar "GENV6"]));
          Not (Deltadelete ("projv", [NamedVar "A"]))
        ]);
        (* __updated__projv(A) :- +projv(A). *)
        (Pred ("__updated__projv", [NamedVar "A"]), [
          Rel (Deltainsert ("projv", [NamedVar "A"]))
        ]);
        (* projv(A) :- projs(A, B). *)
        (Pred ("projv", [NamedVar "A"]), [
          Rel (Pred ("projs", [NamedVar "A"; NamedVar "B"]))
        ]);
      ],
      Some ("projv", []),
      ["projs", []];
      expected = [
        (*
        source projs('A':int, 'B':string).
        view projv('A':int).
        +projs(A, B) :- -projv(GENV5) , +projv(A) , projs(GENV5, B) , not projs(A, _).
        +projs(A, B) :- +projv(A) , not -projs(_, _) , not projs(A, _) , B = 'a'.
        -projs(A, B) :- -projv(A) , projs(A, B).
        *)
        (* +projs(A, B) :- -projv(GENV5) , +projv(A) , projs(GENV5, B) , not projs(A, _). *)
        (Deltainsert ("projs", [NamedVar "A"; NamedVar "B"]), [
          Rel (Deltadelete ("projv", [NamedVar "GENV5"]));
          Rel (Deltainsert ("projv", [NamedVar "A"]));
          Rel (Pred ("projs", [NamedVar "GENV5"; NamedVar "B"]));
          Not (Pred ("projs", [NamedVar "A"; AnonVar]))
        ]);
        (* +projs(A, B) :- +projv(A) , not -projs(_, _) , not projs(A, _) , B = 'a'. *)
        (Deltainsert ("projs", [NamedVar "A"; NamedVar "B"]), [
          Rel (Deltainsert ("projv", [NamedVar "A"]));
          Not (Deltadelete ("projs", [AnonVar; AnonVar]));
          Not (Pred ("projs", [NamedVar "A"; AnonVar]));
          Equat (Equation ("=", Var (NamedVar "B"), Const (String "a")))
        ]);
        (* -projs(A, B) :- -projv(A) , projs(A, B). *)
        (Deltadelete ("projs", [NamedVar "A"; NamedVar "B"]), [
          Rel (Deltadelete ("projv", [NamedVar "A"]));
          Rel (Pred ("projs", [NamedVar "A"; NamedVar "B"]))
        ]);
      ]
    };
    {
      title = "--prepare option: union";
      input = [
        (*
        source uniona('NAME':string).
        source unionb('NAME':string).
        source uniono('NAME':string, 'TP':string).
        view unionview('NAME':string, 'TP':string).
        -uniona(N) :- uniona(N) , -unionview(N, T) , T = 'A'.
        -unionb(N) :- unionb(N) , -unionview(N, T) , T = 'B'.
        -uniono(N, T) :- uniono(N, T) , -unionview(N, T).
        +uniona(N) :- +unionview(N, T) , not uniona(N) , T = 'A' , not uniono(N, T).
        +unionb(N) :- +unionview(N, T) , not unionb(N) , T = 'B' , not uniono(N, T).
        +uniono(N, T) :- +unionview(N, T) , not uniono(N, T) , not T = 'A' , not T = 'B'.
        unionview(N, T) :- uniona(N) , T = 'A'.
        unionview(N, T) :- unionb(N) , T = 'B'.
        unionview(N, T) :- uniono(N, T).
        *)
        (* -uniona(N) :- uniona(N) , -unionview(N, T) , T = 'A'. *)
        (Deltadelete ("uniona", [NamedVar "N"]), [
          Rel (Pred ("uniona", [NamedVar "N"]));
          Rel (Deltadelete ("unionview", [NamedVar "N"; NamedVar "T"]));
          Equat (Equation ("=", Var (NamedVar "T"), Const (String "A")))
        ]);
        (* -unionb(N) :- unionb(N) , -unionview(N, T) , T = 'B'. *)
        (Deltadelete ("unionb", [NamedVar "N"]), [
          Rel (Pred ("unionb", [NamedVar "N"]));
          Rel (Deltadelete ("unionview", [NamedVar "N"; NamedVar "T"]));
          Equat (Equation ("=", Var (NamedVar "T"), Const (String "B")))
        ]);
        (* -uniono(N, T) :- uniono(N, T) , -unionview(N, T). *)
        (Deltadelete ("uniono", [NamedVar "N"; NamedVar "T"]), [
          Rel (Pred ("uniono", [NamedVar "N"; NamedVar "T"]));
          Rel (Deltadelete ("unionview", [NamedVar "N"; NamedVar "T"]))
        ]);
        (* +uniona(N) :- +unionview(N, T) , not uniona(N) , T = 'A' , not uniono(N, T). *)
        (Deltainsert ("uniona", [NamedVar "N"]), [
          Rel (Deltainsert ("unionview", [NamedVar "N"; NamedVar "T"]));
          Not (Pred ("uniona", [NamedVar "N"]));
          Equat (Equation ("=", Var (NamedVar "T"), Const (String "A")));
          Not (Pred ("uniono", [NamedVar "N"; NamedVar "T"]))
        ]);
        (* +unionb(N) :- +unionview(N, T) , not unionb(N) , T = 'B' , not uniono(N, T). *)
        (Deltainsert ("unionb", [NamedVar "N"]), [
          Rel (Deltainsert ("unionview", [NamedVar "N"; NamedVar "T"]));
          Not (Pred ("unionb", [NamedVar "N"]));
          Equat (Equation ("=", Var (NamedVar "T"), Const (String "B")));
          Not (Pred ("uniono", [NamedVar "N"; NamedVar "T"]))
        ]);
        (* +uniono(N, T) :- +unionview(N, T) , not uniono(N, T) , not T = 'A' , not T = 'B'. *)
        (Deltainsert ("uniono", [NamedVar "N"; NamedVar "T"]), [
          Rel (Deltainsert ("unionview", [NamedVar "N"; NamedVar "T"]));
          Not (Pred ("uniono", [NamedVar "N"; NamedVar "T"]));
          Noneq (Equation ("=", Var (NamedVar "T"), Const (String "A")));
          Noneq (Equation ("=", Var (NamedVar "T"), Const (String "B")))
        ]);
        (* unionview(N, T) :- uniona(N) , T = 'A'. *)
        (Pred ("unionview", [NamedVar "N"; NamedVar "T"]), [
          Rel (Pred ("uniona", [NamedVar "N"]));
          Equat (Equation ("=", Var (NamedVar "T"), Const (String "A")))
        ]);
        (* unionview(N, T) :- unionb(N) , T = 'B'. *)
        (Pred ("unionview", [NamedVar "N"; NamedVar "T"]), [
          Rel (Pred ("unionb", [NamedVar "N"]));
          Equat (Equation ("=", Var (NamedVar "T"), Const (String "B")))
        ]);
        (* unionview(N, T) :- uniono(N, T). *)
        (Pred ("unionview", [NamedVar "N"; NamedVar "T"]), [
          Rel (Pred ("uniono", [NamedVar "N"; NamedVar "T"]))
        ]);
      ],
      Some ("unionview", []),
      ["uniona", []; "unionb", []; "uniono", []];
      expected = [
        (*
        source uniono('NAME':string, 'TP':string).
        source unionb('NAME':string).
        source uniona('NAME':string).
        view unionview('NAME':string, 'TP':string).
        +uniono(N, T) :- +unionview(N, T) , not uniono(N, T) , not T = 'A' , not T = 'B'.
        +unionb(N) :- +unionview(N, T) , not unionb(N) , not uniono(N, T) , T = 'B'.
        +uniona(N) :- +unionview(N, T) , not uniona(N) , not uniono(N, T) , T = 'A'.
        -uniono(N, T) :- -unionview(N, T) , uniono(N, T).
        -unionb(N) :- -unionview(N, T) , unionb(N) , T = 'B'.
        -uniona(N) :- -unionview(N, T) , uniona(N) , T = 'A'.
        *)
        (* +uniono(N, T) :- +unionview(N, T) , not uniono(N, T) , not T = 'A' , not T = 'B'. *)
        (Deltainsert ("uniono", [NamedVar "N"; NamedVar "T"]), [
          Rel (Deltainsert ("unionview", [NamedVar "N"; NamedVar "T"]));
          Not (Pred ("uniono", [NamedVar "N"; NamedVar "T"]));
          Noneq (Equation ("=", Var (NamedVar "T"), Const (String "A")));
          Noneq (Equation ("=", Var (NamedVar "T"), Const (String "B")))
        ]);
        (* +unionb(N) :- +unionview(N, T) , not unionb(N) , not uniono(N, T) , T = 'B'. *)
        (Deltainsert ("unionb", [NamedVar "N"]), [
          Rel (Deltainsert ("unionview", [NamedVar "N"; NamedVar "T"]));
          Not (Pred ("unionb", [NamedVar "N"]));
          Not (Pred ("uniono", [NamedVar "N"; NamedVar "T"]));
          Equat (Equation ("=", Var (NamedVar "T"), Const (String "B")))
        ]);
        (* +uniona(N) :- +unionview(N, T) , not uniona(N) , not uniono(N, T) , T = 'A'. *)
        (Deltainsert ("uniona", [NamedVar "N"]), [
          Rel (Deltainsert ("unionview", [NamedVar "N"; NamedVar "T"]));
          Not (Pred ("uniona", [NamedVar "N"]));
          Not (Pred ("uniono", [NamedVar "N"; NamedVar "T"]));
          Equat (Equation ("=", Var (NamedVar "T"), Const (String "A")))
        ]);
        (* -uniono(N, T) :- -unionview(N, T) , uniono(N, T). *)
        (Deltadelete ("uniono", [NamedVar "N"; NamedVar "T"]), [
          Rel (Deltadelete ("unionview", [NamedVar "N"; NamedVar "T"]));
          Rel (Pred ("uniono", [NamedVar "N"; NamedVar "T"]))
        ]);
        (* -unionb(N) :- -unionview(N, T) , unionb(N) , T = 'B'. *)
        (Deltadelete ("unionb", [NamedVar "N"]), [
          Rel (Deltadelete ("unionview", [NamedVar "N"; NamedVar "T"]));
          Rel (Pred ("unionb", [NamedVar "N"]));
          Equat (Equation ("=", Var (NamedVar "T"), Const (String "B")))
        ]);
        (* -uniona(N) :- -unionview(N, T) , uniona(N) , T = 'A'. *)
        (Deltadelete ("uniona", [NamedVar "N"]), [
          Rel (Deltadelete ("unionview", [NamedVar "N"; NamedVar "T"]));
          Rel (Pred ("uniona", [NamedVar "N"]));
          Equat (Equation ("=", Var (NamedVar "T"), Const (String "A")))
        ]);
      ]
    }
  ]
