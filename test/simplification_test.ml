open Birds
open Utils
open Expr


type test_case = {
  title    : string;
  input    : rule list * source list;
  expected : rule list;
}

type test_result =
  | Pass
  | Fail of { expected : string; got : string }


let run_test (test_case : test_case) =
  let open ResultMonad in
  let rules, sources = test_case.input in
  match Inlining.sort_rules rules with
  | Error err ->
      return (Fail { expected = "no error when sorting rules."; got = Inlining.string_of_error err })
  | Ok rules ->
    Simplification.simplify rules sources >>= fun got ->
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
      input    = [], [];
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
    }
  ]
