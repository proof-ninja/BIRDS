open Birds
open Utils
open Expr


type test_case = {
  title    : string;
  input    : rule list;
  expected : rule list;
}

type test_result =
  | Pass
  | Fail of { expected : string; got : string }


let run_test (test_case : test_case) =
  let open ResultMonad in
  Simplification.simplify test_case.input >>= fun got ->
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
      input    = [];
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
      ];
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
      ];
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
      ];
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
      ];
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
      ];
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
      ];
      expected = [
        (Pred ("f", [NamedVar "X"]), [Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 1))))]);
        (Pred ("g", [NamedVar "X"]), [Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 1))))]);
        (Deltainsert ("f", [NamedVar "X"]), [Rel (Pred ("f", [NamedVar "X"]))]);
        (Deltadelete ("g", [NamedVar "X"]), [Rel (Pred ("g", [NamedVar "X"]))]);
        (Deltainsert ("a", [NamedVar "X"]), [
          Rel (Pred ("n", [NamedVar "X"])); Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 42))))
        ]);
        (Deltadelete ("b", [NamedVar "X"]), [
          Not (Pred ("n", [NamedVar "X"])); Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 42))))
        ]);
        (Deltadelete ("d", [NamedVar "X"]), [ Equat (Equation ("=", (Var (NamedVar "X")), (Const (Int 42)))) ]);
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
      ];
      expected = [
        (Pred ("v", [NamedVar "A"]), [Rel (Pred ("s", [NamedVar "A"; AnonVar]))]);
        (Deltadelete ("v", [NamedVar "GV1"]), [
          Rel (Pred ("v", [NamedVar "GV1"]));
          Equat (Equation ("=", (Var (NamedVar "GV1")), (Const (Int 1))));
        ]);
         (Deltainsert ("v", [NamedVar "GV1"]), [
           Rel (Deltadelete ("v", [AnonVar]));
           Equat (Equation ("=", (Var (NamedVar "GV1")), (Const (Int 4))));
         ]);
         (Deltadelete ("s", [NamedVar "A"; NamedVar "B"]), [
          Rel (Deltadelete ("v", [NamedVar "A"]));
           Rel (Pred ("s", [NamedVar "A"; NamedVar "B"]));
         ]);
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
      ]
    }
  ]
