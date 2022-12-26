view tracks1('TRACK':string,'DATE':int,'RATING':int,'ALBUM':string,'QUANTITY':int).

tracks1(T, D, R, A, Q) :- tracks(T, D, R, A) , albums(A, Q).
+tracks(TRACK, DATE, RATING, ALBUM) :- +tracks1(TRACK, DATE, RATING, ALBUM, _) , not tracks(TRACK, DATE, RATING, ALBUM).
-tracks(TRACK, DATE, RATING, ALBUM) :- albums(ALBUM, _) , tracks(TRACK, DATE, RATING, ALBUM) , -tracks1(TRACK, DATE, RATING, ALBUM, _).
-tracks(TRACK, DATE, RATING, ALBUM) :- albums(ALBUM, _) , tracks(TRACK, DATE, RATING, ALBUM) , not +tracks1(TRACK, DATE, RATING, ALBUM, _) , not -tracks1(_, _, _, ALBUM, _).
-tracks(TRACK, DATE, RATING, ALBUM) :- tracks(TRACK, DATE, RATING, ALBUM) , +tracks1(_, _, _, ALBUM, _) , -tracks1(TRACK, DATE, RATING, ALBUM, _).
-tracks(TRACK, DATE, RATING, ALBUM) :- tracks(TRACK, DATE, RATING, ALBUM) , +tracks1(_, _, _, ALBUM, _) , not albums(ALBUM, _).
-tracks(TRACK, DATE, RATING, ALBUM) :- tracks(TRACK, DATE, RATING, ALBUM) , +tracks1(_, _, _, ALBUM, _) , not +tracks1(TRACK, DATE, RATING, ALBUM, _).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(TRACK, DATE, RATING, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY) , not albums(ALBUM, _) , not tracks(TRACK, DATE, RATING, ALBUM).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(_, _, _, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY) , not albums(ALBUM, _) , not -tracks1(_, _, _, ALBUM, _).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(_, _, _, ALBUM) , not albums(ALBUM, _) , not +tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(_, _, _, ALBUM, _).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , not albums(ALBUM, _) , not +tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , not albums(ALBUM, _) , not tracks(_, _, _, ALBUM).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(TRACK, DATE, RATING, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY) , -tracks1(_, _, _, ALBUM, _) , not tracks(TRACK, DATE, RATING, ALBUM).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , -tracks1(_, _, _, ALBUM, _) , not +tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , -tracks1(_, _, _, ALBUM, _) , not tracks(_, _, _, ALBUM).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(TRACK, DATE, RATING, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(_, _, _, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(_, _, _, ALBUM, _).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(_, _, _, ALBUM) , not +tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(_, _, _, ALBUM, _).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(_, _, _, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY) , not +tracks1(_, _, _, ALBUM, _) , not -tracks1(_, _, _, ALBUM, _).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(_, _, _, ALBUM) , not +tracks1(_, _, _, ALBUM, QUANTITY) , not +tracks1(_, _, _, ALBUM, _) , not -tracks1(_, _, _, ALBUM, _).
+albums(ALBUM, QUANTITY) :- +tracks1(_, _, _, ALBUM, QUANTITY) , not albums(ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(TRACK, DATE, RATING, ALBUM) , +tracks1(TRACK, DATE, RATING, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(TRACK, DATE, RATING, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY) , not tracks(TRACK, DATE, RATING, ALBUM).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(_, _, _, ALBUM) , +tracks1(_, _, _, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(_, _, _, ALBUM, _).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(_, _, _, ALBUM) , +tracks1(_, _, _, ALBUM, _) , not +tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(_, _, _, ALBUM, _).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , not +tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , not tracks(_, _, _, ALBUM).


INSERT INTO tracks1 VALUES('Mysong', 2018, 10, 'Paris', 20)
