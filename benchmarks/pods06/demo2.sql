view tracks1('TRACK':string,'DATE':int,'RATING':int,'ALBUM':string,'QUANTITY':int).


tracks1(T, D, R, A, Q) :- tracks(T, D, R, A) , albums(A, Q).
+tracks(TRACK, DATE, RATING, ALBUM) :- +tracks1(TRACK, DATE, RATING, ALBUM, _) , not tracks(TRACK, DATE, RATING, ALBUM).
-tracks(TRACK, DATE, RATING, ALBUM) :- albums(ALBUM, _) , tracks(TRACK, DATE, RATING, ALBUM) , -tracks1(TRACK, DATE, RATING, ALBUM, _).
-tracks(TRACK, DATE, RATING, ALBUM) :- albums(ALBUM, V34) , tracks(TRACK, DATE, RATING, ALBUM) , tracks(V31, V32, V33, ALBUM) , -tracks1(TRACK, DATE, RATING, ALBUM, _) , not -tracks1(V31, V32, V33, ALBUM, V34).
-tracks(TRACK, DATE, RATING, ALBUM) :- tracks(TRACK, DATE, RATING, ALBUM) , +tracks1(_, _, _, ALBUM, _) , -tracks1(TRACK, DATE, RATING, ALBUM, _).
-tracks(TRACK, DATE, RATING, ALBUM) :- albums(ALBUM, V34) , tracks(TRACK, DATE, RATING, ALBUM) , tracks(V31, V32, V33, ALBUM) , not albums(ALBUM, _) , not -tracks1(V31, V32, V33, ALBUM, V34).
-tracks(TRACK, DATE, RATING, ALBUM) :- tracks(TRACK, DATE, RATING, ALBUM) , +tracks1(_, _, _, ALBUM, _) , not albums(ALBUM, _).
-tracks(TRACK, DATE, RATING, ALBUM) :- albums(ALBUM, V34) , tracks(TRACK, DATE, RATING, ALBUM) , tracks(V31, V32, V33, ALBUM) , not +tracks1(TRACK, DATE, RATING, ALBUM, _) , not -tracks1(V31, V32, V33, ALBUM, V34).
-tracks(TRACK, DATE, RATING, ALBUM) :- tracks(TRACK, DATE, RATING, ALBUM) , +tracks1(_, _, _, ALBUM, _) , not +tracks1(TRACK, DATE, RATING, ALBUM, _).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(TRACK, DATE, RATING, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(TRACK, DATE, RATING, ALBUM, _).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(_, _, _, ALBUM, _) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , not +tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(_, _, _, ALBUM, _) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , not tracks(_, _, _, ALBUM) , not -tracks1(_, _, _, ALBUM, _) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(_, _, _, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , not +tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(TRACK, DATE, RATING, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY) , not albums(ALBUM, _) , not tracks(TRACK, DATE, RATING, ALBUM).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY) , not albums(ALBUM, _) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , not albums(ALBUM, _) , not +tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , not albums(ALBUM, _) , not +tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , not albums(ALBUM, _) , not tracks(_, _, _, ALBUM) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , not albums(ALBUM, _) , not tracks(_, _, _, ALBUM).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(TRACK, DATE, RATING, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY) , -tracks1(_, _, _, ALBUM, _) , not tracks(TRACK, DATE, RATING, ALBUM).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY) , -tracks1(_, _, _, ALBUM, _) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , -tracks1(_, _, _, ALBUM, _) , not +tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , -tracks1(_, _, _, ALBUM, _) , not +tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , -tracks1(_, _, _, ALBUM, _) , not tracks(_, _, _, ALBUM) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , -tracks1(_, _, _, ALBUM, _) , not tracks(_, _, _, ALBUM).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY) , not tracks(_, _, _, ALBUM) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , not tracks(_, _, _, ALBUM) , not +tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , not tracks(_, _, _, ALBUM) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY) , not +tracks1(_, _, _, ALBUM, _) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , not +tracks1(_, _, _, ALBUM, QUANTITY) , not +tracks1(_, _, _, ALBUM, _) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , not tracks(_, _, _, ALBUM) , not +tracks1(_, _, _, ALBUM, _) , not -tracks1(V5, V6, V7, ALBUM, V8).
+albums(ALBUM, QUANTITY) :- +tracks1(_, _, _, ALBUM, QUANTITY) , not albums(ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(TRACK, DATE, RATING, ALBUM) , +tracks1(TRACK, DATE, RATING, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(TRACK, DATE, RATING, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY) , not tracks(TRACK, DATE, RATING, ALBUM).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , +tracks1(_, _, _, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , -tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , +tracks1(_, _, _, ALBUM, _) , not +tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , not +tracks1(_, _, _, ALBUM, QUANTITY).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , albums(ALBUM, V8) , tracks(V5, V6, V7, ALBUM) , +tracks1(_, _, _, ALBUM, _) , not tracks(_, _, _, ALBUM) , not -tracks1(V5, V6, V7, ALBUM, V8).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , +tracks1(_, _, _, ALBUM, _) , not tracks(_, _, _, ALBUM).




DELETE FROM tracks1 WHERE TRACK='Lovesong'

