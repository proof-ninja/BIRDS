view tracks1('TRACK':string,'DATE':int,'RATING':int,'ALBUM':string,'QUANTITY':int).

tracks1(T,D,R,A,Q) :- tracks(T,D,R,A), albums(A,Q).

-tracks(T, D, R, A) :- albums(A, V8) , tracks(T, D, R, A) , tracks(V5, V6, V7, A) , -tracks1(T, D, R, A, _) , not -tracks1(V5, V6, V7, A, V8).
-tracks(T, D, R, A) :- tracks(T, D, R, A) , +tracks1(_, _, _, A, _) , -tracks1(T, D, R, A, _).
-tracks(T, D, R, A) :- albums(A, V8) , tracks(T, D, R, A) , tracks(V5, V6, V7, A) , not albums(A, _) , not -tracks1(V5, V6, V7, A, V8).
-tracks(T, D, R, A) :- tracks(T, D, R, A) , +tracks1(_, _, _, A, _) , not albums(A, _).
-tracks(T, D, R, A) :- albums(A, V8) , tracks(T, D, R, A) , tracks(V5, V6, V7, A) , not +tracks1(T, D, R, A, _) , not -tracks1(V5, V6, V7, A, V8).
-tracks(T, D, R, A) :- tracks(T, D, R, A) , +tracks1(_, _, _, A, _) , not +tracks1(T, D, R, A, _).
-albums(A, Q) :- albums(A, Q) , tracks(T, D, R, A) , -tracks1(_, _, _, A, Q) , not -tracks1(T, D, R, A, _).
-albums(A, Q) :- albums(A, Q) , tracks(_, _, _, A) , -tracks1(_, _, _, A, Q).
-albums(A, Q) :- albums(A, Q) , tracks(T, D, R, A) , +tracks1(T, D, R, A, _) , -tracks1(_, _, _, A, Q).

DELETE FROM tracks1 WHERE TRACK='Lovesong'
