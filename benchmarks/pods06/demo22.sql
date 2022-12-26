view tracks1('TRACK':string,'DATE':int,'RATING':int,'ALBUM':string,'QUANTITY':int).

tracks1(T, D, R, A, Q) :- tracks(T, D, R, A) , albums(A, Q).
-albums(ALBUM, QUANTITY) :- albums(ALBUM, QUANTITY) , tracks(_, _, _, ALBUM) , -tracks1(_, _, _, ALBUM, QUANTITY) , not -tracks1(_, _, _, ALBUM, _).

DELETE FROM tracks1 WHERE TRACK='Lovesong'

