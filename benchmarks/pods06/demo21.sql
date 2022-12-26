view tracks1('TRACK':string,'DATE':int,'RATING':int,'ALBUM':string,'QUANTITY':int).

tracks1(T, D, R, A, Q) :- tracks(T, D, R, A) , albums(A, Q).


-albums(A, Q) :- albums(A, Q) , tracks(T, D, R, A) , -tracks1(_, _, _, A, Q).

DELETE FROM tracks1 WHERE TRACK='Lovesong'


