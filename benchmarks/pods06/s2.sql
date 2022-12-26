view tracks1('TRACK':string,'DATE':int,'RATING':int,'ALBUM':string,'QUANTITY':int).

tracks1(T, D, R, A, Q) :- tracks(T, D, R, A) , albums(A, Q).
-tracks(T,D,R,A) :- tracks(T,D,R,A),not tracks1(T,D,R,A,_), tracks1(_,_,_,A,_).


DELETE FROM tracks1 WHERE TRACK='Lovesong'
