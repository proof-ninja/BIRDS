view ced('E':string, 'D':string).

ced(E, D) :- ed(E, D) , not eed(E, D).
+eed(E, D) :- ed(E, D) , -ced(E, D) , not eed(E, D).
+eed(E, D) :- ed(E, D) , not eed(E, D) , not +ced(E, D).
-eed(E, D) :- +ced(E, D) , eed(E, D).
+ed(E, D) :- +ced(E, D) , not ed(E, D).

DELETE FROM ced WHERE E='Alice'
