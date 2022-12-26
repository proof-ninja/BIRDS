view ced('E':string, 'D':string).

ced(E, D) :- ed(E, D) , not eed(E, D).
+eed(E, D) :- ed(E, D) , -ced(E, D) , not eed(E, D).
-eed(E, D) :- +ced(E, D) , eed(E, D).
+ed(E, D) :- +ced(E, D) , not ed(E, D).

UPDATE ced SET D='R&D' WHERE E='Joe'
