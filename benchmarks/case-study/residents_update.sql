view residents('ename':string,'bday':string,'gender':string).

residents(E, B, G) :- male(E, B) , G = 'M'.
residents(E, B, G) :- female(E, B) , G = 'F'.
residents(E, B, G) :- others(E, B, G).
+male(E, B) :- +residents(E, B, G) , G = 'M' , not male(E, B) , not others(E, B, G).
-male(E, B) :- male(E, B) , -residents(E, B, G) , G = 'M'.
+female(E, B) :- +residents(E, B, G) , G = 'F' , not female(E, B) , not others(E, B, G).
-female(E, B) :- female(E, B) , -residents(E, B, G) , G = 'F'.
+others(E, B, G) :- +residents(E, B, G) , not G = 'M' , not G = 'F' , not others(E, B, G).
-others(E, B, G) :- others(E, B, G) , -residents(E, B, G).


UPDATE residents SET GENDER='M' WHERE ENAME='Joe'
