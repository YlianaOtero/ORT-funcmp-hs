module Pruebas where
import Prelude hiding (EQ)
import Circuitos
import Data.List

--Instrucciones
--1) Poner el archivo Circuitos.hs y este mismo (Pruebas.hs) en la misma carpeta.
--2) Abrir este archivo con ghci.
--3) Para cada función fi pedida hay una serie de pruebas pi0,.. pin que pueden 
--   correrse por separado y devuelven True cuando el resultado es correcto.
--4) También hay una prueba pi que verifica todos los casos de cada función fi.
--5) Finalmente hay una prueba pTodo que verifica todas las pruebas de todas las 
--   funciones del entregable.
 

--2 Pruebas inputs

p20 = sort (inputs c0) == [(1,1)]

p21 = sort (inputs c5) == sort [(4,1),(3,1),(1,1),(2,1),(5,1),(7,1),(6,1)]

p22 = sort (inputs c8) == [(1,4)]

p23 = sort (inputs c11) == sort [(3,2),(2,3),(4,1),(7,1),(1,2),(6,1)]

p24 = sort (inputs c14) == sort [(1,11),(4,1),(3,3),(2,5),(5,1),(7,1),(6,3)]

p2 = and[p20,p21,p22,p23,p24]



--3 Pruebas pertenece

p30 = pertenece 3 c1

p31 = pertenece 8 c5 == False

p32 = pertenece 2 c7

p33 = pertenece 2 c11

p34 = pertenece 8 c12 == False

p3 = and[p30,p31,p32,p33,p34]



--4 Pruebas cantInp

p40 = cantInp c1 == 1

p41 = cantInp c2 == 2

p42 = cantInp c8 == 1

p43 = cantInp c11 == 6

p44 = cantInp c13 == 4

p4 = and[p40,p41,p42,p43,p44]



--5 Pruebas cantComp

p50 = cantComp c1 == 1

p51 = cantComp c4 == 1

p52 = cantComp c10 == 2

p53 = cantComp c11 == 9

p54 = cantComp c14 == 20

p5 = and[p50,p51,p52,p53,p54]



--6 Pruebas swapAO

p60 = swapAO c1 == c1

p61 = swapAO c2 == c2

p62 = swapAO c8 == AND[INP 1,INP 1,INP 1,INP 1]

p63 = swapAO c9 == OR[INP 4,INP 2,INP 3,INP 7]

p64 = swapAO c14 == EQ [NOT(AND[swapAO c8,swapAO c5,swapAO c2]),OR[swapAO c7,swapAO c1,swapAO c6],XOR (swapAO c0)(NOT (NOT (swapAO c7)))]

p65 = swapAO c12 == NOT(NOT(XOR (swapAO c11) (AND[OR[(swapAO c7),(swapAO c6)],NOT (swapAO c11)])))

p6 = and[p60,p61,p62,p63,p64,p65]



--7 Pruebas mirror 

p71 = mirror c1 == c1

p72 = mirror c5 == OR[INP 6,INP 7,INP 5,INP 2,INP 1,INP 3,INP 4]

p73 = mirror c7 == OR[XOR(INP 1)(INP 6),XOR(INP 2)(INP 1)]

p74 = mirror c12 == NOT (NOT (XOR (OR [NOT (AND [NOT (OR [XOR (INP 1) (INP 6),XOR (INP 2) (INP 1)]),AND [INP 7,INP 3,INP 2,INP 4],XOR (OR [INP 2]) (NOT (INP 3))]),AND [NOT (OR [NOT (INP 2),INP 2]),OR [XOR (INP 1) (INP 6),XOR (INP 2) (INP 1)]]]) (AND [NOT (OR [XOR (INP 1) (INP 6),XOR (INP 2) (INP 1)]),AND [INP 7,INP 3,INP 2,INP 4],XOR (OR [INP 2]) (NOT (INP 3))])))

p75 = mirror(mirror c12) == c12

p76 = mirror(mirror c14) == c14

p7 = and[p71,p72,p73,p74,p75,p76]



--8 Pruebas output

po0 = output a0 c0 == True

po1 = output a1 c1 == False

po2 = output a1 c2 == True

po3 = output a3 c2 == False 

po4 = output a1 c2 == True

po5 = output a3 c4 == True

po6 = output a4 c5 == False

po7 = output a1 c5 == True

po8 = output a2 c6 == False

po9 = output a0 c7 == True

po10 = output a3 c7 == False

po11 = output a1 c8 == False

po12 = output a3 c8 == True

po13 = output a0 c9 == False

po14 = output a3 c9 == True

po15 = output a1 c10 == False

po16 = output a3 c11 == True

po17 = output a4 c12 == True

po18 = output a4 c13 == True

po19 = output a1 c13 == False

po20 = output a0 c14 == True

po21 = output a3 c14 == False

po = and [po0,po1,po2,po3,po4,po5,po6,po7,po8,po9,po10,po11,po12,po13,po14,po15,po16,po17,po18,po19,po20,po21]



--9 Pruebas minPath

p90 = minPath c0 == 0

p91 = minPath c5 == 1

p92 = minPath c6 == 2

p93 = minPath c12 == 5

p94 = minPath c14 == 2

p9 = and[p90,p91,p92,p93,p94]



--10 Pruebas maxPath

p100 = maxPath c0 == 0

p101 = maxPath c5 == 1

p102 = maxPath c6 == 3

p103 = maxPath c12 == 9

p104 = maxPath c14 == 6

p10 = and[p100,p101,p102,p103,p104]



--11 Pruebas maxGate

p110 = maxGate c0 == 0

p111 = maxGate c3 == 2

p112 = maxGate c5 == 7

p113 = maxGate c9 == 4

p114 = maxGate c14 == 7

p115 = maxGate c1 == 1

p11 = and[p110,p111,p112,p113,p114]



-- Prueba final

pTodo = and[p2,p3,p4,p5,p6,p7,po,p9,p10,p11]











-- Circuitos para las pruebas

c0 = INP 1

c1 = NOT (INP 3)

c2 = XOR (INP 1) (INP 3)

c3 = XOR (INP 3) (INP 1)

c4 = OR [INP 2]

c5 = OR[INP 4,INP 3,INP 1,INP 2,INP 5,INP 7,INP 6]

c6 = NOT(OR[INP 2,NOT(INP 2)])

c7 = OR[XOR(INP 1)(INP 2),XOR(INP 6)(INP 1)]

c8 = OR[INP 1,INP 1,INP 1,INP 1]

c9 = AND[INP 4,INP 2,INP 3,INP 7]

c10 = AND[INP 1,NOT(INP 1)]

c11 = AND[XOR c1 c4,c9,NOT c7]

c12 = NOT(NOT(XOR c11 (OR[AND[c7,c6],NOT c11])))

c13 = EQ[INP 3,INP 6,INP 5,INP 4]

c14 = EQ [NOT(OR[c8,c5,c2]),AND[c7,c1,c6],XOR c0(NOT (NOT c7))]

-- Entradas para las pruebas

a0 :: [(I,Bool)]
a0 = [(1,True),(2,False),(3,True),(4,False),(5,True),(6,True),(7,True)]

a1 :: [(I,Bool)]
a1 = [(1,False),(2,False),(3,True),(4,False),(5,True),(6,True),(7,False)]

a2 :: [(I,Bool)]
a2 = [(1,True),(2,True),(4,True),(5,False),(7,False),(3,True),(6,True)]

a3 :: [(I,Bool)]
a3 = map (\x -> (x,True)) [0..10]

a4 :: [(I,Bool)]
a4 = map (\x -> (x,False)) [0..10]


