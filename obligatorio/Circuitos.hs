{-#LANGUAGE GADTs#-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Circuitos where
import Prelude hiding (EQ)

type I = Int

data CIRC where { INP :: I -> CIRC;
				  NOT :: CIRC -> CIRC;
				  XOR :: CIRC -> CIRC -> CIRC; 
				  OR  :: [CIRC] -> CIRC; 
				  AND :: [CIRC] -> CIRC;  
				  EQ  :: [CIRC] -> CIRC}  
				  deriving (Eq,Show)

--1
c :: CIRC
c = XOR (NOT (EQ [INP 1, INP 2, INP 2, INP 5])) (OR ([AND [INP 4, INP 7, INP 2], INP 1]))

--2
inputs :: CIRC -> [(I,Int)]
inputs = \c -> borrarRepetidosOrd (reverse (armarPar (insertSort (listarTodo c))));

--Recibe una lista, y devuelve una lista de pares donde el primero es un elemento de
--la lista recibida por parámetro, y el segundo es la cantidad de veces que aparece
--el elemento en el resto de la lista.
--Ejemplo: armarPar [1,2,2,3,4] = [(1,1),(2,2),(2,1),(3,1),(4,1)]
armarPar :: [Int] -> [(I,Int)]
armarPar = \l -> case l of {
	[] -> [];
	x:xs -> (x, contarOcurrencias l x):(armarPar xs);
}

--Recibe una lista de pares (fst, snd) que debe estar ordenada, y devuelve la lista sin elementos fst repetidos.
--Ejemplo: borrarRepetidosOrd [(1,1),(2,2),(2,1),(3,1),(4,1)] = [(1,1),(2,1),(3,1),(4,1)]
borrarRepetidosOrd :: [(I,Int)] -> [(I,Int)]
borrarRepetidosOrd = \l -> case l of {
	[] -> [];
	(fstP, sndP):ps -> case ps of {
		[] -> [(fstP, sndP)];
		(fstQ, sndQ):qs -> case (fstP == fstQ) of {
			True -> borrarRepetidosOrd ps;
			False -> (fstP, sndP):(borrarRepetidosOrd ps);
		}
	}
}

--Recibe una lista de enteros y un entero puntual, y devuelve la cantidad de veces que aparece
--ese entero en la lista.
--Ejemplo: contarOcurrencias [1,2,3,2,2,2,4,5,6] 2 = 4
contarOcurrencias :: [Int] -> Int -> Int
contarOcurrencias = \l -> \n -> case l of {
	[] -> 0;
	x:xs -> case (x == n) of {
		True -> 1 + (contarOcurrencias xs n);
		False -> (contarOcurrencias xs n);
	}
}

--Función vista en clase de teórico. Es auxiliar de insertSort.
--Recibe un elemento de algun tipo ordenable y una lista del mismo tipo, y devuelve una lista
--con los mismos elementos de la lista que se le pasó por parámetro y, además, el nuevo elemento
--insertado ordenadamente.
--Ejemplo: insertarOrd 5 [1,1,3,2,2,2,4,7,6] = [1,1,3,2,2,2,4,5,7,6]
insertarOrd :: Ord a => a -> [a] -> [a]
insertarOrd = \e -> \l -> case l of {
     [] -> [e];
      x:xs -> case (x > e) of {
          True -> e:x:xs;
          False -> x:(insertarOrd e xs);
      }
}

--Función vista en clase de teórico.
--Recibe una lista de algun tipo ordenable, y devuelve la lista ordenada.
--Ejemplo: insertSort [1,2,3,2,2,2,4,5,6] = [1,2,2,2,2,3,4,5,6]
insertSort :: Ord a => [a] -> [a]
insertSort = \l -> case l of {
	[] -> [];
	x:xs ->  (insertarOrd x (insertSort xs));
}

--Recibe un circuito y devuelve una lista de enteros con todas sus entradas.
--Ejemplo: listarTodo c = [1,2,2,5,4,7,2,1]
listarTodo :: CIRC -> [Int]
listarTodo = \c -> case c of {
	INP i -> [i];
	NOT c1 -> listarTodo c1;
	XOR c1 c2 -> (listarTodo c1) ++ (listarTodo c2);
	OR c1 -> case c1 of {
		[] -> [];
		x:xs -> (listarTodo x) ++ (listarTodo (OR xs));
	};
	AND c1 -> case c1 of {
		[] -> [];
		x:xs -> (listarTodo x) ++ (listarTodo (AND xs));
	};
    EQ c1 -> case c1 of {
		[] -> [];
		x:xs -> (listarTodo x) ++ (listarTodo (EQ xs));
	}
}

--3
pertenece :: I -> CIRC -> Bool
pertenece = \i -> \c -> case c of {
	INP n -> i == n;
	NOT c1 -> pertenece i c1;
	XOR c1 c2 -> (pertenece i c1) || (pertenece i c2);
	OR c1 -> case c1 of {
		[] -> False;
		x:xs -> (pertenece i x) || (pertenece i (OR xs));
	};
	AND lc1 -> case lc1 of {
		[] -> False;
		y:ys -> (pertenece i y) || (pertenece i (AND ys));
	};
	EQ lc1 -> case 	lc1 of {
		[] -> False;
		z:zs -> (pertenece i z) || (pertenece i (EQ zs));
	}
} 

--4
cantInp :: CIRC -> Int
cantInp = \c -> len(inputs c);

--Recibe una lista de cualquier tipo y devuelve el largo de ella.
--Ejemplo: len [1,2,2,5,4,7,2,1] = 8
len :: [a] -> Int 
len = \l -> case l of {
	[] -> 0;
	x:xs -> 1 + len(xs);
}

--5
cantComp :: CIRC -> Int 
cantComp = \c -> case c of {
	INP i -> 0;
	NOT c1 -> 1 + (cantComp c1);
	XOR c1 c2 -> 1 + (cantComp c1) + (cantComp c2);
	OR c1 -> case c1 of {
		[] -> 1;
		x:xs -> (cantComp x) + (cantComp (OR xs));
	};
	AND c1 -> case c1 of {
		[] -> 1;
		x:xs -> (cantComp x) + (cantComp (AND xs));
	};
	EQ c1 -> case c1 of {
		[] -> 1;
		x:xs -> (cantComp x) + (cantComp (EQ xs));
	}
}

--6
swapAO :: CIRC -> CIRC
swapAO = \c -> case c of {
	INP i -> INP i;
	NOT c1 -> NOT (swapAO c1);
	XOR c1 c2 -> XOR (swapAO c1) (swapAO c2);
	OR c1 -> case c1 of {
		[] -> AND c1;
		x:xs -> AND ((swapAO x):(map swapAO xs));
	};
	AND c1 -> case c1 of {
		[] -> OR c1;
		x:xs -> OR ((swapAO x):(map swapAO xs));
	};
	EQ c1 -> case c1 of {
		[] -> EQ c1;
		x:xs -> EQ ((swapAO x):(map swapAO xs));
	};
}

--7
mirror :: CIRC -> CIRC
mirror = \c -> case c of {
    INP i -> INP i;
    NOT c1 -> NOT (mirror c1);
    XOR c1 c2 -> XOR (mirror c2) (mirror c1);
    OR c1 -> case c1 of {
        [] -> OR c1;
        x:[] -> OR [mirror x];
        x:xs -> case x of {
            INP i -> OR (reverse c1);
            NOT c2 -> OR (reverse ((NOT (mirror c2)):(map mirror xs)));
            XOR c2 c3 -> OR (reverse ((XOR (mirror c3) (mirror c2)):(map mirror xs)));
            _ -> OR (reverse ((mirror x):(map mirror xs)));
        };
    };
    AND c1 -> case c1 of {
        [] -> AND c1;
        x:[] -> AND [mirror x];
        x:xs -> case x of {
            INP i -> AND (reverse c1);
            NOT c2 -> AND (reverse ((NOT (mirror c2)):(map mirror xs)));
            XOR c2 c3 -> AND (reverse ((XOR (mirror c3) (mirror c2)):(map mirror xs)));
            _ -> AND (reverse ((mirror x):(map mirror xs)));
        };
    };
    EQ c1 -> case c1 of {
        [] -> EQ c1;
        x:[] -> EQ [mirror x];
        x:xs -> case x of {
            INP i -> EQ (reverse c1);
            NOT c2 -> EQ (reverse ((NOT (mirror c2)):(map mirror xs)));
            XOR c2 c3 -> EQ (reverse ((XOR (mirror c3) (mirror c2)):(map mirror xs)));
            _ -> EQ (reverse ((mirror x):(map mirror xs)));
        };
    };
}		

--8
output :: [(I,Bool)] -> CIRC -> Bool
output = \l -> \c -> case (todosPertenecen l (listarTodo c)) of {
	True -> outputAux l c;
	False -> error "El circuito tiene elementos que no pertenecen a la lista.";
};

--Recibe una lista de pares (fst, snd), donde fst es un entero, y snd es un booleano. También recibe una lista
--de enteros. Devuelve True en caso de que todos los elementos de la lista de enteros aparezcan como algun fst de la lista
--de pares, y False en caso contrario (es decir, en caso de que algun entero de la lista de enteros, no sea el primer elemento
--de ningun par de la lista de pares).
--Ejemplo: todosPertenecen [(1,True),(2,False),(3,False),(4,True)] [1,2,3,4,5] = False
todosPertenecen :: [(I,Bool)] -> [Int] -> Bool
todosPertenecen = \pares -> \inputs -> case inputs of {
	[] -> True;
	x:xs -> case (elem (x, True) pares || elem (x, False) pares) of {
		True -> todosPertenecen pares xs;
		False -> False;
	}
}

--Esta función realiza lo que debería hacer output si todos los elementos del circuito que se
--le pasa a output por parámetro tienen su correspondiente par en la lista de pares.
--Es decir, es output pero con la pre-condición de que todas las entradas del circuito estan en
--la lista.
outputAux :: [(I,Bool)] -> CIRC -> Bool
outputAux = \l -> \c -> case c of {
	INP i -> valorCorresp l i;
	NOT c1 -> not (outputAux l c1);
	XOR c1 c2 -> (outputAux l c1) /= (outputAux l c2);
	OR c1 -> case c1 of {
		[] -> False;
		x:[] -> outputAux l x;
		x:xs -> case (outputAux l x) of {
			True -> True;
			False -> (outputAux l (OR xs));
		}
	};
	AND c1 -> case c1 of {
		[] -> True;
		x:[] -> outputAux l x;
		x:xs -> case (outputAux l x) of {
			True -> (outputAux l (AND xs));
			False -> False;
		}
	}; 
	EQ c1 -> case c1 of {
		[] -> True;
		x:[] -> (outputAux l x);
		x:xs -> ((not (outputAux l x)) && todosNegativos l (EQ xs)) || ((outputAux l x) && todosPositivos l (EQ xs));
	};
}

--Recibe la lista de pares con los valores de las entradas y si tienen o no señal, y un circuito,
--y devuelve True en caso de que todos los elementos del circuito no tengan señal, y False si al menos
--uno tiene señal.
--Ejemplo: todosNegativos a0 c4 = True
--Ejemplo: todosNegativos a0 c2 = False
todosNegativos :: [(I,Bool)] -> CIRC -> Bool
todosNegativos = \l -> \c -> case c of {
	INP i -> outputAux l c;
	NOT c1 -> outputAux l c;
	XOR c1 c2 -> outputAux l c;
	OR c1-> case c1 of {
		[] -> False;
		x:xs -> not (or ((outputAux l x):[outputAux l (OR xs)]));
	};	
	AND c1 -> case c1 of {
		[] -> True;
		x:xs -> not (or ((outputAux l x):[outputAux l (AND xs)]));
	};	 
	EQ c1 -> case c1 of {
		[] -> True;
		x:xs -> not (or ((outputAux l x):[outputAux l (EQ xs)]));
	};	
} 

--Recibe la lista de pares con los valores de las entradas y si tienen o no señal, y un circuito,
--y devuelve True en caso de que todos los elementos del circuito tengan señal, y False si al menos
--uno no tiene señal.
--Ejemplo: todosPositivos a0 c4 = False
--Ejemplo: todosPositivos a0 c2 = False
todosPositivos :: [(I,Bool)] -> CIRC -> Bool
todosPositivos = \l -> \c -> case c of {
	INP i -> outputAux l c;
	NOT c1 -> outputAux l c;
	XOR c1 c2 -> outputAux l c;
	OR c1-> case c1 of {
		[] -> False;
		x:xs -> and ((outputAux l x):[outputAux l (OR xs)]);
	};	
	AND c1 -> case c1 of {
		[] -> False;
		x:xs -> and ((outputAux l x):[outputAux l (AND xs)]);
	};	 
	EQ c1 -> case c1 of {
		[] -> False;
		x:xs -> and ((outputAux l x):[outputAux l (EQ xs)]);
	};	
} 

--Recibe una lista de pares (fst, snd), y un entero. Busca al entero entre los fst de los pares
--de la lista y, cuando lo encuentra, devuelve el valor de snd de el par donde lo encontró.
--Tiene como precondiciones que el entero aparece exactamente una vez en la lista de pares.
--Ejemplo: a0 2 = False
valorCorresp :: [(I,Bool)] -> Int -> Bool
valorCorresp = \l -> \n -> case l of {
	[] -> False;
	(fst, snd):ps -> case (fst == n) of {
		True -> snd;
		False -> valorCorresp ps n;
	}
}

--9
minPath :: CIRC -> Int
minPath = \c -> case c of {
	INP i -> 0;
	NOT c1 ->  1 + (minPath c1);
	XOR c1 c2 ->  case (minPath c1 <= minPath c2) of {
		True -> 1 + minPath c1;
		False -> 1 + minPath c2;
	} ;
	OR c1 -> case c1 of {
		[] -> 0;
		x:[] -> minPath x;
		x:xs -> case (minPath x <= minPath (OR xs)) of {
			True -> 1 + minPath x;
			False -> 1 + minPath (OR xs);
		};
	};
	AND c1 -> case c1 of {
		[] -> 0;
		x:[] -> minPath x;
		x:xs -> case (minPath x <= minPath (AND xs)) of {
			True -> 1 + minPath x;
			False -> 1 + minPath (AND xs);
		};
	};
	EQ c1 -> case c1 of {
		[] -> 0;
		x:[] -> 1 + minPath x;
		x:xs -> case (minPath x <= minPath (EQ xs)) of {
			True ->  minPath x;
			False -> minPath (EQ xs);
		};
	};
}

--10
maxPath :: CIRC -> Int				
maxPath = \c -> case c of {
	INP i -> 0;
	NOT c1 ->  1 + (maxPath c1);
	XOR c1 c2 ->  case (maxPath c1 >= maxPath c2) of {
		True -> 1 + maxPath c1;
		False -> 1 + maxPath c2;
	} ;
	OR c1 -> case c1 of {
		[] -> 0;
		x:[] -> 1 + maxPath x;
		x:xs -> case (maxPath x >= maxPath (OR xs)) of {
			True -> maxPath x;
			False ->  maxPath (OR xs);
		};
	};
	AND c1 -> case c1 of {
		[] -> 0;
		x:[] -> 1 + maxPath x;
		x:xs -> case (maxPath x >= maxPath (AND xs)) of {
			True -> maxPath x;
			False ->  maxPath (AND xs);
		};
	};
	EQ c1 -> case c1 of {
		[] -> 0;
		x:[] -> 1 + maxPath x;
		x:xs -> case (maxPath x >= maxPath (EQ xs)) of {
			True ->  maxPath x;
			False -> maxPath (EQ xs);
		};
	};
}

--11
maxGate :: CIRC -> Int
maxGate = \c -> maximum (listaGrados c);

--Recibe un circuito, y devuelve una lista con los grados de las compuertas y hojas de dicho circuito.
--Las hojas tienen grado 0, pues obviamente no reciben circuitos. Las incluimos también para asegurarnos
--de poder usar maximum sin problemas cuando hagamos maxGate, pues si en las hojas dejamos la lista vacía,
--obtendríamos "Exception: Prelude.maximum: empty list".
--Ejemplo: [2,1,4,3,2,1,0,0,0,0,0,2,1,0,0,3,2,1,0,0,0,0]
listaGrados :: CIRC -> [Int]
listaGrados = \c -> case c of {
	INP i -> [0];
	NOT c1 -> 1:listaGrados(c1);
	XOR c1 c2 -> 2:listaGrados(c1) ++ listaGrados(c2);
	OR c1 -> case c1 of {
		[] -> [0];
		x:xs -> len(c1):listaGrados(OR xs) ++ listaGrados(x);
	};
	AND c1 -> case c1 of {
		[] -> [0];
		x:xs -> len(c1):listaGrados(AND xs) ++ listaGrados(x);
	};
	EQ c1 -> case c1 of {
		[] -> [0];
		x:xs -> len(c1):listaGrados(EQ xs) ++ listaGrados(x);
	};
}