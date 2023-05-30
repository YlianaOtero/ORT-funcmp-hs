{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Lab2 where

data N where { O :: N ; S :: N -> N } deriving Show

uno = S O
dos = S uno
tres = S dos
      
predecesor :: N -> N
predecesor = \n -> case n of {
                    O -> O; 
                    S x -> x
                }

predecesor2 :: N->N
predecesor2 = \n -> case n of {
                    O -> error "el cero no es precedido por nada";
                    S x -> x;
                }

--ej 1
instance Eq N where 
    (==) = \n -> \m -> case n of {
                        O -> case m of {
                                O -> True;
                                S x -> False;
                        };
                        S y -> case m of {
                                O -> False;
                                S z -> y == z;
                        }
                    }


--ej 2
instance Ord N where 
    (<=) = \n -> \m -> case n of {
                        O -> True;
                        S x -> case m of {
                                O -> False;
                                S y -> x <= y;
                        }
                    }


--ej 3.3
minimo :: N -> N -> N
minimo = \n -> \m -> case (n <= m) of {
                        True -> n;
                        False -> m;
                    }  

maximo :: N -> N -> N
maximo = \n -> \m -> case (n >= m) of {
                        True -> n;
                        False -> m;
                    }  

--ej 3.4
min3 :: N -> N -> N -> N 
min3 = \n -> \m -> \k -> minimo n (minimo m k)

--ej 4.1
instance Num N where
    (+) = \n -> \m -> case n of {
                        O -> m;
                        S k -> S (k + m);
                    }
    (-) = \n -> \m -> case n of {
                        O -> O;
                        S k -> case m of {
                            O -> n;
                            S l -> k - l;
                        };
                    }

    (*) = \n -> \m -> case n of {
                        O -> O;
                        S k -> m + (k * m);
                    }

--ej 4.3
(%) :: N -> N -> N
(%) = \n -> \m -> case m of {
                    O -> uno;
                    S k -> n * (n % k);
                }

--ej 4.4
doble :: N -> N 
doble = (dos *)

--ej 4.5 
fact :: N -> N 
fact = \n -> case n of {
                O -> uno;
                S k -> n * (fact k);
            }

--ej 4.7
sumdoble :: N -> N 
sumdoble = \n -> sumfi doble n

--ej. 4.8
sumfact :: N -> N 
sumfact = \n -> sumfi fact n

--ej 4.9
sumfi :: (N -> N) -> N -> N 
sumfi = \f -> \n -> case n of {
    O -> f O;
    S k -> f(S k) + (sumfi f k);
}

--ej 4.11
par :: N -> Bool 
par = \n -> case n of {
    O -> True; 
    S k -> not (par k)
}

sumpares :: N -> N 
sumpares = \n -> case n of {
    O -> O;
    S k -> case par (S k) of {
        True -> S k + sumpares k;
        False -> sumpares k;
    }
}

--ej 4.12
impar :: N -> Bool 
impar = \n -> not (par n) -- \n -> (not . par) n

sumimpares :: N -> N 
sumimpares = \n -> sumpi impar n

--ej 4.13
sumpi :: (N -> Bool) -> N -> N 
sumpi = \p -> \n -> case n of {
    O -> O;
    S k -> case p (S k) of {
        True -> S k + (sumpi p k);
        False -> sumpi p k;
    }
}

--ej 4.14
sumpares2 :: N -> N
sumpares2 = \n -> sumpi par n

--ej 4.15
sumcuadimp :: N -> N 
sumcuadimp = \n -> case n of {
    O -> O;
    S k -> case impar (S k) of {
        True -> ((S k) * (S k)) + sumcuadimp k;
        False -> sumcuadimp k;
    }
}