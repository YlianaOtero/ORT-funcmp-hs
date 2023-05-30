{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Lab1 where
    import Prelude (Show)
    data Bool where {False::Bool ; True::Bool} deriving Show

    not :: Bool -> Bool
    not = \b1 -> case b1 of {
        True -> False;
        False -> True;
    }

--Ejercicio 2. a)
    (&&) :: Bool -> Bool -> Bool
    (&&) = \b1 -> \b2 -> case b1 of {
        True -> b2;
        False -> False;
    }

--Ejercicio 2. b)
    (||) :: Bool -> Bool -> Bool
    (||) = \b1 -> \b2 -> case b1 of {
        True -> True;
        False -> b2;
    }

--Ejercicio 2. c)
    xor :: Bool -> Bool -> Bool
    xor = \b1 -> \b2 -> case b1 of {
        True -> not b2;
        False -> b2;
    }

--Ejercicio 2. d)
--Equivale a nor
    ni :: Bool -> Bool -> Bool
    ni = \b1 -> \b2 -> case b1 of {
        True -> False;
        False -> not b2;
    }

--Ejercicio 3. a)
    (==) :: Bool -> Bool -> Bool
    (==) = \b1 -> \b2 -> case b1 of {
        True -> b2;
        False -> not b2;
    }

--Ejercicio 3. b)
    (===) :: Bool -> Bool -> Bool
    (===) = \b1 -> \b2 -> not (xor b1 b2)

--Ejercicio 3. c)
    (\=) :: Bool -> Bool -> Bool
    (\=) = \b1 -> \b2 -> case b1 of {
        True -> not b2;
        False -> b2;
    }
--Es igual al xor!

--Ejercicio 3. d)
    (<=) :: Bool -> Bool -> Bool
    (<=) = \b1 -> \b2 -> case b1 of {
        True -> b2;
        False -> True;
    }

--Ejercicio 4. a)
    unanimidad :: Bool -> Bool -> Bool -> Bool
    unanimidad = \b1 -> \b2 -> \b3 -> case b1 of {
        True -> case b2 of {
            True -> b3;
            False -> False;
        };
        False -> case b2 of {
            True -> False;
            False -> not b3;
        }
    }

    unanimidadCorta :: Bool -> Bool -> Bool -> Bool
    unanimidadCorta = \b1 -> \b2 -> \b3 -> case b1 of {
        True -> b2 && b3;
        False -> ni b1 b2;
    }

    unanimidadMasCorta :: Bool -> Bool -> Bool -> Bool
    unanimidadMasCorta = \b1 -> \b2 -> \b3 -> (b1 && b2 && b3) || ((not b1) && ni b2 b3)

--Ejercicio 4. b)
    mayoria :: Bool -> Bool -> Bool -> Bool
    mayoria = \b1 -> \b2 -> \b3 -> case b1 of {
        True -> b2 || b3;
        False -> b2 && b3;
    }
