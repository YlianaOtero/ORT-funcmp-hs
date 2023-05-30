{-#LANGUAGE GADTs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Entregable5_Listas where 
    import Prelude hiding (null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++),reverse,concat,head,tail,last,init,(!!),fst,snd,take,drop,takeWhile,dropWhile,split)
    data N where { O :: N ; S :: N -> N } deriving Show
    
    uno = S O
    dos = S uno
    tres = S dos
    cuatro = S tres
    cinco = S cuatro

    --Ejercicio 1.
    insertarUlt :: a -> [a] -> [a]
    insertarUlt = \e -> \l -> case l of {
        [] -> [e];
        x:xs -> x:(insertarUlt e xs);
    }

    --Ejercicio 2.
    insertarN :: N -> a -> [a] -> [a] 
    insertarN = \n -> \e -> \l -> case l of {
        [] -> [e];
        x:xs -> case n of {
            O -> e:l;
            S k -> x:(insertarN k e xs);
        }
    }

    --Ejercicio 3.
    insertarOrd :: Ord a => a -> [a] -> [a]
    insertarOrd = \e -> \l -> case l of {
        [] -> [e];
        x:xs -> case (x > e) of {
            True -> e:x:xs;
            False -> x:(insertarOrd e xs);
        }
    }
    

    --Ejercicio 4.
    reemplazarP :: (a -> Bool) -> a -> [a] -> [a]
    reemplazarP = \p -> \e -> \l -> case l of {
        [] -> [];
        x:xs -> case (p x) of {
            True -> e:(reemplazarP p e xs);
            False -> x:(reemplazarP p e xs);
        }
    }