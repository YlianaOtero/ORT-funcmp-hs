{-#LANGUAGE GADTs#-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module ParcialNoc_2_12_2022 where
import Prelude

g :: X -> N -> N
g = \t n -> case t of {
     A -> case n of { 
        0 -> 0 ; 
        S k -> S (g t k) 
    } ;
    B x y z -> case x of { 
        True -> g y n; 
        False -> g z n 
    } 
}

--(a) Defina el tipo X para que la funci´on g compile: data X where {A ::... ; B ::... }.
data X where {
    A :: X; --hoja sin info
    B :: Bool -> X -> X -> X; --nodo binario con info :: Bool
}

--(b) Demuestre que (∀t::X)(∀n::N) g t n = n
    --Dmt. por induccion en t::X
    --Caso A: (Vn :: N) g A n = n
        --Dmt por induccion en n::N
        --Caso O: g A O = O
            -- = por def.
            -- O

        --Caso S: sea k::N
            --H) g A k = k
            --T) g A (S k) = S k
                -- g A (S k) 
                -- = por def. de g
                -- S (g A k)
                -- = por H)
                -- S k
    
    --Caso B: Sean y, z :: X:
        --H1) (Vn :: N) g y n = n
        --H2) (Vn :: N) g z n = n
        --H3) (Vx :: Bool) (Vn :: N) g (B x y z) n = n
            --Dmt por casos en x :: Bool:
            --Caso True: (Vn :: N) g (B True y z) n = n
                --g (B True y z) n
                -- = por def.
                --g y n
                -- = por H1).
                --n

            --Caso True: (Vn :: N) g (B False y z) n = n
             --g (B False y z) n
             -- = por def.
             -- g z n
             -- = por H2) 
             -- n

--EJERCICIO 3
data T where {
    H :: T ;
    I :: T -> T -> T 
}

--Un arbol T se llama perfecto si y s´olo si todos sus niveles est´an completos.
--(a) Defina la funci´on niveles :: T -> N que computa la cantidad de niveles de un ´arbol de tipo T
--(no necesariamente perfecto). Puede hacer uso de la funci´on (<=) :: N -> N -> N.
niveles :: T -> N
niveles = \t -> case t of {
    H -> S O;
    I i d -> case ((niveles i) <= (niveles d)) of {
        True -> S(niveles d);
        False -> S(niveles i)
    }
}

--(b) Defina la funci´on perfecto :: T -> Bool que verifique si un ´arbol es perfecto. Puede hacer
--uso la funci´on (==) :: N -> N -> N.
perfecto :: T -> Bool
perfecto = \t -> case t of {
    H -> True; 
    I i d -> (niveles i == niveles d) && (perfecto i && perfecto d);
}

--(c) Defina la funci´on perfGen :: N -> T, que recibe un natural n y genera un ´arbol perfecto con
--n + 1 niveles.
perfGen :: N -> T 
perfGen = \n -> case n of {
    O -> H;
    S x -> I (perfGen x)(perfGen x);
}

--(d) Demuestre que que perfGen es correcta en relacion a la definicion de perfecto, es decir:
--(∀ n::N) perfecto (perfGen n) = True.
--Puede utilizar la reflexividad de (==) en N sin necesidad de demostrarla, o sea: (∀n::N) n == n = True.

--(∀ n::N) perfecto (perfGen n) = True
--Dmt. por induccion en n::N
    --Caso O: perfecto(perfGen O) = True
        -- = por def. de perfGen:
        --perfecto H
        -- = por def. de perfecto:
        --True

    --Caso S x: Sea x::N
        --H) perfecto(perfGen x) = True
        --T) perfecto(perfGen (S x)) = True
        --perfecto(perfGen (S x))
        -- = por def. de perfGen:
        --perfecto(I (perfGen x) (perfGen x))
        -- = por def. de perfecto:
        --(niveles (perfGen x) == niveles (perfGen x)) && perfecto (perfGen x) && perfecto (perfGen x)
        -- = por reflexividad del ==:
        -- True && perfecto (perfGen x) && perfecto (perfGen x)
        -- = por H):
        --True && True && True
        -- = por def. de &&x2:
        --True

        