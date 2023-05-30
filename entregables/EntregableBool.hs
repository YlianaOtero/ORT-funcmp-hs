{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module EntregableBool where
    import Prelude (Show)
    data Bool where {False::Bool ; True::Bool}
        deriving Show

    not :: Bool -> Bool
    not = \b1 -> case b1 of {
        True -> False;
        False -> True;
    }

    ni :: Bool -> Bool -> Bool
    ni = \b1 -> \b2 -> case b1 of {
        False -> not b2;
        True -> False;
    }

    pierde :: Bool -> Bool -> Bool -> Bool
    pierde = \b1 -> \b2 -> \b3 -> case b1 of {
        True -> case b2 of {
            True -> False;
            False -> not b3;
        };
        False -> case b2 of {
            True -> not b3;
            False -> True;
        };
    };

    alt :: Bool -> Bool -> Bool -> Bool
    alt = \b1 -> \b2 -> \b3 -> case b1 of {
        True -> case b2 of {
                True -> False;
                False -> b3;
        };
        False -> case b2 of {
                True -> not b3;
                False -> False;
        };
    };

