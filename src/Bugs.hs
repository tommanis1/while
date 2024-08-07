{-# LANGUAGE FlexibleContexts #-}
module Bugs where

import DebugWhile hiding(And)
import MVD.Debugger
import MVD.Finders
import MVD.Interface

import SmartConstructorsWhile
import While

-- runs out of memory? 
debug_mutex_example' p b = debuggerWithShow (whileStr (Just p)) equalityFinder b ()

no_mutex = 
    -- U_And [
    -- U_And [
        -- C $ InStore "critical_0"
        -- , C $ InStore "critical_1"
        -- ]
    -- ,
    -- , C $ E $ Id $ "critical_1"]
    -- ,  C $ E (Eq (Id $ "critical_0") (Id "critical_1"))
    -- , Equal (E $ Id $ "critical_0") (E $ Id "critical_1")
    -- , Equal  (E $ Id $ "critical_0") (E $ b $ True)
    -- , 
    Equal  (E $ Id $ "claimed_1") (E $ b $ True)
    -- , Equal  (E $ Id $ "claimed_1") (E $ b $ True)

    -- , Equal  (E $ Id $ "claimed_1") (E $ b $ True)

    -- C $ E $ And (Eq (Id $ "critical_0") (Id "critical_1")) (Eq (Id "critical_1") (b True))
    -- ]

no_mutex1 = U_And [
    C $ InStore "i0"
    ,     C $ E (Eq (Id "i0") (i 3))

    ]
-- bu = debug_mutex_example' (   (Seq (Assign "claimed_0" (LitExpr $ LitBool False) )
--     (Seq (Assign "claimed_1" (LitExpr $ LitBool False) )
--     (Seq (Assign "critical_0" (LitExpr $ LitBool False) )
--     (Seq (Assign "critical_1" (LitExpr $ LitBool False) )
--     (Seq (Assign "i0" (LitExpr $ LitInt 0) )
--     (Seq (Assign "i1" (LitExpr $ LitInt 0) )

--     (Seq
--     (Thread $
--         (While (LitExpr $ LitBool True) (Leq (Id "i0") (LitExpr $ LitInt 5) )
--         (Seq (While (Id "claimed_1")  (Id "claimed_1") Done)
--         (Seq Done
--         (Seq (Assign "claimed_0" (LitExpr $ LitBool True) )
--         (Seq (Assign "critical_0" (LitExpr $ LitBool True) )
--         (Seq (Assign "critical_0" (LitExpr $ LitBool False))
--         (Seq (Assign "i0" (Plus (Id "i0") (LitExpr $ LitInt 1)))

--         (Assign "claimed_0" (LitExpr$ LitBool False))
        
--         )))))))
--     )
--     (Thread $
--         (While (Leq (Id "i1") (LitExpr $ LitInt 5) ) (Leq (Id "i1") (LitExpr $ LitInt 5) )

--         (Seq(While (Id "claimed_0")  (Id "claimed_0") Done)
--         (Seq Done
--         (Seq (Assign "claimed_1" (LitExpr $ LitBool True) )
--         (Seq (Assign "critical_1" (LitExpr $ LitBool True) )
--         (Seq (Assign "critical_1" (LitExpr $ LitBool False))
--             (Seq (Assign "i1" (Plus (Id "i1") (LitExpr $ LitInt 1)))
--         (Assign "claimed_1" (LitExpr $ LitBool False))
--         )))))))
--     ))))))))) no_mutex

incorrect_mutex = debug_mutex_example' (   (Seq (Assign "claimed_0" (LitExpr $ LitBool False) )
    (Seq (Assign "claimed_1" (LitExpr $ LitBool False) )
    (Seq (Assign "critical_0" (LitExpr $ LitBool False) )
    (Seq (Assign "critical_1" (LitExpr $ LitBool False) )
    (Seq (Assign "i0" (LitExpr $ LitInt 0) )
    (Seq (Assign "i1" (LitExpr $ LitInt 0) )

    (Seq
    (Thread $
        (While (Leq (Id "i0") (LitExpr $ LitInt 2) ) (Leq (Id "i0") (LitExpr $ LitInt 2) )
            (Seq (While (Id "claimed_1")  (Id "claimed_1") Done)
            -- (Seq Done
            (Seq (Assign "claimed_0" (LitExpr $ LitBool True) )
            (Seq (Assign "critical_0" (LitExpr $ LitBool True) )
            (Seq (Assign "critical_0" (LitExpr $ LitBool False))
            (Seq (Assign "i0" (Plus (Id "i0") (LitExpr $ LitInt 1)))

            (Assign "claimed_0" (LitExpr$ LitBool False))
        
        ))))))
    )
    (Thread $
        (While (Leq (Id "i1") (LitExpr $ LitInt 2) ) (Leq (Id "i1") (LitExpr $ LitInt 2) )

            (Seq(While (Id "claimed_0") (Id "claimed_0") Done)
            -- (Seq Done
            (Seq (Assign "claimed_1" (LitExpr $ LitBool True) )
            (Seq (Assign "critical_1" (LitExpr $ LitBool True) )
            (Seq (Assign "critical_1" (LitExpr $ LitBool False))
                (Seq (Assign "i1" (Plus (Id "i1") (LitExpr $ LitInt 1)))
            (Assign "claimed_1" (LitExpr $ LitBool False))
        ))))))
    ))))))))) no_mutex

bug = debug_mutex_example' (
    Thread (while (b True) (Print $ s "t0" )) ⍮
    Thread (while (b True) (Print $ s "t1" ))
    ) (U_And [
        Equal
        (OutputCoutingFromLatest 0)
        (OutputCoutingFromLatest 1)
    ])

present_bug = debug_mutex_example' (   
    (Seq (Assign "claimed_0" (LitExpr $ LitBool False) )
    (Seq (Assign "claimed_1" (LitExpr $ LitBool False) )
    -- (Seq (Assign "critical_0" (LitExpr $ LitBool False) )
    -- (Seq (Assign "critical_1" (LitExpr $ LitBool False) )
    (Seq (Assign "i0" (LitExpr $ LitInt 0) )
    (Seq (Assign "i1" (LitExpr $ LitInt 0) )

    (Seq
    (Thread $
        (While (Leq (Id "i0") (LitExpr $ LitInt 2) ) (Leq (Id "i0") (LitExpr $ LitInt 2) )
        -- Removing this loop works
        (Seq (While (Id "claimed_1")  (Id "claimed_1") Done)
        -- (Seq Done
        -- (Seq 
        (Assign "claimed_0" (LitExpr $ LitBool True) )
        -- (Seq (Assign "critical_0" (LitExpr $ LitBool True) )
        -- (Seq (Assign "critical_0" (LitExpr $ LitBool False))
        -- (Seq (Assign "i0" (Plus (Id "i0") (LitExpr $ LitInt 1)))

        -- (Assign "claimed_0" (LitExpr$ LitBool False))
        
        )))
    (Thread $
        (While (Leq (Id "i1") (LitExpr $ LitInt 2) ) (Leq (Id "i1") (LitExpr $ LitInt 2) )

        (Seq(While (Id "claimed_0") (Id "claimed_0") Done)
        -- (Seq Done
        -- (Seq 
        (Assign "claimed_1" (LitExpr $ LitBool True) )
        -- (Seq (Assign "critical_1" (LitExpr $ LitBool True) )
        -- (Seq (Assign "critical_1" (LitExpr $ LitBool False))
            -- (Seq (Assign "i1" (Plus (Id "i1") (LitExpr $ LitInt 1)))
        -- (Assign "claimed_1" (LitExpr $ LitBool False))
        )))
    )))))) (    
        U_And[
         C $ InStore "claimed_1"
        , Equal  (E $ Id $ "claimed_1") (E $ b $ True)
            , Equal  (E $ Id $ "claimed_0") (E $ b $ True)

        ]
    ) 

t = debug_mutex_example' (   
    (Seq (Assign "claimed_0" (LitExpr $ LitBool False) )
    (Seq (Assign "claimed_1" (LitExpr $ LitBool False) )
    (Seq (Assign "s" (LitExpr $ LitBool False) )
    (Seq (Assign "critical_0" (LitExpr $ LitBool False) )
    (Seq (Assign "critical_1" (LitExpr $ LitBool False) )
    (Seq (Assign "i0" (LitExpr $ LitInt 0) )
    (Seq (Assign "i1" (LitExpr $ LitInt 0) )
    -- (Seq (Assign "s" (LitExpr $ LitInt 0) )
    (Seq
    (Thread $
        (While (Leq (Id "i0") (LitExpr $ LitInt 0) ) (Leq (Id "i0") (LitExpr $ LitInt 0) )
        (Seq (While (Id "claimed_1")  (Id "claimed_1") Done)
        -- (Assign "s" (Plus (Id "s") (i 1))))
        -- (Seq Done
        (Seq 
        (Assign "claimed_0" (LitExpr $ LitBool True) )
        (Seq (Assign "critical_0" (LitExpr $ LitBool True) )
        (Seq (Assign "critical_0" (LitExpr $ LitBool False))
        (Seq  (Assign "claimed_0" (LitExpr$ LitBool False))
        (Assign "i0" (Plus (Id "i0") (LitExpr $ LitInt 1)))
        )
        )))
        )))
        --)
    (Thread $
        (While (Leq (Id "i1") (LitExpr $ LitInt 0) ) (Leq (Id "i1") (LitExpr $ LitInt 0) )

        (Seq(While (Id "claimed_0") (Id "claimed_0") Done)
        -- (Seq Done
        (Seq 
        (Assign "claimed_1" (LitExpr $ LitBool True) )
        (Seq (Assign "critical_1" (LitExpr $ LitBool True) )
        (Seq (Assign "critical_1" (LitExpr $ LitBool False))
        (Seq (Assign "i1" (Plus (Id "i1") (LitExpr $ LitInt 1)))
        (Assign "claimed_1" (LitExpr $ LitBool False))
        ))))
        )))

    )))))))))
    -- ))) 
    (    
        U_And[
         C $ InStore "claimed_1"
        --  , C $ IsTrue "claimed_1"
        --  , C $ IsTrue "claimed_0"
        , Equal  (E $ Id $ "claimed_1") (E $ b $ True)
        , Equal  (E $ Id $ "claimed_0") (E $ b $ True)

        ]
    ) 

tt = debug_mutex_example' (   
    (Seq (Assign "claimed_0" (LitExpr $ LitBool False) )
    (Seq (Assign "claimed_1" (LitExpr $ LitBool False) )
    (Seq (Assign "s" (LitExpr $ LitBool False) )
    (Seq (Assign "critical_0" (LitExpr $ LitBool False) )
    (Seq (Assign "critical_1" (LitExpr $ LitBool False) )
    (Seq (Assign "i0" (LitExpr $ LitInt 0) )
    (Seq (Assign "i1" (LitExpr $ LitInt 0) )
    -- (Seq (Assign "s" (LitExpr $ LitInt 0) )
    (Seq
    (Thread $
        -- (While (Leq (Id "i0") (LitExpr $ LitInt 0) ) (Leq (Id "i0") (LitExpr $ LitInt 0) )
        (Seq (While (Id "claimed_1")  (Id "claimed_1") Done)
        -- (Assign "s" (Plus (Id "s") (i 1))))
        -- (Seq Done
        (Seq 
        (Assign "claimed_0" (LitExpr $ LitBool True) )
        -- (Seq (Assign "critical_0" (LitExpr $ LitBool True) )
        -- (Seq (Assign "critical_0" (LitExpr $ LitBool False))
        -- (Seq  (Assign "claimed_0" (LitExpr$ LitBool False))
        (Assign "i0" (Plus (Id "i0") (LitExpr $ LitInt 1)))
        )
        ))
        -- ))
        --)
    (Thread $
        -- (While (Leq (Id "i1") (LitExpr $ LitInt 0) ) (Leq (Id "i1") (LitExpr $ LitInt 0) )

        (Seq(While (Id "claimed_0") (Id "claimed_0") Done)
        -- (Seq Done
        (Seq 
        (Assign "claimed_1" (LitExpr $ LitBool True) )
        -- (Seq (Assign "critical_1" (LitExpr $ LitBool True) )
        -- (Seq (Assign "critical_1" (LitExpr $ LitBool False))
        (Assign "i1" (Plus (Id "i1") (LitExpr $ LitInt 1)))
        -- (Assign "claimed_1" (LitExpr $ LitBool False))
        )))
        -- ))

    )))))))))
    -- ))) 
    (    
        U_And[
         C $ InStore "claimed_1"
        --  , C $ IsTrue "claimed_1"
        --  , C $ IsTrue "claimed_0"
        , Equal  (E $ Id $ "claimed_1") (E $ b $ True)
        , Equal  (E $ Id $ "claimed_0") (E $ b $ True)

        ]
    )