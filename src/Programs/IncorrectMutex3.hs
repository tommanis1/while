module Programs.IncorrectMutex3 where
import While

-- p = 
--     Seq
--     (Thread $
--         (Seq (Assign "claimed_0" (LitExpr $ LitBool True))
--         (Seq (While (LitExpr $ LitBool True) (Id "claimed_1") Done)
--         (Seq (Print . LitExpr . LitString $ "Begin critical section 0 ")
--         (Seq (Print . LitExpr . LitString $ "End critical section 0 ")
--         (Assign "claimed_0" (LitExpr$ LitBool False))
--         ))))
--     )
--     (Thread $
--         (Seq (Assign "claimed_1" (LitExpr $ LitBool True))
--         (Seq (While (LitExpr $ LitBool True) (Id "claimed_0") Done)
--         (Seq (Print . LitExpr . LitString $ "Begin critical section 1 ")
--         (Seq (Print . LitExpr . LitString $ "End critical section 1 ")
--         (Assign "claimed_1" (LitExpr$ LitBool False))
--         ))))
--     )

p =     
    (Seq (Assign "claimed_0" (LitExpr $ LitBool False) )
    (Seq (Assign "claimed_1" (LitExpr $ LitBool False) )
    
    (Seq (Assign "critical_0" (LitExpr $ LitBool False) )
    (Seq (Assign "critical_1" (LitExpr $ LitBool False) )

    (Seq
    (Thread $
        (While (LitExpr $ LitBool True) (LitExpr $ LitBool True)
        (Seq (Assign "claimed_0" (LitExpr $ LitBool True) )
        (Seq (While  (Id "claimed_1") (Id "claimed_1") Done)
        (Seq (Assign "critical_0" (LitExpr $ LitBool True) )
        (Seq (Assign "critical_0" (LitExpr $ LitBool False))
        (Assign "claimed_0" (LitExpr$ LitBool False))
        
        )))))
    )
    (Thread $
        (While (LitExpr $ LitBool True) (LitExpr $ LitBool True)
        (Seq (Assign "claimed_1" (LitExpr $ LitBool True) )
        (Seq(While (Id "claimed_0") (Id "claimed_0") Done)
        (Seq (Assign "critical_1" (LitExpr $ LitBool True) )
        (Seq (Assign "critical_1" (LitExpr $ LitBool False))
        (Assign "claimed_1" (LitExpr $ LitBool False))
        )))))
    ))))))