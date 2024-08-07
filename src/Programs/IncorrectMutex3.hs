module Programs.IncorrectMutex3 where
import While

p = 
    Seq
    (Thread $
        (Seq (Assign "claimed_0" (LitExpr $ LitBool True))
        (Seq (While (LitExpr $ LitBool True) (Id "claimed_1") Done)
        (Seq (Print . LitExpr . LitString $ "Begin critical section 0 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 0 ")
        (Assign "claimed_0" (LitExpr$ LitBool False))
        ))))
    )
    (Thread $
        (Seq (Assign "claimed_1" (LitExpr $ LitBool True))
        (Seq (While (LitExpr $ LitBool True) (Id "claimed_0") Done)
        (Seq (Print . LitExpr . LitString $ "Begin critical section 1 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 1 ")
        (Assign "claimed_1" (LitExpr$ LitBool False))
        ))))
    )