module Programs.CorrectMutex where
import While
petersons_algorithm =
    Seq
    (Thread $
        (Seq (Assign "claimed_0" (LitExpr $ LitBool True))
        (Seq (Assign "turn" (LitExpr $ LitInt 1))
        (Seq (While (LitExpr $ LitBool True) (And (Id "claimed_1") (Eq (Id "turn") (LitExpr $ LitInt 1))) Done)
        (Seq (Print . LitExpr . LitString $ "Begin critical section 0 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 0 ")
        (Assign "claimed_0" (LitExpr$ LitBool False))
        )))))
    )
    (Thread $
        (Seq (Assign "claimed_0" (LitExpr $ LitBool True))
        (Seq (Assign "turn" (LitExpr $ LitInt 1))
        (Seq (While (LitExpr $ LitBool True) (And (Id "claimed_1") (Eq (Id "turn") (LitExpr $ LitInt 1))) Done)
        (Seq (Print . LitExpr . LitString $ "Begin critical section 0 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 0 ")
        (Assign "claimed_0" (LitExpr$ LitBool False))
        )))))
    )