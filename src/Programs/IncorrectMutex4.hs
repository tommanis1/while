module Programs.IncorrectMutex4 where
import While
-- acquire(r) ≡
--     claimed(self) := true;
--     while claimed(other) loop
--         claimed(self) := false;
--         while claimed(other) loop null; end loop;
--         claimed(self) := true;
--     end loop;

-- print 

p = 
    Seq
    (Thread $
        (Seq (Assign "claimed_0" (LitExpr $ LitBool True))
        (Seq (While (LitExpr $ LitBool True) (Id "claimed_1") 
            (Seq (Assign "claimed_0" (LitExpr $ LitBool False)) 
            (Seq (While (LitExpr $ LitBool True) (Id "claimed_1") Done)
            (Assign "claimed_0" (LitExpr$ LitBool True)))))
        (Seq (Print . LitExpr . LitString $ "Begin critical section 0 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 0 ")
        (Assign "claimed_0" (LitExpr$ LitBool False))
        ))))
    )
    (Thread $
        (Seq (Assign "claimed_1" (LitExpr $ LitBool True))
        (Seq (While (LitExpr $ LitBool True) (Id "claimed_0") 
            (Seq (Assign "claimed_1" (LitExpr $ LitBool False)) 
            (Seq (While (LitExpr $ LitBool True) (Id "claimed_0") Done)
            (Assign "claimed_1" (LitExpr$ LitBool True)))))
        (Seq (Print . LitExpr . LitString $ "Begin critical section 1 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 1 ")
        (Assign "claimed_1" (LitExpr$ LitBool False))
        ))))
    )

    -- claimed[i] = true;
-- while (claimed[(i+1) % 2]) {
--     claimed[i] = false;
--     while (claimed[(i+1) % 2]) {done};
--     claimed[i] = true;
-- }