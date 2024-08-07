module Programs.IncorrectMutex2 where
import While
-- acquire(r) ≡
--     while claimed(other) loop null; end loop;
--     claimed(self) := true;

-- relinquish(r) ≡
--     claimed(self) := false;

p' =     
    (Seq (Assign "claimed_0" (LitExpr $ LitBool False) )
    (Seq (Assign "claimed_1" (LitExpr $ LitBool False) )

    (Seq
    (Thread $
        (While (LitExpr $ LitBool True) (LitExpr $ LitBool True)
        (Seq (While (LitExpr $ LitBool True) (Id "claimed_1") Done)
        (Seq Done
        (Seq (Assign "claimed_0" (LitExpr $ LitBool True) )
        (Seq (Print . LitExpr . LitString $ "Begin critical section 0 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 0 ")
        (Assign "claimed_0" (LitExpr$ LitBool False))
        ))))))
    )
    (Thread $
        (While (LitExpr $ LitBool True) (LitExpr $ LitBool True)
        (Seq(While (LitExpr $ LitBool True) (Id "claimed_0") Done)
        (Seq Done
        (Seq (Assign "claimed_1" (LitExpr $ LitBool True) )
        (Seq (Print . LitExpr . LitString $ "Begin critical section 1 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 1 ")
        (Assign "claimed_1" (LitExpr$ LitBool False))
        ))))))
    ))))
p =     
    (Seq (Assign "claimed_0" (LitExpr $ LitBool False) )
    (Seq (Assign "claimed_1" (LitExpr $ LitBool False) )

    (Seq
    (Thread $
        (Seq (While (LitExpr $ LitBool True) (Id "claimed_1") Done)
        (Seq (Assign "claimed_0" (LitExpr $ LitBool True) )
        (Seq (Print . LitExpr . LitString $ "Begin critical section 0 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 0 ")
        (Assign "claimed_0" (LitExpr$ LitBool False))
        ))))
    )
    (Thread $
        (Seq(While (LitExpr $ LitBool True) (Id "claimed_0") Done)
        (Seq (Assign "claimed_1" (LitExpr $ LitBool True) )
        (Seq (Print . LitExpr . LitString $ "Begin critical section 1 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 1 ")
        (Assign "claimed_1" (LitExpr$ LitBool False))
        ))))
    ))))

-- Concrete syntax possibilities 

-- {-
-- t {
--     while (claimed[1]) {Done}
--     claimed[0] = true
--     print ("Begin critical section "  ++ str i)
--     print ("End critical section "  ++ str i)
--     claimed[0] = true
-- }
--   -}

{-
f(i) {
    while (claimed[(i+1) % 2]) {Done}
    claimed[i] = true
    print ("Begin critical section "  ++ str i)
    print ("End critical section "  ++ str i)
    claimed[i] = false
}
t {f(0)}
t {f(1)}
-}