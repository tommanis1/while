module Programs.IncorrectMutex1 where
import While

import SmartConstructorsWhile
{- 
loop
    noncritical code for process self;
    acquire(r);
    critical section for process self;
    relinquish(r);

exit when process self is finished;
    end loop
where 

acquire(r) ≡
    while turn = other loop null; end loop;

relinquish(r) ≡
    turn := other; 
-}

p = Seq (Assign "turn" (LitExpr $ LitInt 0))
    (Seq
    (Thread $
        Seq
        (While (LitExpr $ LitBool True) (Eq (Id "turn") (LitExpr $ LitInt 1)) Done)
        (Seq (Print . LitExpr . LitString $ "Begin critical section 0 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 0 ")
        (Assign "turn" (LitExpr $ LitInt 1))))    
        )
    (Thread $
        Seq
        (While (LitExpr $ LitBool True) (Eq (Id "turn") (LitExpr $ LitInt 0)) Done)
        (Seq (Print . LitExpr . LitString $ "Begin critical section 1 ")
        (Seq (Print . LitExpr . LitString $ "End critical section 1 ")
        (Assign "turn" (LitExpr $ LitInt 0))))    
        ))

-- ps = 
--     (Assign "turn" (i 0)) ⍮
--     (Thread $
--         (while (Eq (Id "turn") (i 1)) Done) ⍮
--         (Print $ s "Begin critical section 0 ") ⍮
--         (Print $ s "End critical section 0 ") ⍮
--         (Assign "turn" (i 1)))
--     (Thread $
--         (while(Eq (Id "turn") (LitExpr $ LitInt 0)) Done) ⍮
--          (Print . LitExpr . LitString $ "Begin  critical section 1 ") ⍮
--         (Print . LitExpr . LitString $ "End critical section 1 ") ⍮
--         (Assign "turn" (i 0)))

-- possible syntax 

{- 
t {
    while (turn == other) {
        done
    }
    print("Begin critical section 0")
    print("End critical section 0")
    turn = other

}

t {
    while (turn == other) {
        done
    }
    print("Begin critical section 1")
    print("End critical section 1")
    turn = other

}

 -}
