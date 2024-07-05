module Programs.DiningPhilosophers where
import While
import SmartConstructorsWhile
-- Dining philosphers with one mutex, using Petersons algorithm, for each fork. We consider each philosopher to be the 'first' user of it's left hand fork, and the second user of it's right hand fork. 
naive = 
    Thread (
        (Print $ s "philosopher 0 is thinking") ⍮
        --acquire fork 0
        (Assign "0_claimed_0" (LitExpr $ LitBool True)) ⍮
        (Assign "0_turn" (i 1)) ⍮
        (while (And (Id "0_claimed_1") (Eq (Id "0_turn") (i 1))) Done) ⍮
        (Print $ s "philosopher 0 acquired fork 0") ⍮


        --acquire fork 1
        (Assign "1_claimed_1" (LitExpr $ LitBool True)) ⍮
        (Assign "1_turn" (i 0)) ⍮
        (while (And (Id "1_claimed_0") (Eq (Id "1_turn") (i 0))) Done) ⍮

        (Print $ s "philosopher 0 is eating") ⍮
        --release fork 0
        (Assign "0_claimed_0" (LitExpr$ LitBool False))⍮
        --release fork 0
        (Assign "1_claimed_1" (LitExpr$ LitBool False))
    ) ⍮
    Thread (
        (Print $ s "philosopher 1 is thinking") ⍮
        --acquire fork 1
        (Assign "1_claimed_0" (LitExpr $ LitBool True)) ⍮
        (Assign "1_turn" (i 1)) ⍮
        (while (And (Id "1_claimed_1") (Eq (Id "1_turn") (i 1))) Done) ⍮
        (Print $ s "philosopher 0 acquired fork 1") ⍮


        --acquire fork 2
        (Assign "2_claimed_1" (LitExpr $ LitBool True)) ⍮
        (Assign "2_turn" (i 0)) ⍮
        (while (And (Id "2_claimed_0") (Eq (Id "2_turn") (i 0))) Done) ⍮

        (Print $ s "philosopher 1 is eating") ⍮
        --release fork 1
        (Assign "1_claimed_0" (LitExpr$ LitBool False))⍮
        --release fork 2
        (Assign "2_claimed_1" (LitExpr$ LitBool False))
    )
    ⍮
        Thread (
        (Print $ s "philosopher 2 is thinking") ⍮
        --acquire fork 2
        (Assign "2_claimed_0" (LitExpr $ LitBool True)) ⍮
        (Assign "2_turn" (i 1)) ⍮
        (while (And (Id "2_claimed_1") (Eq (Id "2_turn") (i 1))) Done) ⍮
        (Print $ s "philosopher 0 acquired fork 2") ⍮


        --acquire fork 0
        (Assign "0_claimed_1" (LitExpr $ LitBool True)) ⍮
        (Assign "0_turn" (i 0)) ⍮
        (while (And (Id "0_claimed_0") (Eq (Id "0_turn") (i 0))) Done) ⍮

        (Print $ s "philosopher 2 is eating") ⍮
        --release fork 2
        (Assign "2_claimed_0" (LitExpr$ LitBool False))⍮
        --release fork 0
        (Assign "0_claimed_1" (LitExpr$ LitBool False))
    )