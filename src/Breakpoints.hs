module While.Breakpoints where

-- Configuration conditions: conditions describing a single state, corresponding to a single case in the evluate function.
BWating = Waiting 

-- Step conditions: conditions on the edges. 

turnChanged = Not $ Equal (TargetConfiguration (Id "turn")) (CurrentConfiguration (Id "turn"))


BWaiting = And 
    (Equal 
        (CurrentConfiguration $ OutputCoutingFromLatest 0)
        (lit_str "Waiting"))
    (Equal 
        (TargetConfiguration $ OutputCoutingFromLatest 0)
        (lit_str "Waiting"))

