module Examples where

import qualified Programs.IncorrectMutex1 as P1
import qualified Programs.IncorrectMutex2 as P2
import qualified Programs.IncorrectMutex3 as P3
import qualified Programs.IncorrectMutex4 as P4

import DebugWhile
import MultiverseDebug
import While hiding (And)

no_mutual_exclusion_breakpoint :: BreakPoint 
no_mutual_exclusion_breakpoint = Or
    (And 
        (OutputCoutingFromLatest 0 "Begin critical section 0 ") 
        (OutputCoutingFromLatest 1 "Begin critical section 1 "))
    (And 
        (OutputCoutingFromLatest 1 "Begin critical section 0 ") 
        (OutputCoutingFromLatest 0 "Begin critical section 1 "))

debug_mutex_example p = debugger (whileStr (Just p)) finder no_mutual_exclusion_breakpoint ()

-- In the implementation IncorrectMutex1 two threads are forced to alternate, ensuring mutual exclusion, but preventing the same thread from entering its critical section consecutively.
example_Mutex1 = debugger (whileStr (Just P1.p)) finder (
    Or
    (And 
        (OutputCoutingFromLatest 0 "End critical section 0 ") 
        (OutputCoutingFromLatest 1 "Begin critical section 0 "))
    (And 
        (OutputCoutingFromLatest 1 "Begin critical section 1 ") 
        (OutputCoutingFromLatest 0 "End critical section 1 "))) ()

example_Mutex2 = debug_mutex_example P2.p
example_Mutex3 = debug_mutex_example P3.p
example_Mutex4 = debug_mutex_example P4.p

