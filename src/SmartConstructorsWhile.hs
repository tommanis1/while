module SmartConstructorsWhile where
import While

(⍮) :: Command -> Command -> Command
c1 ⍮ c2 = Seq c1 c2

s :: String -> Expr
s = LitExpr . LitString

i = LitExpr . LitInt

while :: Expr -> Command -> Command
while e c = While (LitExpr $ LitBool True) e c