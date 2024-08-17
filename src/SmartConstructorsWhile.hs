module SmartConstructorsWhile where
import While

(⍮) :: Command -> Command -> Command
c1 ⍮ c2 = Seq c1 c2

s :: String -> Expr
s = LitExpr . LitString

i = LitExpr . LitInt


b = LitExpr . LitBool

while :: Expr -> Command -> Command
while e c = While e e c