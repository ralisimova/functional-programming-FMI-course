{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}
--import System.Win32 (xBUTTON1, COORD (yPos))
import Data.Char (GeneralCategory(LetterNumber))


--(a+b)
--a*b+3
data Variable 

data Number=Int|Double
data Atom=Variable|Number|Expression|Function Expression
data Member =Atom|Power Atom Member 
data Term=Member|Multiply Member Term|Divide Member Term
data Expression=Term|Add Expression Term|Subtract Expression Term

data Function a=Shorten a|OpenBrackets a|GetGCDInFront a|CommonDivisor a|Diferentiate a|Integrate a
 
 Power 


data BTree a=Empty|Node a (BTree a)(BTree a)
getRoot (Node a _ _)=a
getLeft(Node _ l _)=l
getRight (Node _ _ r)=r
--toTree 
--treeCalculation

simplify :: (Fractional b, Integral b) => b -> b -> (b, b)
simplify a b=(a/gcd a b,b/gcd a b)

--openBrackets _ + exp=


calculate :: BTree Expression -> BTree Expression
calculate exp 
    |(:k exp)==Expression = calculateExpression exp
    |(:k exp)==Term = calculateTerm exp
    |(:k exp)==Member = calculateMember exp
    |(:t exp)==Atom = calculateAtom exp
    |otherwise=  exp
    
calculateExpression :: BTree (Expression) -> BTree a
calculateExpression exp 
    |(:k exp)==Term=calculateTerm exp 
    |getRoot exp==Add= applyAdd (calculateExp (getLeft exp)) calculateTerm (getRight exp )
    |getRoot exp==Subtract=applySubtract (calculateExp (getLeft exp)) calculateTerm (getRight exp)
    |otherwise=Empty

calculateTerm :: BTree (Term) -> BTree a
calculateTerm exp 
    |(:t exp)==Member=calculateMember exp 
    |getRoot exp==Multiply= applyMultiply (calculateMember (getLeft exp)) calculateTerm (getRight exp )
    |getRoot exp==Divide=applyDivide (calculateMember (getLeft exp)) calculateTerm (getRight exp)
    |otherwise =Empty
    
calculateMember :: BTree (Member) -> BTree a
calculateMember exp 
    |(:t exp)==Atom=calculateAtom exp 
    |getRoot exp==Power= applyPower (getLeft exp) getRight exp 
    |otherwise =Empty

calculateAtom :: BTree (Atom) -> BTree a
calculateAtom exp 
    |(:t exp)==Variable||Number= exp 
    |getRoot exp==Function= applyFunction (getLeft exp) getRight exp 
    |getRoot exp==Expression= calculateExpression (getLeft exp) getRight exp 
    |otherwise =Empty

-- applyMultiply x y 
--     |(:t x==Number) &&(:t y==Number)=x*y
--     |
