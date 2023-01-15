import Data.Char
import System.Win32 (COORD(xPos))

data Token
  = PlusTok
  | TimesTok
  | SubtractTok
  | DivideTok
  | PowerTok
  | OpenTok
  | CloseTok
  | IntTok Int
  |VariableTok String
  |FunctionTok String
  deriving (Show)



lexer :: String -> [Token]
lexer []              = []
lexer ('+' : restStr) = PlusTok  : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr 
lexer ('-' : restStr) = SubtractTok   : lexer restStr
lexer ('/' : restStr) = DivideTok   : lexer restStr
lexer ('^' : restStr) = PowerTok   : lexer restStr
lexer ('(' : restStr) = OpenTok    : lexer restStr 
lexer (')' : restStr) = CloseTok   : lexer restStr
lexer (chr : restStr) 
  | isSpace chr       = lexer restStr
lexer str@(chr : _) 
  | isDigit chr = IntTok (stringToInt digitStr) : lexer restStrDigit
  | isUpper chr  =VariableTok varStr : lexer restStr 
  | isLetter chr  =FunctionTok  varStr :  lexer restStr
  where
     (digitStr, restStrDigit) = break (not . isDigit) str
     (varStr, restStr) = break (not . isLetter) str
     -- local function to convert a string to an integer value
stringToInt :: String -> Int
stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0    


intToString::Int->String
intToString s=show s

-- data BinaryTree a
--   = Node a (BinaryTree a) (BinaryTree a)
--   | Leaf



data Expr
  = IntLit Int   
  | Variable String          
  | Add    Expr Expr    
  | Mult   Expr Expr 
  | Subtract   Expr Expr 
  | Divide   Expr Expr
  | Function Expr --change to expr
  | Power   Expr Expr 
  |Brackets Expr
  deriving (Show,Eq)
--data Operations= Power

parseInt :: [Token] -> Maybe (Expr, [Token])
parseInt (IntTok n : restTokens)
  = Just (IntLit n, restTokens)
parseInt (VariableTok n : restTokens)
  = Just (Variable n, restTokens)
parseInt tokens
  = Nothing

parseFunctionOrInt :: [Token] -> Maybe (Expr, [Token])
parseFunctionOrInt (FunctionTok n : restTokens)=
   case parseInt restTokens of
            Just (expr2, restTokens2) -> Just (Function expr2, restTokens2)
            Nothing                   -> Nothing
parseFunctionOrInt tokens= parseSumOrProdOrIntOrParenExpr tokens          


parsePowerOrInt :: [Token] -> Maybe (Expr, [Token])
parsePowerOrInt tokens
  = case parseFunctionOrInt tokens of
      Just (expr1, (PowerTok : restTokens1)) -> 
          case parseFunctionOrInt restTokens1 of
            Just (expr2, restTokens2) -> Just (Power expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result
   

-- parseFunctionOrInt :: [Token] -> Maybe (Expr, [Token])
-- parseFunctionOrInt tokens
--   = case parseInt tokens of
--       Just (expr1,FunctionTok n : restTokens1) -> 
--           case parseProdOrIntOrParenExpr restTokens1 of
--             Just (expr2, restTokens2) -> Just (Function expr2 , restTokens2)
--             Nothing                   -> Nothing
--       result -> result
parseProdOrInt :: [Token] -> Maybe (Expr, [Token])
parseProdOrInt tokens
  = case parsePowerOrInt tokens of
      Just (expr1, (TimesTok : restTokens1)) -> 
          case parseProdOrInt restTokens1 of
            Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      Just (expr1, (DivideTok : restTokens1)) -> 
          case parseProdOrInt restTokens1 of
            Just (expr2, restTokens2) -> Just (Divide expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result

     

parseSumOrProdOrInt :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrInt tokens
  = case parseProdOrInt tokens of
      Just (expr1, (PlusTok : restTokens1)) -> 
          case parseProdOrInt restTokens1 of
            Just (expr2, restTokens2) -> Just (Add expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      Just (expr1, (SubtractTok : restTokens1)) -> 
          case parseProdOrInt restTokens1 of
            Just (expr2, restTokens2) -> Just (Subtract expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result

parseIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseIntOrParenExpr (IntTok n : restTokens)
  = Just (IntLit n,   restTokens)
parseIntOrParenExpr (VariableTok n : restTokens)
  = Just (Variable n,   restTokens)
-- parseIntOrParenExpr (FunctionTok n : restTokens)=
--    case parseSumOrProdOrIntOrParenExpr restTokens of
--             Just (expr2, restTokens2) -> Just (Function expr2, restTokens2)
--             Nothing                   -> Nothing
parseIntOrParenExpr (OpenTok : restTokens1)
  = case parseSumOrProdOrIntOrParenExpr restTokens1 of
       Just (expr, (CloseTok : restTokens2)) -> Just (Brackets expr, restTokens2)
       Just _  -> Nothing -- no closing paren
       Nothing -> Nothing
parseIntOrParenExpr tokens
  = Nothing
-- parseBracketsOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
-- parseBracketsOrIntOrParenExpr (FunctionTok n : restTokens)=
--      case parseIntOrParenExpr restTokens of
--        Just (expr2, restTokens2) -> Just (Function expr2, restTokens2)
--        Nothing                   -> Nothing
-- parseFunctionOrIntOrParenExpr tokens= parseIntOrParenExpr tokens

parseFunctionOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseFunctionOrIntOrParenExpr (FunctionTok n : restTokens)=
     case parseIntOrParenExpr restTokens of
       Just (expr2, restTokens2) -> Just (Function expr2, restTokens2)
       Nothing                   -> Nothing
parseFunctionOrIntOrParenExpr tokens= parseIntOrParenExpr tokens  


parsePowerOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parsePowerOrIntOrParenExpr tokens
  = case parseFunctionOrIntOrParenExpr tokens of
      Just (expr1, (PowerTok : restTokens1)) -> 
          case parsePowerOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Power expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result   

parseProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseProdOrIntOrParenExpr tokens
  = case parsePowerOrIntOrParenExpr tokens of
      Just (expr1, (TimesTok : restTokens1)) -> 
          case parseProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      Just (expr1, (DivideTok : restTokens1)) -> 
          case parseProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Divide expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result   
              

parseSumOrProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrIntOrParenExpr tokens
  = case parseProdOrIntOrParenExpr tokens of
      Just (expr1, (PlusTok : restTokens1)) -> 
          case parseSumOrProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Add expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      Just (expr1, (SubtractTok : restTokens1)) -> 
          case parseSumOrProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Subtract expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result


parse :: [Token] -> Expr
parse tokens =
  case parseSumOrProdOrIntOrParenExpr tokens of
    Just (expr, []) -> expr    
    _               -> error "Could not parse input"

--data Constant= Int Int| String String

evalInt :: Expr -> Int
evalInt (IntLit n) = n
evalInt (Add expr1 expr2)
  = evalInt expr1 + evalInt expr2
evalInt (Mult expr1 expr2)
  = evalInt expr1 * evalInt expr2 
evalInt (Subtract expr1 expr2)
  = evalInt expr1 - evalInt expr2 
evalInt (Divide expr1 expr2)
  = evalInt expr1 `div` evalInt expr2 
evalInt (Power expr1 expr2)
  =  evalInt expr1 ^ evalInt expr2
evalInt (Function expr1)
  = (^2) $ evalInt expr1 


 
-- evalString :: Expr -> String
-- evalString (IntLit n) = intToString n
-- evalString (Variable n) =  n
-- -- evalString (Add expr1  (Brackets expr2))
-- --   =  (evalString expr1) ++"+"++ (evalString expr2)
-- -- evalString (Add (Brackets expr1)  expr2)
-- --   =  (evalString expr1) ++"+"++ (evalString expr2)
-- evalString (Add (IntLit expr1) (IntLit expr2))
--   = intToString $ evalInt(Add (IntLit expr1) (IntLit expr2))
-- evalString (Add (Variable expr1) (Variable expr2))=if(expr1==expr2)then evalString (Mult (IntLit 2) (Variable expr1 )) else expr1 ++"+"++expr2
--   -- =  expr1 ++"+"++expr2
-- evalString (Add (IntLit expr1) (Variable expr2))
--   =   intToString expr1  ++"+"++ expr2
-- evalString (Add (Variable expr1) (IntLit expr2))
--   =  expr1 ++"+"++intToString expr2
-- evalString (Add expr1  expr2)
--   =  (evalString expr1) ++"+"++ (evalString expr2)


-- evalString (Subtract (IntLit expr1) (IntLit expr2))
--   = intToString $ evalInt(Subtract (IntLit expr1) (IntLit expr2))
-- evalString (Subtract (Variable expr1) (Variable expr2))=if(expr1==expr2)then "0" else expr1 ++"-"++expr2

-- -- evalString (Subtract (Variable expr1) (Variable expr2))
-- --   = expr1 ++"-"++expr2
-- evalString (Subtract (IntLit expr1) (Variable expr2))
--   = intToString expr1 ++"-"++expr2
-- evalString (Subtract (Variable expr1) (IntLit expr2))
--   = expr1 ++"-"++ intToString expr2
-- evalString (Subtract expr1  expr2)
--   =  (evalString expr1) ++"-"++ (evalString expr2)

-- evalString (Mult (IntLit 0) _)
--   = "0"
-- evalString (Mult _ (IntLit 0))
--   = "0"
-- evalString (Mult expr(IntLit 1))
--   = evalString expr
-- evalString (Mult (IntLit 1) expr)
--   = evalString expr
-- evalString (Mult (IntLit expr1) (IntLit expr2))
--   = intToString $ evalInt(Mult (IntLit expr1) (IntLit expr2))
-- evalString (Mult (Variable expr1) (Variable expr2))
--   = expr1 ++"*"++expr2
-- evalString (Mult (IntLit expr1) (Variable expr2))
--   = intToString expr1 ++"*"++expr2
-- evalString (Mult (Variable expr1) (IntLit expr2))
--   = expr1 ++"*"++ intToString expr2
-- evalString (Mult expr1  expr2)
--   =  (evalString expr1) ++"*"++ (evalString expr2)

-- evalString (Divide expr(IntLit 1))
--   = evalString expr
-- evalString (Divide (IntLit expr1) (IntLit expr2))
--   = intToString $ evalInt(Divide (IntLit expr1) (IntLit expr2))
-- evalString (Divide (Variable expr1) (Variable expr2))=if(expr1==expr2)then "1" else expr1 ++"/"++expr2
-- --   = expr1 ++"/"++expr2
-- -- evalString (Divide (Variable expr1) (Variable expr2))
-- --   = expr1 ++"/"++expr2
-- evalString (Divide (IntLit expr1) (Variable expr2))
--   = intToString expr1 ++"/"++expr2
-- evalString (Divide (Variable expr1) (IntLit expr2))
--   = expr1 ++"/"++ intToString expr2
-- evalString (Divide expr1  expr2)
--   =  (evalString expr1) ++"/"++ (evalString expr2)



evalExpr :: Expr -> Expr
evalExpr (IntLit n) =(IntLit n)
evalExpr (Variable n) =(Variable n)
evalExpr (Brackets expr)=expr

evalExpr (Add expr1 (IntLit 0))= evalExpr expr1
evalExpr (Add  (IntLit 0) expr1)= evalExpr expr1
evalExpr (Add expr1 (Brackets expr2))= Add  expr1 (evalExpr expr2)
evalExpr (Add  (Brackets expr2) expr1)= Add   (evalExpr expr2)expr1

--evalExpr (Add (Brackets expr2)(IntLit expr1) )=  (mapOp (Mult (IntLit expr1)) (evalExpr expr2))
evalExpr (Add (IntLit expr1) (IntLit expr2))
  =  IntLit (evalInt(Add (IntLit expr1) (IntLit expr2)))
evalExpr (Add (Variable expr1) (Variable expr2))=if(expr1==expr2)then (Mult (IntLit 2) (Variable expr1 )) else (Add (Variable expr1) (Variable expr2))

evalExpr (Add (IntLit expr1) (Variable expr2))
  = (Add  (Variable expr2)(IntLit expr1))
evalExpr (Add (Variable expr1) (IntLit expr2))
  = (Add (Variable expr1) (IntLit expr2))
evalExpr (Add expr1  expr2)
  =   (Add (evalExpr expr1) (evalExpr expr2))

evalExpr (Subtract expr1 (IntLit 0))=evalExpr expr1
evalExpr (Subtract (IntLit expr1) (IntLit expr2))
  =  IntLit (evalInt(Subtract (IntLit expr1) (IntLit expr2)))
evalExpr (Subtract (Variable expr1) (Variable expr2))=if(expr1==expr2)then IntLit 0 else  (Subtract (Variable expr1) (Variable expr2))
--Brackets?????
evalExpr (Subtract (IntLit expr1) (Variable expr2))
  = (Subtract (IntLit expr1) (Variable expr2))
evalExpr (Subtract (Variable expr1) (IntLit expr2))
  = (Subtract (Variable expr1) (IntLit expr2))
evalExpr (Subtract expr1  expr2)
  = (Subtract (evalExpr expr1) (evalExpr expr2))

evalExpr (Mult (IntLit 0) _)
  = (IntLit 0)
evalExpr (Mult _ (IntLit 0))
  = (IntLit 0)
evalExpr (Mult expr1 (Brackets expr2))=  (mapOp (Mult expr1) (evalExpr expr2))
evalExpr (Mult (Brackets expr1)expr2 )=  (mapOp (Mult expr2) (evalExpr expr1))

evalExpr (Mult (IntLit expr1) (IntLit expr2))
  = evalExpr $ IntLit $ evalInt(Mult (IntLit expr1) (IntLit expr2))
evalExpr (Mult (Variable expr1) (Variable expr2))=if(expr1==expr2)then (Power (Variable expr1 ) (IntLit 2)) else (Mult (Variable expr1) (Variable expr2))
--use Power???/
evalExpr (Mult (IntLit expr1) (Variable expr2))
  = (Mult (IntLit expr1) (Variable expr2))
evalExpr (Mult (Variable expr1) (IntLit expr2))
  = (Mult (IntLit expr2) (Variable expr1))
evalExpr (Mult expr1  expr2)
  =   (Mult (evalExpr expr1)(evalExpr  expr2))

evalExpr (Divide expr1 (IntLit 1))= evalExpr expr1
evalExpr (Divide  (Brackets expr1)(Brackets expr2))= ( Divide (evalExpr expr1)(evalExpr expr1))
evalExpr (Divide expr1 (Brackets expr2))= (Divide  expr1 (evalExpr expr2))
evalExpr (Divide  (Brackets expr1) expr2)= ( Divide (evalExpr expr1)expr2)
evalExpr (Divide (IntLit expr1) (IntLit expr2))
  =  evalExpr $ IntLit $ evalInt(Divide (IntLit expr1) (IntLit expr2))
evalExpr (Divide (Variable expr1) (Variable expr2))=if(expr1==expr2)then IntLit 1 else (Divide (Variable expr1) (Variable expr2))
evalExpr (Divide (IntLit expr1) (Variable expr2))
  = (Divide (IntLit expr1) (Variable expr2))
evalExpr (Divide (Variable expr1) (IntLit expr2))
  = (Divide (Variable expr1) (IntLit expr2))
evalExpr (Divide expr1  expr2)
  =  (Divide (evalExpr expr1)(evalExpr  expr2))

evalExpr (Power expr1 (IntLit 0))= IntLit 1
evalExpr (Power (IntLit 1) _)= IntLit 1
evalExpr (Power (IntLit 0) _)= IntLit 0
evalExpr (Power expr1 (IntLit 1))= evalExpr expr1
evalExpr (Power  (Brackets expr1)(Brackets expr2))= ( Power (evalExpr expr1)(evalExpr expr1))
evalExpr (Power expr1 (Brackets expr2))= (Power  expr1 (evalExpr expr2))
evalExpr (Power  (Brackets expr1) expr2)= ( Power (evalExpr expr1)expr2)
evalExpr (Power (IntLit expr1) (IntLit expr2))
  =   IntLit $ evalInt(Power (IntLit expr1) (IntLit expr2))
evalExpr (Power (Variable expr1) (Variable expr2))=Power (Variable expr1) (Variable expr2)
evalExpr (Power (IntLit expr1) (Variable expr2))
  = Power (IntLit expr1) (Variable expr2)
evalExpr (Power (Variable expr1) (IntLit expr2))
  = Power (Variable expr1) (IntLit expr2)
evalExpr (Power expr1  expr2)
  =  Power (evalExpr expr1)(evalExpr expr2)


-- evalString (Mult expr1 expr2)
--   = evalInt expr1 * evalInt expr2 
-- evalInt (Subtract expr1 expr2)
--   = evalInt expr1 - evalInt expr2 
-- evalInt (Divide expr1 expr2)
--   = evalInt expr1 `div` evalInt expr2 
-- evalInt (Power expr1 expr2)
--   =  evalInt expr1 ^ evalInt expr2
-- evalInt (Function expr1)
--   = (^2) $ evalInt expr1 

-- eval :: Expr -> Expr
-- eval (IntLit n) =  IntLit n
-- eval (Add expr1 expr2)
--   = eval expr1 + eval expr2
-- eval (Mult expr1 expr2)
--   = eval expr1 * eval expr2 
-- eval (Subtract expr1 expr2)
--   = eval expr1 - eval expr2 
-- eval (Divide expr1 expr2)
--   = eval expr1 `div` eval expr2 
-- eval (Power expr1 expr2)
--   =  eval expr1 ^ eval expr2
-- eval (Function expr1)
--   = (^2) $ eval expr1 

-- getGCDInFront :: Expr->Expr
-- getGCDInFront (Add (IntLit n1) (IntLit n2))=Mult  (IntLit gcd(n1,n2))(Add(IntLit(n1/gcd(n1,n2)))(IntLit (n2/gcd(n1,n2))))
-- getGCDInFront (Subtract (IntLit n1) (IntLit n2))=Mult  (IntLit gcd(n1,n2))(Subtract(IntLit(n1/gcd(n1,n2)))(IntLit (n2/gcd(n1,n2))))
-- getGCDInFront (Subtract (Variable n1) (Variable n2))=if(n1==n2)then Mult  (IntLit gcd(n1,n2))(Subtract(IntLit(n1/gcd(n1,n2)))(IntLit (n2/gcd(n1,n2))))
--getGCDInFront (Subtract (IntLit n1) (IntLit n2))=Mult  (IntLit gcd(n1,n2))(Subtract(IntLit(n1/gcd(n1,n2)))(IntLit (n2/gcd(n1,n2))))
getGCDInFront :: Expr->Expr
--getGCDInFront(Add (Variable n1) (Variable n2))=(Mult )
getGCDInFront (Add e1 e2)=
  Mult (gcdExpr e1 e2)(Brackets  (evalExpr(Add (Divide e1 (gcdExpr e1 e2))(Divide e2 (gcdExpr e1 e2)))))
getGCDInFront (Subtract e1 e2)=
  Mult (gcdExpr e1 e2)(Brackets  (evalExpr(Subtract (Divide e1 (gcdExpr e1 e2))(Divide e2 (gcdExpr e1 e2)))))
-- getGCDInFront (Add e1@(Mult  (IntLit n1)(Variable v1)) e2@(Mult(IntLit n2)(Variable v2)))=
--   Mult (gcdExpr e1 e2)(Brackets (evalExpr (Add (Divide e1 (gcdExpr e1 e2))(Divide e2 (gcdExpr e1 e2)))))

gcdExpr :: Expr->Expr->Expr
gcdExpr (IntLit n1) (IntLit n2)=IntLit (gcd n1 n2)
gcdExpr (Variable n1) (Variable n2)=if(n1==n2)then (Variable n1)else(IntLit 1)
gcdExpr (IntLit n1) (Variable n2)=IntLit 1 
gcdExpr (Variable n1)(IntLit n2)=IntLit 1 
gcdExpr (IntLit n1) (Mult  (IntLit n2)(Variable v))=IntLit (gcd n1 n2)
gcdExpr (Mult  (IntLit n1)(Variable v)) (IntLit n2)=IntLit (gcd n1 n2)
gcdExpr (Mult  (IntLit n1)(Variable v1)) (Mult(IntLit n2)(Variable v2))=Mult (IntLit (gcd n1 n2))  (gcdExpr (Variable v1) (Variable v2))
gcdExpr expr1 expr2=IntLit 1--is it true???



mapOp op (IntLit n)=op (IntLit n)
mapOp op (Variable n)=op (Variable n)
mapOp op (Add expr1 expr2)=Add (op expr1)(op expr2)
mapOp op (Subtract expr1 expr2)=Subtract (op expr1)(op expr2)
mapOp op (Mult expr1 expr2)=Mult (op expr1)(op expr2)
mapOp op (Divide expr1 expr2)=Divide (op expr1)(op expr2)


lcmExpr::Expr->Expr->Expr
-- lcmExpr (Variable n1) (Variable n2)=if(n1==n2)then (Variable n1)else (Mult (Variable n1) (Variable n2))
-- lcmExpr (Mult  (IntLit n1)(Variable v1)) (Mult(IntLit n2)(Variable v2))=Mult (IntLit (gcd n1 n2))  (gcdExpr (Variable v1) (Variable v2))   
lcmExpr (IntLit n1) (IntLit n2)=IntLit (lcm n1 n2)
lcmExpr expr1 expr2=Divide (Mult expr1 expr2)(gcdExpr expr1 expr2)

commonDivisor::Expr->Expr
commonDivisor(Add (Divide expr1 expr2) (Divide expr3 expr4))=
  Divide(Brackets(Add (Mult expr1 (Divide (lcmExpr expr2 expr4)expr2))(Mult expr3 (Divide  (lcmExpr expr2 expr4)expr4))))
  (lcmExpr expr2 expr4)

shortenDivision :: Expr -> Expr
shortenDivision (Divide (Brackets expr1) expr2)=(Divide (Brackets (evalExpr(Divide expr1 (gcdExpr expr1 expr2))))(evalExpr(Brackets(Divide expr2( gcdExpr expr1 expr2)))))
shortenDivision (Divide expr1 (Brackets expr2))=(Divide (Divide expr1 (gcdExpr expr1 expr2))(Divide expr2( gcdExpr expr1 expr2)))
shortenDivision (Divide expr1 expr2)=(Divide (evalExpr(Divide expr1 (gcdExpr expr1 expr2)))(evalExpr(Divide expr2( gcdExpr expr1 expr2))))



exprToString::Expr->String
exprToString (IntLit n) = intToString n
exprToString (Variable n) =  n
exprToString (Brackets expr) ="("++ exprToString expr++")"  
exprToString (Add expr1 expr2)=exprToString expr1 ++"+"++exprToString expr2
exprToString (Subtract expr1 expr2)=exprToString expr1 ++"-"++exprToString expr2
exprToString (Mult expr1 expr2)=exprToString expr1 ++"*"++exprToString expr2
exprToString (Divide expr1 expr2)=exprToString expr1 ++"/"++exprToString expr2
exprToString (Power expr1 expr2)=exprToString expr1 ++"^"++exprToString expr2

evalLoop expr=if expr==(evalExpr expr)then expr else evalLoop $ evalExpr expr

