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
intToString s=(show s)

data BinaryTree a
  = Node a (BinaryTree a) (BinaryTree a)
  | Leaf



data Expr
  = IntLit Int   
  | Variable String          
  | Add    Expr Expr    
  | Mult   Expr Expr 
  | Subtract   Expr Expr 
  | Divide   Expr Expr
  | Function Expr --change to expr
  | Power   Expr Expr 
  deriving (Show)
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
       Just (expr, (CloseTok : restTokens2)) -> Just (expr, restTokens2)
       Just _  -> Nothing -- no closing paren
       Nothing -> Nothing
parseIntOrParenExpr tokens
  = Nothing
      
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

data Constant= Int Int| String String

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


 
evalString :: Expr -> String
evalString (IntLit n) = intToString n
evalString (Variable n) =  n
evalString (Add (IntLit expr1) (IntLit expr2))
  = intToString $ evalInt(Add (IntLit expr1) (IntLit expr2))
evalString (Add (Variable expr1) (Variable expr2))
  = expr1 ++"+"++expr2
evalString (Add (IntLit expr1) (Variable expr2))
  = intToString expr1 ++"+"++expr2
evalString (Add (Variable expr1) (IntLit expr2))
  = expr1 ++"+"++ intToString expr2

evalString (Subtract (IntLit expr1) (IntLit expr2))
  = intToString $ evalInt(Subtract (IntLit expr1) (IntLit expr2))
evalString (Subtract (Variable expr1) (Variable expr2))
  = expr1 ++"+"++expr2
evalString (Subtract (IntLit expr1) (Variable expr2))
  = intToString expr1 ++"+"++expr2
evalString (Subtract (Variable expr1) (IntLit expr2))
  = expr1 ++"+"++ intToString expr2

evalString (Mult (IntLit expr1) (IntLit expr2))
  = intToString $ evalInt(Mult (IntLit expr1) (IntLit expr2))
evalString (Mult (Variable expr1) (Variable expr2))
  = expr1 ++"+"++expr2
evalString (Mult (IntLit expr1) (Variable expr2))
  = intToString expr1 ++"+"++expr2
evalString (Mult (Variable expr1) (IntLit expr2))
  = expr1 ++"+"++ intToString expr2

evalString (Divide (IntLit expr1) (IntLit expr2))
  = intToString $ evalInt(Divide (IntLit expr1) (IntLit expr2))
evalString (Divide (Variable expr1) (Variable expr2))
  = expr1 ++"+"++expr2
evalString (Divide (IntLit expr1) (Variable expr2))
  = intToString expr1 ++"+"++expr2
evalString (Divide (Variable expr1) (IntLit expr2))
  = expr1 ++"+"++ intToString expr2


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