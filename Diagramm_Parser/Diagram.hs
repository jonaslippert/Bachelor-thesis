{-

Bedingungen an das Diagramm:
-> \ar[optional]{direction}[optional]{name}
-> Keine Kreise
-> Pfeilrichtung muss in folgender Reihenfolge angegebene werden: l-u-r-d
-> Parallele Pfade werden als gleich betrachtet


-}


module SAD.Parser.Diagram (
  tikzcdP) where

import Control.Applicative
import Data.List
import Data.List.Split 
import Data.Maybe


data Arrow = Arrow
  { name   :: String,
    source :: Position,
    target :: Position
  }
  deriving Show

instance Eq Arrow where
  (==) a b = source a == source b && target a == target b

data Diagram = Diagram
  { arrows :: [Arrow]
  }
  deriving Show

type Position = (Int, Int)
initpos :: Position
initpos = (0,0)


newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

diagramP :: Parser Diagram
diagramP = Diagram <$> many arP


instance Functor Parser where  
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where   
  pure x = Parser $ \input -> Just (input, x)
  (Parser p) <*> (Parser q) = 
    Parser $ \input -> do
      (input', f) <- p input
      (input'', a) <- q input'
      Just (input'', f a)

instance Alternative Parser where  
  empty = Parser $ \_ -> Nothing
  (Parser p) <|> (Parser q) =
    Parser $ \input -> p input <|> q input 


arP :: Parser Arrow
arP = Parser $ \input -> do
  let pos = pstn input
  (input1, rest)  <- runParser (spanP (/= '{')) input
  let newpos = src rest pos
  (input2, _)    <- runParser (charP '{') input1
  (input3, dr)    <- runParser (spanP (/= '}')) input2
  --(input4, x4)    <- runParser (charP '}') input3
  (input5, _)    <- runParser (spanP (/= '{')) input3
  (input6, _)    <- runParser (charP '{') input5
  (input7, name)  <- runParser (spanP (/= '}')) input6
  (input8, _)    <- runParser (charP '}') input7
  case name /= "" of
    True  -> Just (show newpos ++ input8, Arrow name newpos (trgt dr newpos))
    _     -> Nothing

pstn :: String -> Position
pstn input@(x:xs) =
  if x == '(' then
    let (a, rest) = span (/= ',') xs in
    let (b, rest') = span (/= ')') (tail rest) in
      (read a, read b)
  else initpos

src :: String -> Position -> Position
src s pos@(a,b) =
  let s1 = splitOn "\\\\" s in
  let cl = length $ filter (=='&') $ last s1 in
    case length s1 of
      1 ->  (a,b+cl)
      _ ->  (a-(length s1)+1,cl)

trgt :: String -> Position -> Position
trgt lurd pos@(a,b) =
  let (lur,d) = span (/= 'd') lurd in
    let (lu,r) = span (/= 'r') lur in
      let (l,u) = span (/= 'u') lu in
        (a + (length u) - (length d), b + (length r) - (length l))


charP :: Char -> Parser String
charP x = Parser $ \input -> do
  case input of
    y:ys
      | y == x -> Just (ys, [])
      | otherwise -> Nothing
    _ -> Nothing 

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> 
  let (token,rest) = span f input
  in Just (rest, token)

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)


mkPaths1 :: Arrow -> Arrow -> [Arrow] -- Kreise nicht erlaubt
mkPaths1 a x 
  | source x == target a  = [Arrow ((name x) ++ " \\mcirc " ++ (name a)) (source a) (target x)]
  | source a == target x  = [Arrow ((name a) ++ " \\mcirc " ++ (name x)) (source x) (target a)]
  | otherwise             = [] 

mkPaths2 :: Arrow -> Diagram -> Diagram
mkPaths2 x d = case arrows d of
  [] -> Diagram []
  (y:ys) -> Diagram $ mkPaths1 x y ++ arrows (mkPaths2 x (Diagram ys))

mkPaths :: Diagram -> Diagram
mkPaths d = case arrows d of
  [] -> Diagram []
  (x:xs) -> Diagram $ x : arrows (mkPaths (Diagram (xs ++ newA)))
    where newA = arrows $ mkPaths2 x $ Diagram xs


mkEqtClass1 :: (Eq a) => a -> a -> [a]
mkEqtClass1 x y 
  | x == y    = [y]
  | otherwise = []

mkEqtClass2 :: (Eq a) => a -> [a] -> [a]
mkEqtClass2 x xs = case xs of
  [] -> [x]
  (y:ys) -> mkEqtClass2 x ys ++ mkEqtClass1 x y 

mkEqtClass :: (Eq a) => [a] -> [[a]]
mkEqtClass xs = case xs of
  [] -> []
  (y:ys) -> z : mkEqtClass (ys \\ z)
    where z = (mkEqtClass2 y ys)


printEq1 :: [Arrow] -> String
printEq1 l = case l of
  [] -> ""
  [x] -> name x
  (x:xs) -> name x ++ " = " ++ printEq1 xs

printEq :: [[Arrow]] -> String
printEq a = case filter (\l -> length l >= 2) a of
  [] -> ""
  [l] -> printEq1 l -- ++ "."
  (l:ls) -> printEq1 l ++ " and " ++ printEq ls


tikzcdP :: String -> String
tikzcdP a = maybe "Error: No proper diagram found." (printEq . mkEqtClass . arrows . mkPaths . snd) (runParser diagramP a) 


--test = tikzcdP "A \\ar{r}{f} \\ar{dd}[swap]{h} \\ar{rdd}{\\Phi} \\ar[bend left=40]{rr}{j} & B \\ar{dd}{g} & C \\ar{dd}{k} \\\\\\\\ D & E \\ar{r}{i} & F"







