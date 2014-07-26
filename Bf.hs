module Main where

import Control.Monad.State
import Data.Char
import System.Environment

data Zip a = Zip [a] [a] 
type Arr = Zip Char
type Prog = Zip Char

instance Show a => Show (Zip a) where
  show (Zip l1 l2) = show (reverse l1) ++ " " ++ show l2

toZip :: [a] -> [a] -> Zip a
toZip l1 l2 = Zip (reverse l1) l2
 
mvLft :: Zip a -> Zip a
mvLft zl@(Zip l1 l2) = 
  case l1 of
    (h:l3) -> Zip l3 (h:l2)
    [] -> zl

mvRght :: Zip a -> Zip a
mvRght zl@(Zip l1 l2) = 
  case l2 of
    (h:l3) -> Zip (h:l1) l3
    [] -> zl

jmpToL' n zl@(Zip l1 l2) =
  let zl1 = mvLft zl
  in
  case l1 of
    [] -> zl
    (']' : _) -> jmpToL' (n+1) zl1
    ('[' : _) -> if n == 0
                   then zl1
                   else jmpToL' (n-1) zl1
    _ -> jmpToL' n zl1
 
jmpToL zl = jmpToL' 0 zl


jmpToR' n zl =
  let zl1@(Zip l1 l2) = mvRght zl
  in
  case l2 of
    [] -> zl1
    ('[' : _) -> jmpToR' (n+1) zl1
    (']' : _) -> if n == 0
                   then zl1
                   else jmpToR' (n-1) zl1
    _ -> jmpToR' n zl1
 
jmpToR zl = jmpToR' 0 zl

endP (Zip l1 l2) = (null l1) || (null l2)

bfEx :: StateT (Prog, Arr) IO ()
bfEx = do (p@(Zip p1 p2), ar@(Zip ar1 ar2)) <- get
          if (null p2)           -- || length p1 == 50
            then return ()
            else do
              let pn = mvRght p
              case (head p2) of
                '<' -> put (pn, mvLft ar)
                '>' -> put (pn, mvRght ar)
                '+' -> let (h:ar3) = ar2
                           h3 = chr $ mod (ord h + 1) 256 
                       in put (pn, Zip ar1 (h3:ar3))
                '-' -> let (h:ar3) = ar2
                           h3 = chr $ mod (ord h - 1 + 256) 256 
                       in put (pn, Zip ar1 (h3:ar3))
                '.' -> let (h:ar3) = ar2
                       in do liftIO $ putChar h
                             put (pn, ar)
                ',' -> let (_:ar3) = ar2
                       in do h3 <- liftIO $ getChar
                             put (pn, Zip ar1 (h3:ar3))
                '[' -> let p3 = if (ord $ head ar2) == 0
                                then mvRght $ jmpToR p
                                else pn
                       in put (p3, ar)
                ']' -> let p3 = if (ord $ head ar2) /= 0
                                then mvRght $ jmpToL p
                                else pn
                       in put (p3, ar)
                _ -> put (pn, ar)
              bfEx

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  let usage = "Usage: "++name++" <brainfuck_file>"
  if null args || (head.head) args == '-'
  then putStrLn usage
  else do
    p11 <- readFile $ head args
    let p1 = filter (\x -> elem x ".,<>+-[]") p11
    putStrLn "Program is running..."
    putStrLn "Output:"
    p2 <- runStateT bfEx (toZip "" p1 , toZip "" (replicate 30000 (chr 0)))
    let (q1,Zip q21 q22) = snd p2
    putStrLn "\nYour program:"
    print q1
    putStrLn "\nMewory array (first 200 bytes if long):"
    let q223 = reverse $ dropWhile (== 0) $ reverse (map ord q22)
    print (Zip (take 100 (map ord q21)) (take 100 q223))
