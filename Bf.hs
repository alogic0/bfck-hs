module Bf where

import Control.Monad.State
import Data.Char

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

code :: StateT (Prog, Arr) IO ()
code = do (p@(Zip p1 p2), ar@(Zip ar1 ar2)) <- get
          if (null p2)           -- || length p1 == 50
            then return ()
            else
              let pn = mvRght p
              in
              case (head p2) of
                '<' -> do put (pn, mvLft ar)
--                          code
                '>' -> do put (pn, mvRght ar)
--                          code
                '+' -> let (h:ar3) = ar2
                           h3 = chr $ mod (ord h + 1) 128 
                       in do put (pn, Zip ar1 (h3:ar3))
--                             code
                '-' -> let (h:ar3) = ar2
                           h3 = chr $ mod (ord h - 1 + 128) 128 
                       in do put (pn, Zip ar1 (h3:ar3))
--                             code
                '.' -> let (h:ar3) = ar2
                       in do liftIO $ putChar h
                             put (pn, ar)
--                             code
                ',' -> let (_:ar3) = ar2
                       in do h3 <- liftIO $ getChar
                             put (pn, Zip ar1 (h3:ar3))
--                             code
                '[' -> let p3 = if (ord $ head ar2) == 0
                                then mvRght $ jmpToR p
                                else pn
                       in do put (p3, ar)
--                             code
                ']' -> let p3 = if (ord $ head ar2) /= 0
                                then mvRght $ jmpToL p
                                else pn
                       in do put (p3, ar)
--                             code
                _ -> do put (pn, ar)
--                        code
