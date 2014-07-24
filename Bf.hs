module Bf where

import Control.Monad.State

data Zip a = Zip [a] [a] 
type Arr = Zip Integer
type Prog = Zip String

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
    [] -> error "No matched [ found"
    (']' : _) -> jmpToL' (n+1) zl1
    ('[' : _) -> if n == 0
                   then zl1
                   else jmpToL' (n-1) zl1
    _ -> jmpToL' n zl1
 
jmpToL zl = jmpToL' 0 zl

