module Exp (
    Exp, (+++), (***), (^^^), parse
) where
import qualified Data.Map                 as Map
import           Data.Maybe
import qualified Data.MultiSet            as MultiSet
import           Development.Placeholders

type Identifier = Char
type Exp = Map.Map Term Int
type Term = MultiSet.MultiSet Atom
data Atom = AVar Identifier | AExp Exp

instance {-# OVERLAPPING #-} Show Exp where
    show e = $(notImplemented)

-- | Add two Expressions
-- >>> parse' "a" +++ parse' "b"
-- a+b
(+++) :: Exp -> Exp -> Exp
e1 +++ e2 = $(notImplemented)

-- |
-- >>> parse' "dead" *** parse' "beef"
-- abde^3f
(***) :: Exp -> Exp -> Exp
e1 *** e2 = e1

-- |
-- >>> parse' "a+b" ^ 2
-- a^2+2ab+b^2
(^^^) :: Exp -> Int -> Exp
e ^^^ i = e

-- |
-- >>> parse' "b^2c+a"
-- a+b^2c
parse :: String -> Maybe Exp
parse = $(notImplemented)

parse':: String -> Exp
parse' = fromJust . parse
