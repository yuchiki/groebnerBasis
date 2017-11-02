module Exp (
    Exp, (+++), (***), (^^^), Exp.parse
) where
import qualified Data.Map                 as Map
import           Data.Maybe
import qualified Data.MultiSet            as MultiSet
import           Development.Placeholders
import           Text.Parsec              as Parsec
import           Text.Parsec.String

type Identifier = Char
type Exp = Map.Map Prod Int
type Term = (Prod, Int)
type Prod = MultiSet.MultiSet Atom
type Pow = Prod
data Atom = AVar Identifier | AExp Exp deriving(Eq, Ord)

instance {-# OVERLAPPING #-} Show Exp where
    show e = $(notImplemented)

{--
    exp  ::= term (+ term | - term)* eof
    term ::= natural prod | prod | natural
    prod ::= pow+
    pow ::= atom ^ natural | atom
    atom ::= identifier | '(' exp ')'
--}

($->) :: Functor f => (a -> b) -> f a -> f b
($->) = (<$>)
infixr 2 $->

topP :: Parser Exp
topP = expP <* eof

expP :: Parser Exp
expP = do
    t <- termP
    ts <- many (char '+' *> termP <|> (\(p, n)-> (p, -n)) $-> char '-' *> termP)
    return $ Map.fromListWith (+) (t:ts)

termP :: Parser Term
termP = try ((,) <$> prodP <*> naturalP)
        <|> (MultiSet.empty,) $-> naturalP
        <|> (, 1) $-> prodP

prodP :: Parser Prod
prodP = MultiSet.unions $-> many1 powP

powP :: Parser Pow
powP =
    do
        a <- atomP
        char '^'
        n <- naturalP
        return $ MultiSet.insertMany a n MultiSet.empty
    <|> MultiSet.singleton $-> atomP

atomP :: Parser Atom
atomP = ham $
            AVar $-> letter <|>
            AExp $-> char '(' *> expP <* char ')'

naturalP :: Parser Int
naturalP = read $-> ham (many1 digit)

ham :: Parser a -> Parser a
ham = (spaces *>) . (<* spaces)

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
parse = either (const Nothing) Just . Parsec.parse topP []

parse':: String -> Exp
parse' = fromJust . Exp.parse
