module Exp (
    Exp, (+++), (***), Exp.parse
) where
import           Data.List
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
type Pow = (Atom, Int)
data Atom = AVar Identifier | AExp Exp deriving(Eq, Ord)

instance {-# OVERLAPPING #-} Show Exp where
    show = intercalate "+" . map show . Map.toList
instance {-# OVERLAPPING #-} Show Term where
    show (p, 1) = show p
    show (p, i) = show i ++ show p
instance {-# OVERLAPPING #-} Show Prod where
    show = concatMap show . MultiSet.toOccurList
instance {-# OVERLAPPING #-} Show Pow where
    show (a, 1) = show a
    show (a, n) = show a ++ "^" ++ show n
instance Show Atom where
    show (AVar i) = [i]
    show (AExp e) = "(" ++ show e ++ ")"

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
topP = flattenExp $-> expP <* eof

expP :: Parser Exp
expP = do
    t <- termP
    ts <- many (try (char '+' *> termP) <|> (\(p, n)-> (p, -n)) $-> char '-' *> termP)
    return $ Map.filter (> 0) . Map.fromListWith (+) $ t:ts

termP :: Parser Term
termP = try ((,) <$> prodP <*> naturalP)
        <|> try ((MultiSet.empty,) $-> naturalP)
        <|> (, 1) $-> prodP

prodP :: Parser Prod
prodP = MultiSet.unions $-> many1 powP

powP :: Parser Prod
powP =
    try (do
        a <- atomP
        char '^'
        n <- naturalP
        return $ MultiSet.insertMany a n MultiSet.empty)
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
-- >>> _parse "a" +++ _parse "b"
-- a+b
(+++) :: Exp -> Exp -> Exp
(+++) = Map.unionWith (+)

-- |
-- >>> _parse "dead" *** _parse "beef"
-- abd^2e^3f
(***) :: Exp -> Exp -> Exp
e1 *** e2 = Map.fromList [(MultiSet.union p1 p2, i1 * i2) | (p1, i1) <- Map.toList e1,
                                                            (p2, i2) <- Map.toList e2]

-- |
-- >>> _parse "b^2c+a"
-- a+b^2c
parse :: String -> Either String Exp
parse = either (Left . show) Right  . Parsec.parse topP []

_parse :: String -> Exp
_parse = (\(Right e) -> e) . Exp.parse

flattenExp :: Exp -> Exp
flattenExp e = Map.unions $
                    map (\(p, i) -> Map.map (* i) (flattenProd p)) $ Map.toList e

flattenProd :: Prod -> Exp
flattenProd  p = foldr (***) constOne $ concatMap (\(a, i) -> replicate i (flattenAtom a)) $ MultiSet.toOccurList p

flattenAtom :: Atom -> Exp
flattenAtom v@(AVar _) = Map.singleton (MultiSet.insertMany v 1 MultiSet.empty) 1
flattenAtom (AExp e) = flattenExp e

constOne :: Exp
constOne = Map.singleton MultiSet.empty 1
