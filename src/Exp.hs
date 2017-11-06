module Exp (
    Exp, (+++), (***), Exp.parse
) where
import           Data.Function
import           Data.List
import qualified Data.Map                 as Map
import           Data.Maybe
import qualified Data.MultiSet            as MultiSet
import           Data.Tuple
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
    show e | e == Map.empty = "0"
    show e = intercalate "+" . map show . Map.toList $ e
instance {-# OVERLAPPING #-} Show Term where
    show (p, i) | p == MultiSet.empty = show i
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

instance {-# OVERLAPPING #-} Ord Prod where
    p1 <= p2 = on (<=?) MultiSet.toOccurList p1 p2
        where
            (<=?) :: [Pow] -> [Pow] -> Bool
            _ <=? [] = True
            [] <=? _ = False
            ((a1, i1):pw1) <=? ((a2, i2):pw2)
                | a1 < a2 = True
                | a1 > a2 = False
                | i1 > i2 = True
                | i1 < i2 = False
                | otherwise = pw1 <=? pw2

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
    t <- try
        ((\(p, n) -> (p, -n)) $-> spaces *> char '-' *> termP )<|> termP
    ts <- many (try ((char '+' *> termP) <|> (\(p, n)-> (p, -n)) $-> char '-' *> termP))
    return $ Map.filter (/= 0) . Map.fromListWith (+) $ t:ts

termP :: Parser Term
termP = try (swap <$> ((,) <$> naturalP <*> prodP))
        <|> try (do
            n <- naturalP
            char '^'
            i <- naturalP
            return (MultiSet.empty, n^i))
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
e1 *** e2 = Map.fromListWith (+) [(MultiSet.union p1 p2, i1 * i2) | (p1, i1) <- Map.toList e1,
                                                            (p2, i2) <- Map.toList e2]

-- |
-- >>> _parse "b^2c+a"
-- a+b^2c
parse :: String -> Either String Exp
parse = either (Left . show) Right  . Parsec.parse topP []

_parse :: String -> Exp
_parse = (\(Right e) -> e) . Exp.parse

flattenExp :: Exp -> Exp
flattenExp e = Map.unionsWith (+) $
                    map (\(p, i) -> constN i *** flattenProd p) $ Map.toList e

flattenProd :: Prod -> Exp
flattenProd  p = foldr (***) constOne $ concatMap (\(a, i) -> replicate i (flattenAtom a)) $ MultiSet.toOccurList p

flattenAtom :: Atom -> Exp
flattenAtom v@(AVar _) = Map.singleton (MultiSet.insertMany v 1 MultiSet.empty) 1
flattenAtom (AExp e) = flattenExp e

constN :: Int -> Exp
constN = Map.singleton MultiSet.empty

constOne :: Exp
constOne = constN 1
