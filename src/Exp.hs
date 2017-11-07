module Exp (
    Exp, (+++), (***), Exp.parse
) where
import           Data.Function
import           Data.List
import qualified Data.Map                 as Map
import           Data.Maybe
import qualified Data.MultiSet            as MultiSet
import           Data.Ratio
import qualified Data.Set                 as Set
import           Data.Tuple
import           Development.Placeholders
import           Text.Parsec              as Parsec
import           Text.Parsec.String

type Coef = Rational
type Identifier = Char
type Exp = Map.Map Prod Coef
type Term = (Prod, Coef)
type Prod = MultiSet.MultiSet Atom
type Pow = (Atom, Int)
data Atom = AVar Identifier | AExp Exp deriving(Eq, Ord)

empty :: Exp
empty = Map.empty


instance {-# OVERLAPPING #-} Show Exp where
    show e | e == Map.empty = "0"
    show e = foldl1 (\acc -> (acc ++) . addPlusSign) . map show . Map.toList $ e
        where
            addPlusSign :: String -> String
            addPlusSign s@('-':_) = s
            addPlusSign s = '+':s
instance {-# OVERLAPPING #-} Show Term where
    show (p, i) | p == MultiSet.empty = showRational i
    show (p, -1) = "-" ++ show p
    show (p, 1) = show p
    show (p, i) = showRational i ++ show p
instance {-# OVERLAPPING #-} Show Prod where
    show = concatMap show . MultiSet.toOccurList
instance {-# OVERLAPPING #-} Show Pow where
    show (a, 1) = show a
    show (a, n) = show a ++ "^" ++ show n
instance Show Atom where
    show (AVar i) = [i]
    show (AExp e) = "(" ++ show e ++ ")"

showRational :: Rational -> String
showRational r
    | denominator r == 1 = show (numerator r)
    | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

instance {-# OVERLAPPING #-} Ord Prod where
    p1 <= p2
        | on (<) MultiSet.size p1 p2 = False
        | on (>) MultiSet.size p1 p2 = True
        | otherwise = on (<=?) MultiSet.toOccurList p1 p2
        where
            (<=?) :: [Pow] -> [Pow] -> Bool
            [] <=? [] = True
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
termP = try (swap <$> ((,) <$> rationalP <*> prodP))
        <|> try (do
            n <- rationalP
            char '^'
            i <- naturalP
            return (MultiSet.empty, n^i))
        <|> try ((MultiSet.empty,) $-> rationalP)
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

rationalP :: Parser Rational
rationalP = toRational <$> naturalP

naturalP :: Parser Int
naturalP = read $-> ham (many1 digit)

ham :: Parser a -> Parser a
ham = (spaces *>) . (<* spaces)

-- | Add two Expressions
-- >>> _parse "a" +++ _parse "b"
-- a+b
(+++) :: Exp -> Exp -> Exp
(+++) = Map.unionWith (+)

(-:-) ::Exp -> Exp -> Exp
e1 -:- e2 = Map.filter (/= 0) $ e1 +++ (e2 *** constN (-1))

-- |
-- >>> _parse "dead" *** _parse "beef"
-- abd^2e^3f
(***) :: Exp -> Exp -> Exp
e1 *** e2 = Map.fromListWith (+) [(MultiSet.union p1 p2, i1 * i2) | (p1, i1) <- Map.toList e1,
                                                            (p2, i2) <- Map.toList e2]

(///) :: Exp -> Coef -> Exp
e1 /// c = e1 *** constN (1 / c)

-- |
-- >>> _parse "b^2c+a"
-- b^2c+a
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

constN :: Coef -> Exp
constN = Map.singleton MultiSet.empty

constOne :: Exp
constOne = constN 1

lc :: Exp -> Coef
lc = snd . head . Map.toAscList

lm :: Exp -> Prod
lm = fst. head . Map.toAscList

lt :: Exp -> Term
lt = head . Map.toAscList

lcm :: Prod -> Prod -> Prod
lcm = MultiSet.maxUnion

divProd :: Prod -> Prod -> Prod
divProd = MultiSet.difference

divTerm :: Term -> Term -> Term
divTerm (p1, c1) (p2, c2) = (divProd p1 p2, c1 / c2)

(@|@) :: Prod  -> Prod -> Bool
p1 @|@ p2 = Exp.lcm p1 p2 == p1

(@%@) :: Exp -> Exp -> Exp
e1 @%@ e2
    | e2 == empty = error "second argument of @%@ must not be empty."
    | e1 == empty = empty
    | lm e1 @|@ lm e2 = e1' @%@ e2
    | otherwise = uncurry Map.insert t (ts @%@ e2)
        where
            e1' = e1 -:- (uncurry Map.singleton (on divTerm lt e1 e2) ***e2)
            (t, ts) = Map.deleteFindMin e1

-- |
-- >>> let a = _parse "x^2y+xy^2+y^2"
-- >>> let s = Set.fromList . map _parse $ ["xy-1", "y^2-1"]
-- >>> a @%@@ s
-- x+y+1
(@%@@) :: Exp -> Set.Set Exp -> Exp
e @%@@ s = Set.foldl (@%@) e s


(@@%@) :: Set.Set Exp -> Exp -> Set.Set Exp
s @@%@ e =Set.map (@%@ e) s

-- |
-- >>> on sExpression _parse "xy-1" "y^2+x"
-- -x^2-y
sExpression :: Exp -> Exp -> Exp
sExpression e1 e2 = Map.filter (/=0) $ (e1 *** multiplier1) -:- (e2 *** multiplier2)
    where
        lcm0 =  on Exp.lcm lm e1 e2
        multiplier1 = Map.singleton (lcm0 `divProd` lm e1) (1 / lc e1)
        multiplier2 = Map.singleton (lcm0 `divProd` lm e2) (1 / lc e2)


buchBerger :: Set.Set Exp -> Set.Set Exp
buchBerger f = buchLoop f p
    where p = Set.fromList [(f1, f2) | f1 <- Set.toList f, f2 <- Set.toList f, f1 /= f2]

buchLoop :: Set.Set Exp -> Set.Set (Exp, Exp) -> Set.Set Exp
buchLoop g p
    | p == Set.empty = g
    | otherwise = buchLoop next_g next_p
    where
        (expPair, p') = Set.deleteFindMin p
        h = uncurry sExpression expPair @%@@ g
        next_p = if h == empty then p' else  Set.union p' $ Set.fromList [(u, h) | u <- Set.toList g]
        next_g = if h == empty then g else Set.insert h g

reduce :: Set.Set Exp -> Set.Set Exp
reduce f = foldr (\e acc -> let f' = normalizeCoef (Set.filter (/= Map.empty) $ Set.insert e (f @@%@ e)) in if f' == f then acc else reduce f') f f

normalizeCoef :: Set.Set Exp -> Set.Set Exp
normalizeCoef = Set.map (\e -> e /// lc e)

reducedBasis :: Set.Set Exp -> Set.Set Exp
reducedBasis = reduce . buchBerger
