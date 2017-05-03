{-
File: proof_checker.hs
Authors:
  Dor Rondel
  Nirender
Instructor: Prof. Daniel Schlegel
Course: CSc 344
Institution: SUNY Oswego
-}


-- Imports
-- Note: will need to install parsec through cabal
import Text.ParserCombinators.Parsec.Char
import Data.Maybe
import System.IO
import Control.Exception hiding (try)  -- Will give a namespace error w/o the hide
import Text.ParserCombinators.Parsec

data ExprTree = Constant { val :: String }
              | Num   { num :: Int }
              | Proof { line :: Int, operator :: String, sub :: ExprTree }
              | Subproof  { operator   :: String, line  :: Int, arg1 :: ExprTree, arg2 :: ExprTree, arg3 :: [ExprTree] }
              | DerivLine  { line :: Int, operator  :: String, rule :: ExprTree }
              | Hypothesis   { line :: Int, operator  :: String, rule :: ExprTree }
              | CondIntro  { operator  :: String, rule :: ExprTree }
              | ConjIntroCondElim  { operator  :: String, rule :: ExprTree, line1 :: Int, line2 :: Int }
              | ConjElim  { operator  :: String, rule :: ExprTree, line :: Int }
              | Expr  { operator  :: String, arg1 :: ExprTree, arg2 :: ExprTree } deriving (Show, Eq)

proofexpr :: GenParser Char st ExprTree
proofexpr = do
    num <- numberexpr
    space
    char '('
    expression <- subproofexpr
    return (Proof num "proof" expression)

subproofexpr :: GenParser Char st ExprTree
subproofexpr = do
    arg1 <- dischargeExpr
    spaces
    char '('
    string "proof"
    arg2 <- hypothesisexpr
    arg3 <- (many derivationexpr)
    char ')'
    char ')'
    return (Subproof "sub" (-1) arg1 arg2 arg3)

derivationexpr :: GenParser Char st ExprTree
derivationexpr = do
    spaces
    num <- numberexpr
    space
    char '('
    exp <- try subproofexpr <|> try nondischargeExpr
    return (DerivLine num "derv" exp)

hypothesisexpr :: GenParser Char st ExprTree
hypothesisexpr = do
    spaces  -- includes newline
    num <- numberexpr
    space
    char '('
    string "hyp"
    space
    arg <- expr
    char ')'
    return (Hypothesis num "hyp" arg)

dischargeExpr :: GenParser Char st ExprTree
dischargeExpr = try conditionalIntroExpr

nondischargeExpr :: GenParser Char st ExprTree
nondischargeExpr = try conjIntroExpr <|> try conjElimExpr <|> try conditionalElimExpr

conditionalIntroExpr :: GenParser Char st ExprTree
conditionalIntroExpr = do
    string "->I"
    space
    exp <- implicationExpr
    return (CondIntro "->I" exp)

conditionalElimExpr :: GenParser Char st ExprTree
conditionalElimExpr = do
    string "->E"
    space
    arg1 <- expr
    space
    arg2 <- numberexpr
    space
    arg3 <- numberexpr
    char ')'
    return (ConjIntroCondElim "->E" arg1 arg2 arg3)

conjIntroExpr :: GenParser Char st ExprTree
conjIntroExpr = do
    string "&I"
    space
    arg1 <- conjExpr
    space
    arg2 <- numberexpr
    space
    arg3 <- numberexpr
    char ')'
    return (ConjIntroCondElim "&I" arg1 arg2 arg3)

conjElimExpr :: GenParser Char st ExprTree
conjElimExpr = do
    string "&E"
    space
    ant <- expr
    space
    cq <- numberexpr
    char ')'
    return (ConjElim "&E" ant cq)

expr :: GenParser Char st ExprTree
expr = try implicationExpr <|> try conjExpr <|> try constexpr

implicationExpr :: GenParser Char st ExprTree
implicationExpr = do
    char '('
    string "if"
    space
    ant <- expr
    space
    cq <- expr
    char ')'
    return (Expr "if" ant cq)

conjExpr :: GenParser Char st ExprTree
conjExpr = do
    char '('
    string "and"
    space
    ant <- expr
    space
    cq <- expr
    char ')'
    return (Expr "and" ant cq)

constexpr :: GenParser Char st ExprTree
constexpr = do
    exp <- many1 upper
    return (Constant exp)

numberexpr :: GenParser Char st Int
numberexpr = do
    num <- read <$> many1 digit
    return (num)

-- Take rule and # returns the rule at the corresponding to the rule the number references
findRule :: ExprTree -> Int -> Maybe ExprTree
findRule tree n =
    if (line tree) == n then Just (rule tree)
        else if (operator tree) == "proof" then findRule (sub tree) n
            else if (operator tree) == "sub" && (line (arg2 tree)) == n then Just (arg2 tree)  -- type == Expr
                else if (operator tree) == "sub" then findRule2 (arg3 tree) n  -- type = (arg3 subproof) == derivation lines
                    else Nothing

-- For arg3 of subproof, derivationlines by BNF, recursively gets corresponding rule
findRule2 :: [ExprTree] -> Int -> Maybe ExprTree
findRule2 trees n =
    if (line (take 1 trees !! 0)) == n then Just (rule (take 1 trees !! 0))
        else if (operator (take 1 trees !! 0)) == "derv" && (operator (rule (take 1 trees !! 0))) == "sub" then
            case findRule (rule (take 1 trees !! 0)) n of
                Nothing -> findRule2 (tail trees) n
                Just trees -> Just trees
            else if length trees > 1 then findRule2 (tail trees) n
                else Nothing

-- if theres a nested proof recursively simplify else map theorems get true/false
implicationIntro :: ExprTree -> ExprTree -> Bool
implicationIntro ant cq = (arg1 (rule (arg1 cq))) == (rule (arg2 cq)) &&
            if (operator (rule (take 1 (arg3 cq) !! 0))) == "sub" &&
                (rule (arg1 (rule (take 1 (arg3 cq) !! 0)))) == (arg2 (rule (arg1 cq)))
                then implicationIntro ant (rule (take 1 (arg3 cq) !! 0))
                else theoremMapper (arg3 cq) ant

-- If x and y and x then y
implicationElim :: ExprTree -> ExprTree -> Bool
implicationElim expresion theorem =
    case findRule expresion (line1 theorem) of
        Nothing -> False
        Just x -> case findRule expresion (line2 theorem) of
            Nothing -> False
            Just y -> "if" == (operator $ rule x) && (arg1 $ rule x) == (rule y) && (rule theorem) == (arg2 $ rule x)

-- p ^ q ^ (q or p)
conjunctionElim :: ExprTree -> ExprTree -> Bool
conjunctionElim half1 half2 =
    case findRule half1 (line half2) of
        Nothing -> False
        Just x -> (operator $ rule x) == "and" && ((arg1 $ rule x) == (rule half2) || (arg2 $ rule x) == (rule half2))

-- p ^ q then (p ^ q)
conjunctionIntro :: ExprTree -> ExprTree -> Bool
conjunctionIntro expresion j =
    case findRule expresion (line1 j) of
        Nothing -> False
        Just x -> case findRule expresion (line2 j) of
            Nothing -> False
            Just y -> if (operator x) == "sub"
            then (arg1 $ rule j) == (rule $ arg1 x) && (arg2 $ rule j) == (rule $ arg1 y)
            else (arg1 $ rule j) == (rule x) && (arg2 $ rule j) == (rule y)


-- Uses correct rulee validator per deriv in derivation lines
theoremMapper :: [ExprTree] -> ExprTree -> Bool
theoremMapper derivations prevNode = (all (== True) (map (\x -> if
                                (operator $ rule x) == "&E"
                                then conjunctionElim prevNode (rule x) else if
                                (operator $ rule x) == "->E"
                                then implicationElim prevNode (rule x) else if
                                (operator $ rule x) == "&I"
                                then conjunctionIntro prevNode (rule x) else if
                                (operator $ rule x) == "sub"
                                then implicationIntro prevNode (rule x) else False) derivations))

proofValidator :: ExprTree -> Bool
proofValidator proof = implicationIntro proof (sub proof)

main :: IO ()
main = do
    file <- readFile "proof2.txt"
    case (parse proofexpr "" file) of
        Left e -> do
                    print "Couldn't parse given file"
                    print e
        Right r -> do
                    print (proofValidator r) `catch` \(SomeException _) -> putStrLn ("Invalid Proof")
