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
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import System.IO
import Data.Char


-- Custom data type provided by Professor Schlegel
data ExprTree = Proof { line :: ExprTree, subpr :: ExprTree}
              | Subproof { sub_rule :: ExprTree, hypo :: ExprTree, derivations :: [ExprTree]}
              | DerivLine { line :: ExprTree, inner :: ExprTree}
              | Hyp { line :: ExprTree, ante :: ExprTree }
              | DisRule { d_rule :: ExprTree }
              | NonDisRule { d_rule :: ExprTree }
              | CondIntro { rule :: [Char], prop :: ExprTree }
              | CondElim { rule :: [Char], prop :: ExprTree, c_arg1 :: ExprTree, c_arg2 :: ExprTree }
              | ConjIntro { rule :: [Char], exp :: ExprTree,  c_arg1 :: ExprTree, c_arg2 :: ExprTree }
              | ConjElim { rule :: [Char], exp :: ExprTree, arg :: ExprTree }
              | Expr { val :: ExprTree }
              | CondExpr { op :: [Char], arg1 :: ExprTree, arg2 :: ExprTree }
              | ConjExpr { op :: [Char], arg1 :: ExprTree, arg2 :: ExprTree }
              | Const { value :: [Char] }
              | Number { value :: [Char] } deriving(Show, Eq)


-- parses for <const-expr>
constexpr :: GenParser Char st ExprTree
constexpr = do
    exp <- many1 upper
    return (Const exp)

-- parses for <number>
numexpr :: GenParser Char st ExprTree
numexpr = do
    exp <- many1 digit
    return (Number exp)

-- parses for <conjunction-expr>
conjexpr :: GenParser Char st ExprTree
conjexpr = do
  char '('
  op <- string "and"
  space
  exp1 <- exprexpr
  space
  exp2 <-  exprexpr
  char ')'
  return (ConjExpr op exp1 exp2)

-- parses for <conditional-expr>
condexpr :: GenParser Char st ExprTree
condexpr = do
  char '('
  op <- string "if"
  space
  exp1 <- exprexpr
  space
  exp2 <- exprexpr
  char ')'
  return (CondExpr op exp1 exp2)

-- parse for <expr>
exprexpr :: GenParser Char st ExprTree
exprexpr = do
  val <- try condexpr <|> try conjexpr <|> try constexpr
  return (Expr val)

-- parses for <conjunction-elim-rule>
conjelimexpr :: GenParser Char st ExprTree
conjelimexpr = do
  rule <- string "&E"
  space
  arg1 <- exprexpr
  space
  arg2 <- numexpr
  return (ConjElim rule arg1 arg2)

-- parses for <conjunction-intro-rule>
conjintroexpr :: GenParser Char st ExprTree
conjintroexpr = do
  rule <- string "&I"
  space
  arg1 <- conjexpr
  space
  arg2 <- numexpr
  space
  arg3 <- numexpr
  return (ConjIntro rule arg1 arg2 arg3)

-- parses for <conditional-elim-rule>
condelimexpr :: GenParser Char st ExprTree
condelimexpr = do
  rule <- string "->E"
  space
  arg1 <- exprexpr
  space
  arg2 <- numexpr
  space
  arg3 <- numexpr
  return (CondElim rule arg1 arg2 arg3)

-- parses for <conditional-intro-rule>
condintroexpr :: GenParser Char st ExprTree
condintroexpr = do
  rule <- string "->I"
  space
  arg <- condexpr
  return (CondIntro rule arg)

-- parses for <non-discharge-rule>
nondisexpr :: GenParser Char st ExprTree
nondisexpr = do
  char '('
  rule <- try conjintroexpr <|> try condelimexpr <|> try conjelimexpr
  char ')'
  return (NonDisRule rule)

-- parses for <non-discharge-rule>
disexpr :: GenParser Char st ExprTree
disexpr = do
  arg <- condintroexpr
  return (DisRule arg)

-- parses for <hypothesis>
hypexpr :: GenParser Char st ExprTree
hypexpr = do
  spaces --newline
  arg1 <- numexpr
  space
  char '('
  string "hyp"
  space
  arg2 <- exprexpr
  char ')'
  return (Hyp arg1 arg2)

-- parses for <subproof>
subexpr :: GenParser Char st ExprTree
subexpr = do
  char '('
  arg1 <- disexpr
  spaces --newline
  char '('
  string "proof"
  arg2 <- hypexpr
  arg3 <- many1 derivexpr
  char ')'
  char ')'
  return (Subproof arg1 arg2 arg3)

-- parses for <derivation-line>
derivexpr :: GenParser Char st ExprTree
derivexpr = do
  spaces --newline
  arg1 <- numexpr
  space
  arg2 <- try subexpr <|> try nondisexpr
  return (DerivLine arg1 arg2)

-- parse for <proof>
proofexpr :: GenParser Char st ExprTree
proofexpr = do
  arg1 <- numexpr
  space
  arg2 <- subexpr
  return (Proof arg1 arg2)

main :: IO ()
main = do
  contents <- readFile "proof1.txt"
  print (parse proofexpr "" contents)
