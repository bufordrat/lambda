-- Matt's Typechecker: Parser for Lambda Terms
-- (C) Matt Teichman, 2019

module LambdaParse where

-- standard modules
import Data.Maybe
import Control.Applicative

-- my modules
import Lexer
import LTerm

-- datatype for lexicon: association list
type Store = [(String, TypedExpression)]

-- lexeme list parser
newtype Parser a = MkParser { runParser :: Store -> [Lexeme] -> [(a, [Lexeme])] }

-- Functor, Applicative, Monad, and Alternative instances
instance Functor Parser where
  fmap g parser = MkParser $ \st l -> 
    [ (g x, y) | (x, y) <- runParser parser st l ]

instance Applicative Parser where
  pure x = MkParser $ \st l -> [(x, l)]
  af <*> ax = MkParser $ \st l -> 
    [ (f x, u) | (f, t) <- runParser af st l
               , (x, u) <- runParser ax st t ]

instance Monad Parser where
  mp >>= mf = MkParser $ \st l -> 
    [ (y, u) | (x, t) <- runParser mp st l
             , (y, u) <- runParser (mf x) st t ]

instance Alternative Parser where
  empty = MkParser $ \st s -> []
  (MkParser f) <|> (MkParser g) = MkParser $ \st l -> f st l ++ g st l 

-- left-biased sum
infixr 7 <++

(<++) :: Parser a -> Parser a -> Parser a  
ap <++ bp = MkParser $ \st l ->
    case runParser ap st l of
        [] -> runParser bp st l
        rs -> rs

-- move the head of a lexeme list onto the left, if it satisfies a predicate
lsatisfy :: (Lexeme -> Bool) -> Parser Lexeme
lsatisfy pred = MkParser $ \st s -> 
  case s of
    [] -> []
    (x : xs) -> [ (x, xs) | pred x ]

-- move the head of a lexeme list onto the left only if it matches the input
lchar :: Lexeme -> Parser Lexeme
lchar lexm = lsatisfy (\l -> l == lexm)

-- move the string from a string lexeme onto the left
lstring :: Parser String
lstring = MkParser $ \st l ->
  case l of
    [] -> []
    (x : xs) -> [ (xgetString x, xs) | isXString x ]

-- move a lexeme onto the left, then substitute something else in
ltokenC :: Lexeme -> a -> Parser a
ltokenC lexm tok = fmap (\_ -> tok) $ lchar lexm

-- parser for arrow types
arrowP :: Parser (LType -> LType -> LType)
arrowP = fmap (const Arrow) $ lchar XLeftA

-- parser for types
typeP :: Parser LType
typeP = arrowP <*> typeP <*> typeP <* lchar XRightA
        <|> ltokenC (XAtomicType 'E') E
        <|> ltokenC (XAtomicType 'K') K
        <|> ltokenC (XAtomicType 'S') S
        <|> ltokenC (XAtomicType 'T') T 

-- function to kick off the type declaration parser
declSchema :: String -> LType -> TypedExpression
declSchema string typ = Typed (Vr string) typ

-- type declaration parser
declP = pure declSchema
        <*> lstring
        <* lchar XColon
        <*> typeP

-- helper function for building the lexicon
stringFromVar :: TypedExpression -> Maybe String
stringFromVar (Typed (Vr str) _) = Just str
stringFromVar _ = Nothing

-- map a Vr typed expression to a lexical entry for it
makeDenotation :: Maybe TypedExpression -> Maybe (String, TypedExpression)
makeDenotation mexp = do
  exp <- mexp
  let key = stringFromVar exp
  if isNothing key
    then Nothing
    else return (fromJust key, exp)

-- same as makeDenotation, but not for Maybe-s
makeDenotation2 :: TypedExpression -> (String, TypedExpression)
makeDenotation2 texp = (fromJust $ stringFromVar texp, texp) 

-- helper function for bound variable magic
getResult :: [(a, b)] -> a
getResult = fst . head

-- bound variable magic: read a type declaration from a parser,
--   update the lexicon with that new entry, then pass that new
--   entry back into the original parser
addVariableP :: Parser a -> Parser a
addVariableP parser = MkParser $ \st l ->
  let newEntry = makeDenotation2 $ getResult $ runParser declP st l
  in runParser parser (newEntry : st) l

-- application parser
appP :: Parser (Either String TypedExpression)
appP = (pure $ app_lifted)
       <*> termP 
       <* lchar XWhitesp
       <*> termP 

-- lambda abstraction parser
lambdaP = (pure lamb_lifted)
          <*> lstring
          <* lchar XColon
          <*> typeP
          <* lchar XWhitesp
          <* lchar XDot
          <* lchar XWhitesp
          <*> termP

-- consume string lexeme, put value for entry with that string
--   on the left
storeP :: Parser (Maybe TypedExpression)
storeP = MkParser $ \st l -> runParser
  (fmap (\str -> lookup str st) lstring) st l

-- convert lookup Maybe-s to lookup Either-s
eitherify (Nothing) = Left $
                      "Lexicon Error: something " ++
                      "is missing from your lexicon."
eitherify (Just x) = Right x

-- variable parser
varP :: Parser (Either String TypedExpression)
varP = fmap eitherify storeP

-- lambda term parser
termP :: Parser (Either String TypedExpression)
termP = pure (const id)
        <*> lchar XLeftP
        *> lchar XLambda
        *> (addVariableP lambdaP)
        <* lchar XRightP
        <|> lchar XLeftP
        *> appP
        <* lchar XRightP
        <|> varP

-- map a string from Matt's DSL to a typed expression,
--   passing in a lexicon as an input
lambdaParse :: Parser a -> Store -> String -> Maybe a
lambdaParse parser st str =
  case fmap (runParser parser st) $ lambdaLex str of
    Nothing -> Nothing
    Just [] -> Nothing
    attempt -> case attempt of
      Just [ (x, []) ] -> Just x
      _ -> Nothing
