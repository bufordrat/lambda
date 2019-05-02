-- Matt's Typechecker: Lexer datatype and associated functions
-- (C) Matt Teichman, 2019

-- Note: this module uses the ReadP library, whereas LambdaParse does
-- not.  LambdaParse hand-rolls a version of the ReadP library so that
-- it can make use of additional functions that reach into the
-- under-the-hood machinery of ReadP.

module Lexer where

-- standard modules
import Control.Applicative
import Data.Char
import Text.ParserCombinators.ReadP as R

-- lexeme datatype
data Lexeme =
    XLambda
  | XString String
  | XAtomicType Char
  | XColon
  | XDot
  | XLeftP
  | XRightP
  | XLeftA
  | XRightA
  | XWhitesp
  | XEqDef
  | XLineBrk
  | XNull
  deriving (Show, Eq)


-- lookup table for significant characters in my DSL
chr_lex :: Char -> Lexeme
chr_lex chr = 
  case chr of
    '\\' -> XLambda
    ':' -> XColon
    '.' -> XDot
    '(' -> XLeftP
    ')' -> XRightP
    '<' -> XLeftA
    '>' -> XRightA
    '\n' -> XLineBrk
    '=' -> XEqDef
    'E' -> XAtomicType 'E'
    'K' -> XAtomicType 'K'
    'S' -> XAtomicType 'S'
    'T' -> XAtomicType 'T'
    _ -> XNull

-- retrieve a string from a string lexeme
xgetString :: Lexeme -> String
xgetString (XString str) = str
xgetString _ = ""

-- predicate for string lexemes
isXString :: Lexeme -> Bool
isXString (XString _) = True
isXString _ = False

-- string lexer
lexString :: ReadP Lexeme
lexString = fmap XString (munch1 isAlpha)

-- whitespace lexer
lexWhitesp :: ReadP Lexeme
lexWhitesp = fmap (const XWhitesp) (munch1 $ \chr -> chr == ' ')

-- significant character lexer
lexChar :: Char -> ReadP Lexeme
lexChar chr = fmap chr_lex (char chr)

-- arbitrary symbol lexer
lexSymbol :: ReadP Lexeme
lexSymbol = lexChar '\\' <|>
            lexChar ':' <|>
            lexChar '.' <|>
            lexChar '(' <|>
            lexChar ')' <|>
            lexChar '<' <|>
            lexChar '>' <|>
            lexChar '=' <|>
            lexChar '\n' <|>
            lexChar 'E' <|>
            lexChar 'K' <|>
            lexChar 'S' <|>
            lexChar 'T'

-- whitespace/symbol/string lexer
lexItem :: ReadP Lexeme
lexItem = lexWhitesp <++
          lexSymbol <++
          lexString 

-- pull result out of a 'many parser' parser
cleanUpMany :: [([Lexeme], String)] -> Maybe [Lexeme]
cleanUpMany lst =
  case filter (\(x,y) -> y == []) lst of
    [] -> Nothing
    [([],_)] -> Nothing
    pair:[] -> Just $ fst pair
    _ -> Nothing


-- map a string to a list of lexemes representing that string
lambdaLex :: String -> Maybe [Lexeme]
lambdaLex str = cleanUpMany $
                filter (\(x,y) -> y == []) $
                readP_to_S (R.many lexItem) str

-- predicate for lexeme lists with balanced angle brackets
balanced_A :: [Lexeme] -> Bool
balanced_A lst =
  let countParens acc [] = acc
      countParens acc (XLeftA:xs) = countParens (acc + 1) xs
      countParens acc (XRightA:xs) = countParens (acc - 1) xs
      countParens acc (_:xs) = countParens acc xs
  in countParens 0 lst == 0

-- predicate for lexeme lists with balanced parentheses
balanced_P :: [Lexeme] -> Bool
balanced_P lst =
  let countParens acc [] = acc
      countParens acc (XLeftP:xs) = countParens (acc + 1) xs
      countParens acc (XRightP:xs) = countParens (acc - 1) xs
      countParens acc (_:xs) = countParens acc xs
  in countParens 0 lst == 0
