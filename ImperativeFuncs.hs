-- Matt's Typechecker: Imperative Helper Functions
-- (C) Matt Teichman, 2019

module ImperativeFuncs where

-- standard modules
import Control.Exception
import System.Exit

-- my modules
import LambdaParse
import Latex
import Lexer
import LTerm


-- load DECLARATIONS file
getLexicon :: FilePath -> IO (Maybe [(String, TypedExpression)])
getLexicon filename = do
  declarations <- catch (readFile filename) handleLexicon
  let lexicon = map (makeDenotation . (lambdaParse declP [])) $
                lines declarations
  return $ sequence lexicon

-- load LAMBDATERM file
getInput :: FilePath -> IO (Maybe [String])
getInput filename = do
  input <- catch (readFile filename) handleLambdas
  return $ case lines input of
    [] -> Nothing
    _ -> Just $ lines input

-- error message for empty DECLARATIONS file
lexiconError :: IO ()
lexiconError = do
  putStrLn "There is a parse error in your lexicon."
  exitFailure

-- error message for empty LAMBDATERM file
termError :: IO ()
termError = do
  putStrLn "Please populate your LAMBDATERM file with terms to be parsed."
  exitFailure

-- message for a parse error
parseError :: IO ()
parseError = putStrLn "Parse Error!"

-- print a string representation of a typed expression to screen
displayMaybe Nothing = parseError
displayMaybe (Just parse) = displayEither parse

-- helper function for displayMaybe
displayEither (Left x) = putStrLn x
displayEither (Right y) = putStrLn $ show y

-- print a LaTeX representation of a typed expression to screen
latexMaybe Nothing = parseError
latexMaybe (Just parse) = latexEither parse

-- helper function for latexMaybe
latexEither (Left x) = putStrLn x
latexEither (Right y) = putStrLn $ "$" ++ latexTE y ++ "$"

-- handle missing DECLARATIONS file
handleLexicon :: SomeException -> IO String
handleLexicon (SomeException e) = do
  putStrLn $
    "\nPlease create a DECLARATIONS file with lexical entries.\n" ++
    "\n   For example:\n\n" ++
    "      dog:<ET>\n" ++
    "      fido:E\n" ++
    "      barks:<ET>\n" ++
    "      forall:<<ET>T>\n" 
  exitFailure

-- handle missing LAMBDATERM file
handleLambdas :: SomeException -> IO String
handleLambdas (SomeException e) = do
  putStrLn $
    "\nPlease create a LAMBDATERM file with some lambda terms in it.\n" ++
    "\n   For example:\n\n" ++
    "      fido\n" ++
    "      (barks fido)\n" ++
    "      (\\x:E . (barks x))\n"
  exitFailure

-- create a LaTeX document displaying the lambda terms and their types
writeLatex :: String -> IO ()
writeLatex str = writeFile "lambdas.tex" $
                 prelude ++ "\n$" ++ str ++ "$\n" ++ closing
