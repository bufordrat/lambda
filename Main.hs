-- Matt's Typechecker: Main Program
-- (C) Matt Teichman, 2018

-- You can get started by typing the following at the shell:

--     $ cabal new-exec lambda

-- You can also take a look at the README for more details:

--     $ cat README.txt

module Main where

-- library modules
import Data.Maybe
import System.Environment
import Data.List

-- my modules
import ImperativeFuncs
import LambdaParse
import Latex
import Lexer
import LTerm


-- run program in either normal mode or LaTeX mode
displayMode :: (Maybe (Either String TypedExpression) -> IO a) ->
               String -> IO ()
displayMode displayFunc message = do
  input <- getInput "LAMBDATERM"
  case input of
    Nothing -> termError
    _ -> do
      maybe_lexicon <- getLexicon "DECLARATIONS"
      case maybe_lexicon of
        Nothing -> lexiconError
        Just _ -> do
          putStr message
          let lexicon = fromJust maybe_lexicon
              parse = map (lambdaParse termP lexicon) $ fromJust input
          mapM_ displayFunc parse


-- run program in full LaTeX mode
fullLatexMode :: IO ()
fullLatexMode = do
  input <- getInput "LAMBDATERM"
  case input of
    Nothing -> termError
    _ -> do
      maybe_lexicon <- getLexicon "DECLARATIONS"
      case maybe_lexicon of
        Nothing -> lexiconError
        Just _ -> do
          let lexicon = fromJust maybe_lexicon
              parse = map (lambdaParse termP lexicon) $ fromJust input
              maybeEither = fmap sequence $ sequence parse
          case maybeEither of
            Nothing -> parseError
            Just x -> case x of
              Left e -> putStrLn e
              Right y -> do
                let fullString = intercalate "\\\\\n" $ map latexTE y
                putStrLn "Writing LaTeX file..."
                writeLatex fullString


-- command line help message
usage :: String
usage =
  "\n Welcome to Matt's Typechecker!\n\n" ++
  "      USAGE:\n\n" ++
  "         --check-types   (normal mode)\n" ++
  "         --latex         (latex mode)\n" ++
  "         --full-latex    (full latex mode)\n\n" ++
  "      Place your inputs into two text files called:\n\n" ++
  "         DECLARATIONS   (series of lexical entries)\n" ++
  "         LAMBDATERM     (series of lambda terms)\n"


-- command line switchbox
command "--check-types" = do
  displayMode displayMaybe "\nHere are the types of your lambda terms:\n\n"
  putStr "\n"
command "--latex" = displayMode latexMaybe ""
command "--full-latex" = fullLatexMode
command _ = putStrLn usage


-- main program
main = do
  argv <- getArgs
  case length argv of
    1 -> command $ head argv
    _ -> putStrLn usage
