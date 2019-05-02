-- Matt's Typechecker: Datatype for Lambda Calculus Terms
-- (C) Matt Teichman, 2019

module LTerm where

import Control.Monad

data LType = E | K | S | T | Arrow LType LType
  deriving (Eq)

data LTerm =
    Vr String
  | App LTerm LTerm
  | Lambda String LType LTerm
  deriving (Eq)
  
data TypedExpression = Typed LTerm LType

instance Show LType where
  show E = "E"
  show K = "K"
  show S = "S"
  show T = "T"
  show (Arrow ty1 ty2) = "(" ++ (show ty1) ++ " \8594 " ++
                         (show ty2) ++ ")"

instance Show LTerm where
  show (Vr string) = string
  show (App trm1 trm2) = "(" ++ (show trm1) ++ " " ++
                         (show trm2) ++ ")"
  show (Lambda str typ trm) = "(" ++ "\955" ++ str ++ ":" ++ (show typ) ++
                              " . " ++ show trm ++ ")"

instance Show TypedExpression where
  show (Typed trm typ) = (show trm) ++ " : " ++ (show typ)

-- typed expression function application
app :: TypedExpression ->
       TypedExpression ->
       Either String TypedExpression
app (Typed trm1 typ1) (Typed trm2 typ2) =
  case typ1 of
    (Arrow typ3 typ4) -> if typ2 == typ3 
                         then Right $ Typed (App trm1 trm2) typ4
                         else Left ("Type Error:\n  expression " ++ 
                                     (show trm2) ++ 
                                     " was supposed to be of type " ++ 
                                     (show typ3) ++ 
                                     ",\n  but was instead of type " ++ 
                                     (show typ2) ++
                                     ".")
    _ -> Left $
         "Type Error:\n expression " ++ (show trm1) ++
         " is not a function type."


-- typed expression function application for Either types
app_lifted :: Either String TypedExpression ->
              Either String TypedExpression ->
              Either String TypedExpression
app_lifted ete1 ete2 = join $ do
  te1 <- ete1
  te2 <- ete2
  return $ app te1 te2

-- typed expression lambda abstraction
lamb :: String -> LType ->
        TypedExpression ->
        Either String TypedExpression
lamb str typ1 exp@(Typed trm typ2) =
  return $ Typed (Lambda str typ1 trm) (Arrow typ1 typ2)

-- typed expression lambda abstraction for Either types
lamb_lifted :: String -> LType ->
               Either String TypedExpression ->
               Either String TypedExpression
lamb_lifted str typ ete1 = join $ fmap (lamb str typ) ete1
