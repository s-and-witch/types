{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Expression
import GHC.IO (unsafePerformIO)
import Parser

type Env p v = Map (Name p) v

data Value p where
  VClosure :: Name p -> Expr p -> Env p (Value p) ->  Value p

data Msg p where
  VariableNotFound :: Name p -> Msg p

type Except p v = Either (Msg p) v

lookupEnv :: Ord (Name p) => Name p -> Env p v -> Except p v
lookupEnv n env = case env ^. at n of
  Nothing -> Left (VariableNotFound n)
  Just v -> Right v

eval :: Ord (Name p) => Expr p -> (Env p (Value p) -> Except p (Value p))
eval = \case
  Var n -> lookupEnv n
  Lam n e -> pure . VClosure n e
  App f a -> \env -> do
    VClosure n expr env' <- eval f env
    a' <- eval a env
    eval expr (env' & at n ?~ a')


parsedProgram :: Expr TypesP
parsedProgram = either (error . show) id $ unsafePerformIO do
  parseExpr $ encodeUtf8 @Text "(λ bind .  bind (λ a . λ b . a) (λ true . λ guard . a b c d )) (λ v . λ f . f v)"

main :: IO ()
main = do
  print parsedProgram
  print (eval parsedProgram mempty)


deriving instance Show (Value TypesP)
deriving instance Show (Expr TypesP)
deriving instance Show (Msg TypesP)
