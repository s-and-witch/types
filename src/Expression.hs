module Expression where

data Phase = Parsed

type Pipeline :: Phase -> Type
data Pipeline p where
  PParsed :: Pipeline Parsed

type Name :: k -> Type
type family Name p

type instance Name (Pipeline Parsed) = Text

type TypesP = Pipeline Parsed

data Expr p where
  Var :: Name p -> Expr p
  Lam :: Name p -> Expr p -> Expr p
  App :: Expr p -> Expr p -> Expr p
