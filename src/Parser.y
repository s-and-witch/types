{

module Parser (parseExpr) where

import Monad.Parser
import Token
import Lexer
import Expression
import qualified Base.Prelude as Prelude hiding (error, concatMap, map)
}


%expect 0

%token
  '('  { Tok_LParen  }
  ')'  { Tok_RParen  }
  'λ'  { Tok_Lambda  }
  '.'  { Tok_Dot     }
  NAME { Tok_Name $$ }

%monad { P } { >>= } { pure }
%lexer { lex } { Tok_EOF }
%name exprParse expr
%tokentype { Token }

%%

expr :: { Expr TypesP }
  : lam               { $1 }
  | app               { $1 }
  | expr1             { $1 }

expr1 :: {Expr TypesP}
  : '(' expr ')'      { $2 }
  | NAME              { Var (decodeSBSUtf8 $1) }

app :: {Expr TypesP}
  : app expr1        { App $1 $2 }
  | expr1 expr1      { App $1 $2 }

lam :: { Expr TypesP }
  : 'λ' NAME '.' expr { Lam (decodeSBSUtf8 $2) $4 }

{

happyError :: P a
happyError = do
  loc <- use #location
  throwErr (ParseError loc)


parseExpr :: ByteString -> IO (Either ParseMsg (Expr TypesP))
parseExpr = runParser exprParse
}
