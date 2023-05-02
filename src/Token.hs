module Token where
import Data.ByteString.Short
import Monad.Parser
import StringBuffer


data Token
  = Tok_Lambda
  | Tok_LParen
  | Tok_RParen
  | Tok_Name ShortByteString
  | Tok_Dot
  | Tok_EOF
  deriving Show


type TokenAction t = ParserState -> Int -> P t

simpleTok :: Token -> TokenAction Token
simpleTok t = const . const . pure $ t

nameTok :: TokenAction Token
nameTok st len = pure (Tok_Name (getSBSOfLen len st.buffer))
