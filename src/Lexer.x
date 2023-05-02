{

module Lexer where

import StringBuffer
import SrcLoc
import Monad.Parser
import Token

}
%encoding "latin1"

$whitechar   = [\ \t\n\r\f\v\xa0] -- \xa0 is Unicode no-break space
$white_no_nl = $whitechar # \n

$ascdigit  = 0-9
$unidigit  = \x01 -- Trick Alex into handling Unicode. See alexGetChar.
$digit     = [$ascdigit $unidigit]
$octit     = 0-7
$hexit     = [$digit A-F a-f]

$unilarge  = \x03 -- Trick Alex into handling Unicode. See alexGetChar.
$asclarge  = [A-Z \xc0-\xd6 \xd8-\xde]
$large     = [$asclarge $unilarge]

$unismall  = \x04 -- Trick Alex into handling Unicode. See alexGetChar.
$ascsmall  = [a-z \xdf-\xf6 \xf8-\xff]
$small     = [$ascsmall $unismall \_]

$namebegin = [$large $small]
$namechar  = [$namebegin $digit]

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent

@escape      = \\ ([abfnrt\\\'\"\?] | x $hexit{1,2} | $octit{1,3})
@strchar     = ($printable # [\"\\]) | @escape

lex :-
$white_no_nl+ ;

<0> {
  \n                    ;
  \(                    {simpleTok Tok_LParen}
  \)                    {simpleTok Tok_RParen}
  λ                     {simpleTok Tok_Lambda}
  \.                    {simpleTok Tok_Dot   }
  $namebegin $namechar* {nameTok             }
}

{

lex :: (Token -> P a) -> P a
lex cont = do
  tok <- lexToken
  -- trace ("token: " ++ show tok) $ do
  cont tok

lexToken :: P Token
lexToken = do
  inp <- getInput
  sc <- lookupParserState
  case alexScan inp sc of
    AlexEOF -> do pure Tok_EOF
    AlexError st -> throwErr (LexerError st.location)
    AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
    AlexToken inp2 len t -> do
        setInput inp2
        t inp2 len

allTokensWithFuel :: Int -> P [Token]
allTokensWithFuel 0 = pure []
allTokensWithFuel n = lexToken >>= \case
  Tok_EOF -> pure [Tok_EOF]
  t -> (t:) <$> allTokensWithFuel (n - 1)

-- * Alex stuff

type AlexInput = ParserState

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar st = getPrevChar st.buffer '\n'

-- backwards compatibility for Alex 2.x
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar st
  | atEnd st.buffer  = Nothing
  | otherwise = Just (ch, st & #buffer .~ sb & #location .~ loc) where
    (!ch, !sb) = getNextChar st.buffer
    !loc = advanceSrcLoc st.location ch

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte st = alexGetChar st & _Just . _1 %~ (fromIntegral . ord)

getInput :: P AlexInput
getInput = get

setInput :: AlexInput -> P ()
setInput = put

}
