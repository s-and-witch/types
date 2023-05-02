module Monad.Parser where
import StringBuffer
import SrcLoc
import Control.Exception (throwIO)



data ParserState = ParserState {
  buffer     :: {-# UNPACK #-} !StringBuffer,
  location   :: {-# UNPACK #-} !RealSrcLoc,
  stateStack :: [Int]
} deriving Generic

newtype P a = MkP {unP :: StateT ParserState IO a}
  deriving newtype (Functor, Applicative, Monad, MonadState ParserState, MonadIO)

runParser :: P a -> ByteString -> IO (Either ParseMsg a)
runParser p bs = try @_ @ParseMsg . flip evalStateT initState  $ p.unP where
  initState = ParserState (stringBufferFromByteString bs) initLoc []

data ParseMsg where
  LexerError :: RealSrcLoc -> ParseMsg
  ParseError :: RealSrcLoc -> ParseMsg
  deriving (Show, Exception)

lookupParserState :: P Int
lookupParserState = preuse ( #stateStack . _head) <&> \case
  Nothing -> 0
  Just st -> st

pushParserState :: Int -> P ()
pushParserState st = #stateStack %= (st :)

popParserState :: P ()
popParserState = #stateStack %= \case [] -> []; (_:xs) -> xs

throwErr :: ParseMsg -> P a
throwErr err = liftIO $ throwIO err
