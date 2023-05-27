module StringBuffer where

import Data.ByteString.Unsafe
import Control.Lens.Unsound (adjoin)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.ByteString qualified as BS


data StringBuffer = MkStringBuffer{
  buffer   :: {-# UNPACK #-} !ByteString,
  pos      :: {-# UNPACK #-} !Int,
  prevPos  :: {-# UNPACK #-} !Int
} deriving Generic

getNextByte :: StringBuffer -> (Word8, StringBuffer)
getNextByte sb = (unsafeIndex sb.buffer sb.pos, sb & adjoin #pos #prevPos +~ 1)

atEnd :: StringBuffer -> Bool
atEnd sb = sb.pos >= length sb.buffer

getSBSOfLen :: Int -> StringBuffer -> ShortByteString
getSBSOfLen len sb = sbs where
  sp = sb.pos - len
  sbs = toShort (BS.take len (BS.drop sp sb.buffer))

stringBufferFromByteString :: ByteString -> StringBuffer
stringBufferFromByteString bs = MkStringBuffer bs 0 (-1)
