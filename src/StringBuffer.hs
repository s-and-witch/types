module StringBuffer where

import Data.ByteString.Unsafe
import Data.Text.Internal.Encoding.Utf8
import Data.Text.Internal.Unsafe.Char (unsafeChr8)
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


getNextChar :: StringBuffer -> (Char, StringBuffer)
getNextChar sb = (nextChr, sb & #pos +~ l & #prevPos .~ sb.pos) where
  (nextChr, l) = unsafeUtf8ChrBs sb.pos sb.buffer
{-# INLINE getNextChar #-}

unsafeUtf8ChrBs :: Int -> ByteString -> (Char, Int)
unsafeUtf8ChrBs ind bs = (char, l) where
  b0 = unsafeIndex bs ind
  b1 = unsafeIndex bs (ind + 1)
  b2 = unsafeIndex bs (ind + 2)
  b3 = unsafeIndex bs (ind + 3)

  l  = utf8LengthByLeader  b0

  char = case l of
    1 -> unsafeChr8 b0
    2 -> chr2 b0 b1
    3 -> chr3 b0 b1 b2
    _ -> chr4 b0 b1 b2 b3

getPrevChar :: StringBuffer -> Char -> Char
getPrevChar sb def = case sb.pos of
  0 -> def
  _ -> unsafeUtf8ChrBs sb.prevPos sb.buffer ^. _1

atEnd :: StringBuffer -> Bool
atEnd sb = sb.pos >= length sb.buffer

getSBSOfLen :: Int -> StringBuffer -> ShortByteString
getSBSOfLen len sb = sbs where
  sp = sb.pos - len
  sbs = toShort (BS.take len (BS.drop sp sb.buffer))

stringBufferFromByteString :: ByteString -> StringBuffer
stringBufferFromByteString bs = MkStringBuffer bs 0 (-1)
