module SrcLoc where
import Data.Bits

data RealSrcLoc = SrcLoc {
  line   :: {-# UNPACK #-} !Int,
  column :: {-# UNPACK #-} !Int
} deriving (Generic, Show)

-- | Move the 'SrcLoc' down by one line if the character is a newline,
-- to the next 8-char tabstop if it is a tab, and across by one
-- character in any other case
advanceSrcLoc :: RealSrcLoc -> Char -> RealSrcLoc
advanceSrcLoc srcLoc = \case
  '\n' -> srcLoc & #line +~ 1 & #column .~ 1
  '\t' -> srcLoc & #column %~ advance_tabstop
  _    -> srcLoc & #column +~ 1

advance_tabstop :: Int -> Int
advance_tabstop c = ((((c - 1) `shiftR` 3) + 1) `shiftL` 3) + 1

initLoc :: RealSrcLoc
initLoc = SrcLoc 1 1
