{-# LANGUAGE BangPatterns, CPP, EmptyDataDecls, DeriveDataTypeable, OverloadedStrings #-}
-- | Parsing of an Stockholm 1.0 file into a stream of events.
module Bio.Sequence.Stockholm.Stream
    ( -- * Streams
      Event(..)
    , parseEvents
    , renderEvents
    )
    where

-- from base
import Control.Applicative
import Data.Monoid (mappend)

-- from bytestring
import qualified Data.ByteString.Char8 as B

-- from conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

-- from attoparsec
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as A8

-- from attoparsec-conduit
import Data.Conduit.Attoparsec (sinkParser)

-- from blaze-builder
import qualified Blaze.ByteString.Builder as Blaze

-- from blaze-builder-conduit
import Data.Conduit.Blaze (builderToByteString)


-- | An event (roughly a line in the file).
data Event = EvHeader
             -- ^ @# STOCKHOLM 1.0@
           | EvEnd
             -- ^ @\/\/@
           | EvComment B.ByteString
             -- ^ @# ....@
           | EvSeqData B.ByteString B.ByteString
             -- ^ @seqlabel seqdata@
           | EvGF B.ByteString B.ByteString
             -- ^ @#GF feature data@
           | EvGC B.ByteString B.ByteString
             -- ^ @#GC feature data@
           | EvGS B.ByteString B.ByteString B.ByteString
             -- ^ @#GS seqlabel feature data@
           | EvGR B.ByteString B.ByteString B.ByteString
             -- ^ @#GR seqlabel feature data@
             deriving (Eq, Ord, Show)


-- | Parse an 'Event'.
eventParser :: A.Parser Event
eventParser = (hash *> (header
                    <|> ann
                    <|> comment)
           <|> end
           <|> seqdata) <* skipTillNextLine
    where
      spaces = A.skipWhile A8.isHorizontalSpace
      word   = A.takeTill A8.isSpace_w8 <* spaces
      skipTillNextLine = A.skipWhile (not . A8.isEndOfLine) <* some A8.endOfLine
      tillNextLine     = A.takeTill A8.isEndOfLine

      hash    = A8.char '#'
      header  = EvHeader  <$  spaces <* A8.string "STOCKHOLM 1.0"
      comment = EvComment <$> tillNextLine
      end     = EvEnd     <$  A8.string "//"
      seqdata = EvSeqData <$> word <*> tillNextLine

      ann = A8.string "=G" *> (gf <|> gc <|> gs <|> gr)
          where
            gf = EvGF <$ A8.string "F" <* spaces          <*> word <*> tillNextLine
            gc = EvGC <$ A8.string "C" <* spaces          <*> word <*> tillNextLine
            gs = EvGS <$ A8.string "S" <* spaces <*> word <*> word <*> tillNextLine
            gr = EvGR <$ A8.string "R" <* spaces <*> word <*> word <*> tillNextLine



-- | Conduit that parses a file into events.
parseEvents :: C.ResourceThrow m => C.Conduit B.ByteString m Event
parseEvents = C.sequenceSink False go
    where
      go False = CB.dropWhile A8.isSpace_w8 >> go True
      go True  = do
        mevent <- sinkParser $     Nothing <$  A8.endOfInput
                               <|> Just    <$> eventParser
        return $ case mevent of
                   Nothing    -> C.Stop
                   Just event -> C.Emit True [event]


-- | Pretty print an event.
eventPrinter :: Event -> Blaze.Builder
eventPrinter ev =
    case ev of
      EvHeader                    -> bs "# STOCKHOLM 1.0\n"
      EvEnd                       -> bs "//\n"
      EvComment comment           -> bs "#" <> bs comment <> n
      EvSeqData seqlabel seqdata  -> bs seqlabel <> s <> bs seqdata <> n
      EvGF          feature data_ -> bs "#=GF " <> bs feature  <> s <> bs data_ <> n
      EvGC          feature data_ -> bs "#=GC " <> bs feature  <> s <> bs data_ <> n
      EvGS seqlabel feature data_ -> bs "#=GS " <> bs seqlabel <> s <> bs feature <> s <> bs data_ <> n
      EvGR seqlabel feature data_ -> bs "#=GR " <> bs seqlabel <> s <> bs feature <> s <> bs data_ <> n
    where bs = Blaze.fromByteString
          (<>) = mappend
          s = bs " "
          n = bs "\n"


-- | Conduit the pretty prints an event stream into a file.
renderEvents :: C.ResourceUnsafeIO m => C.Conduit Event m B.ByteString
renderEvents = CL.map eventPrinter C.=$= builderToByteString
