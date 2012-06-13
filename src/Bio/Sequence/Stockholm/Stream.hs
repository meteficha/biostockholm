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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

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
           | EvComment L.ByteString
             -- ^ @# ....@
           | EvSeqData B.ByteString L.ByteString
             -- ^ @seqlabel seqdata@
           | EvGF B.ByteString L.ByteString
             -- ^ @#GF feature data@
           | EvGC B.ByteString L.ByteString
             -- ^ @#GC feature data@
           | EvGS B.ByteString B.ByteString L.ByteString
             -- ^ @#GS seqlabel feature data@
           | EvGR B.ByteString B.ByteString L.ByteString
             -- ^ @#GR seqlabel feature data@
             deriving (Eq, Ord, Show)


-- | Parse an 'Event' after 'EvHeader'.
eventParser :: A.Parser Event
eventParser = hash *> (ann <|> comment)
          <|> end
          <|> seqdata
              A.<?> "Event"
    where
      word = A.takeTill A8.isHorizontalSpace <* spaces
      tillNextLine = A.takeLazyByteString

      hash    = A8.char '#'
      comment = EvComment <$> tillNextLine A.<?> "Comment"
      end     = EvEnd     <$  A8.string "//" <* spaces A.<?> "End of Stockholm file (//)"
      seqdata = EvSeqData <$> word <*> tillNextLine A.<?> "Sequence data"

      ann = A8.string "=G" *> (gf <|> gc <|> gs <|> gr) A.<?> "Annotation"
          where
            gf = EvGF <$ A8.char 'F' <* spaces          <*> word <*> tillNextLine
            gc = EvGC <$ A8.char 'C' <* spaces          <*> word <*> tillNextLine
            gs = EvGS <$ A8.char 'S' <* spaces <*> word <*> word <*> tillNextLine
            gr = EvGR <$ A8.char 'R' <* spaces <*> word <*> word <*> tillNextLine

-- | Parse 'EvHeader'.
headerParser :: A.Parser Event
headerParser = EvHeader <$ A8.char '#' <* spaces <* mystring "STOCKHOLM 1.0" <* spaces
               A.<?> "Stockholm Header"
    where
      mystring (x:xs) = A8.char x *> mystring xs
      mystring []     = pure ()

spaces :: A.Parser ()
spaces = A.skipWhile  A8.isHorizontalSpace


-- | Conduit that parses a file into events.
parseEvents :: C.MonadThrow m => C.Conduit B.ByteString m Event
parseEvents = C.sequenceSink LookingForHeader go
    where
      go LookingForHeader = do
        dropSpaces
        let emit = C.Emit InsideStockholm . (:[])
        insideLine C.=$ (sinkParser $  C.Stop <$  A8.endOfInput
                                   <|> emit   <$> headerParser)

      go InsideStockholm = do
        dropSpaces
        event <- insideLine C.=$ sinkParser eventParser
        let newState = case event of
                         EvEnd -> LookingForHeader
                         _     -> InsideStockholm
        return $ C.Emit newState [event]

      dropSpaces = CB.dropWhile A8.isSpace_w8
      insideLine = CB.takeWhile (/= 10)

data ParseEvents = LookingForHeader | InsideStockholm


-- | Pretty print an event.
eventPrinter :: Event -> Blaze.Builder
eventPrinter ev =
    case ev of
      EvHeader                    -> bs "# STOCKHOLM 1.0\n"
      EvEnd                       -> bs "//\n"
      EvComment comment           -> bs "#" <> lbs comment <> n
      EvSeqData seqlabel seqdata  -> bs seqlabel <> s <> lbs seqdata <> n
      EvGF          feature data_ -> bs "#=GF " <> bs feature  <> s <> lbs data_ <> n
      EvGC          feature data_ -> bs "#=GC " <> bs feature  <> s <> lbs data_ <> n
      EvGS seqlabel feature data_ -> bs "#=GS " <> bs seqlabel <> s <> bs feature <> s <> lbs data_ <> n
      EvGR seqlabel feature data_ -> bs "#=GR " <> bs seqlabel <> s <> bs feature <> s <> lbs data_ <> n
    where bs  = Blaze.fromByteString
          lbs = Blaze.fromLazyByteString
          (<>) = mappend
          s = bs " "
          n = bs "\n"


-- | Conduit that pretty prints an event stream into a file.
renderEvents :: C.MonadUnsafeIO m => C.Conduit Event m B.ByteString
renderEvents = CL.map eventPrinter C.=$= builderToByteString
