{-# LANGUAGE BangPatterns, CPP, EmptyDataDecls, DeriveDataTypeable, OverloadedStrings #-}

-- | Parsing and pretty printing of files in Stockholm 1.0
-- format.  See:
--
--    - <http://sonnhammer.sbc.su.se/Stockholm.html>
--
--    - <ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/relnotes.txt>
--
--    - <http://en.wikipedia.org/wiki/Stockholm_format>

module Bio.Sequence.Stockholm
    ( -- * Data types
      Stockholm(..)
    , StockholmSeq(..)
    , Ann(..)
    , FileAnnotation(..)
    , SequenceAnnotation(..)
    , ColumnAnnotation(..)
    , InFile
    , InSeq
    , findAnn

      -- * Parsing
    , parseStockholm

      -- * Printing
    , renderStockholm

      -- * Lazy I/O
    , lazyParseStockholm
    , lazyRenderStockholm
    )
    where

-- from base
import Data.List (find)
import System.IO.Unsafe (unsafePerformIO)

-- from bytestring
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- from conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.Lazy as CZ
import qualified Data.Conduit.List as CL

-- from this package
import Bio.Sequence.Stockholm.Stream
import Bio.Sequence.Stockholm.Document


-- | Find an annotation.  For example, you may use @'findAnn' 'SS'@
-- to find the secondary on an Stockholm file.
findAnn :: Eq d => d -> [Ann d] -> Maybe L.ByteString
findAnn x = fmap text . find ((== x) . feature)


-- | @parseStockholm@ parses a stream of files in Stockholm 1.0
-- format.
--
-- Each file must be completely read before it is used because
-- the Stockholm format allows information to be given in any
-- part of the file.  However, there may be multiple \"Stockholm
-- files\" concatenated in a single \"filesystem file\".  These
-- multiple files are read independently.  If you need to process
-- large Stockholm files, consider using the streaming interface
-- on "Bio.Sequence.Stockholm.Stream".
parseStockholm :: C.MonadThrow m => C.Conduit B.ByteString m Stockholm
parseStockholm = parseEvents C.=$= parseDoc


-- | Pretty prints an Stockholm file.
renderStockholm :: C.MonadUnsafeIO m => C.Conduit Stockholm m B.ByteString
renderStockholm = renderDoc C.=$= renderEvents


-- | Use lazy I/O to parse a stream of files in Stockholm 1.0
-- format.  We recommend using 'parseStockholm'.
lazyParseStockholm :: L.ByteString -> [Stockholm]
lazyParseStockholm lbs =
    unsafePerformIO $
    C.runResourceT $
      CZ.lazyConsume $
        CL.sourceList (L.toChunks lbs) C.$=
        parseStockholm
{-# NOINLINE lazyParseStockholm #-}


-- | Use lazy I/O to render a list of 'Stockholm'@s@ into a
-- stream of files in Stockholm 1.0 format.  We recommend using
-- 'renderStockholm'.
lazyRenderStockholm :: [Stockholm] -> L.ByteString
lazyRenderStockholm stos =
    L.fromChunks $
    unsafePerformIO $
    C.runResourceT $
     CZ.lazyConsume $
       CL.sourceList stos C.$=
       renderStockholm
{-# NOINLINE lazyRenderStockholm #-}
