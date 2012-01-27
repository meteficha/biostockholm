{-# LANGUAGE BangPatterns, CPP, EmptyDataDecls, DeriveDataTypeable, OverloadedStrings #-}

-- | Take low-level events and turn them high-level data structures.
module Bio.Sequence.Stockholm.Document
    ( -- * Data types
      Stockholm(..)
    , StockholmSeq(..)
    , Ann(..)
    , FileAnnotation(..)
    , SequenceAnnotation(..)
    , ColumnAnnotation(..)
    , InFile
    , InSeq

      -- * Conduits
    , parseDoc
    , renderDoc
    )
    where

-- from base
import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.Monad (mplus)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

-- from containers
import qualified Data.Map as M

-- from bytestring
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- from biocore
import Bio.Core.Sequence

-- from conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

-- from this package
import Bio.Sequence.Stockholm.Stream



----------------------------------------------------------------------
-- Types

-- | An Stockholm 1.0 formatted file represented in memory.
data Stockholm = Stockholm [Ann FileAnnotation]
                           [Ann (ColumnAnnotation InFile)]
                           [StockholmSeq]
                 deriving (Show, Eq, Ord, Typeable)

instance NFData Stockholm where
    rnf (Stockholm file clmn seqs) = rnf file `seq` rnf clmn `seq` rnf seqs


-- | A sequence in Stockholm 1.0 format.
data StockholmSeq = StSeq !SeqLabel
                          !SeqData
                          [Ann SequenceAnnotation]
                          [Ann (ColumnAnnotation InSeq)]
                    deriving (Eq, Ord, Typeable)

-- We don't derive Show to be able support biocore-0.1, which
-- doesn't have Show instances for SeqLabel and SeqData.
instance Show StockholmSeq where
    showsPrec prec (StSeq (SeqLabel l) (SeqData d) sa ca) =
        showParen (prec > 10) $
          showString "StSeq (SeqLabel " .
          showsPrec 11 l .
          showString ") (SeqData " .
          showsPrec 11 d .
          (')':) . (' ':) .
          showsPrec 11 sa .
          (' ':) .
          showsPrec 11 ca

instance NFData StockholmSeq where
    rnf (StSeq _ _ sa ca) = rnf sa `seq` rnf ca

instance BioSeq StockholmSeq where
    seqlabel  (StSeq sl _ _ _) = sl
    seqdata   (StSeq _ sd _ _) = sd
    seqlength (StSeq _ sd _ _) = Offset $ L.length (unSD sd)


-- | A generic annotation.
data Ann d = Ann { feature :: !d
                 , text    :: !L.ByteString
                 }
             deriving (Show, Eq, Ord, Typeable)

instance NFData (Ann d) where
    -- already strict, default instance


-- | Possible file annotations.
data FileAnnotation =
    AC -- ^ Accession number:    Accession number in form PFxxxxx.version or PBxxxxxx.
  | ID -- ^ Identification:      One word name for family.
  | DE -- ^ Definition:          Short description of family.
  | AU -- ^ Author:              Authors of the entry.
  | SE -- ^ Source of seed:      The source suggesting the seed members belong to one family.
  | GA -- ^ Gathering method:    Search threshold to build the full alignment.
  | TC -- ^ Trusted Cutoff:      Lowest sequence score and domain score of match in the full alignment.
  | NC -- ^ Noise Cutoff:        Highest sequence score and domain score of match not in full alignment.
  | TP -- ^ Type:                Type of family (presently Family, Domain, Motif or Repeat).
  | SQ -- ^ Sequence:            Number of sequences in alignment.
  | AM -- ^ Alignment Method:    The order ls and fs hits are aligned to the model to build the full align.

  | DC -- ^ Database Comment:    Comment about database reference.
  | DR -- ^ Database Reference:  Reference to external database.
  | RC -- ^ Reference Comment:   Comment about literature reference.
  | RN -- ^ Reference Number:    Reference Number.
  | RM -- ^ Reference Medline:   Eight digit medline UI number.
  | RT -- ^ Reference Title:     Reference Title.
  | RA -- ^ Reference Author:    Reference Author
  | RL -- ^ Reference Location:  Journal location.
  | PI -- ^ Previous identifier: Record of all previous ID lines.
  | KW -- ^ Keywords:            Keywords.
  | CC -- ^ Comment:             Comments.
  | NE -- ^ Pfam accession:      Indicates a nested domain.
  | NL -- ^ Location:            Location of nested domains - sequence ID, start and end of insert.

  | F_Other !B.ByteString -- ^ Other file annotation.
    deriving (Show, Eq, Ord, Typeable)


-- | Possible column annotations.  Phantom type can be 'InFile'
-- or 'InSeq'.
data ColumnAnnotation a =
    SS -- ^ Secondary structure.
  | SA -- ^ Surface accessibility.
  | TM -- ^ TransMembrane.
  | PP -- ^ Posterior probability.
  | LI -- ^ LIgand binding.
  | AS -- ^ Active site.
  | PAS -- ^ AS - Pfam predicted.
  | SAS -- ^ AS - from SwissProt.
  | IN -- ^ INtron (in or after).

  | C_Other !B.ByteString -- ^ Other column annotation.
    deriving (Show, Eq, Ord, Typeable)


-- | Phantom type for 'ColumnAnnotation's of the whole file.
data InFile


-- | Phantom type for 'ColumnAnnotation's of a single sequence.
data InSeq


-- | Possible sequence annotations.
data SequenceAnnotation =
    S_AC -- ^ Accession number
  | S_DE -- ^ Description
  | S_DR -- ^ Database reference
  | OS -- ^ Organism (species)
  | OC -- ^ Organism classification (clade, etc.)
  | LO -- ^ Look (Color, etc.)

  | S_Other !B.ByteString -- ^ Other sequence annotation.
    deriving (Show, Eq, Ord, Typeable)




----------------------------------------------------------------------
-- Parsing and showing features


-- | Parse a feature.
type ParseFeature a = B.ByteString -> a


-- | Show a feature.
type ShowFeature  a = a -> B.ByteString


-- | Helper to create 'ParseFeature'@s@.
mkParseFeature :: (B.ByteString -> a -> B.ByteString)
           -> [(B.ByteString, a)]
           -> (B.ByteString -> a)
           -> ParseFeature a
mkParseFeature modify anns mkOther =
    let annots = M.fromList anns
    in \feat -> let featMod = modify feat (error "mkParseFeature: never here")
                in fromMaybe (mkOther feat) $ M.lookup featMod annots


-- | Helper to create 'ShowFeature'@s@.
mkShowFeature :: Ord a =>
             (B.ByteString -> a -> B.ByteString)
          -> [(B.ByteString, a)]
          -> (a -> Maybe B.ByteString)
          -> ShowFeature a
mkShowFeature modify anns fromOther =
    let annots = M.fromList [(a,b) | (b,a) <- anns]
    in \ann -> fromMaybe (error "mkShowFeature: never here 2") $
               fromOther ann `mplus` (mod' <$> M.lookup ann annots)
             where mod' = flip modify (error "mkShowFeature: never here 1")


-- | Parse and show sequence annotations.
parseSeqFeature :: ParseFeature SequenceAnnotation
showSeqFeature  :: ShowFeature  SequenceAnnotation
(parseSeqFeature, showSeqFeature) =
    ( mkParseFeature const seqFeatures S_Other
    , mkShowFeature  const seqFeatures f )
    where
      f (S_Other o) = Just o
      f _           = Nothing

      seqFeatures = [("LO",LO), ("OC",OC), ("OS",OS),
                     ("AC",S_AC), ("DE",S_DE), ("DR",S_DR)]


-- | Parse and show file annotations.
parseFileFeature :: ParseFeature FileAnnotation
showFileFeature  :: ShowFeature  FileAnnotation
(parseFileFeature, showFileFeature) =
    ( mkParseFeature const fileFeatures F_Other
    , mkShowFeature  const fileFeatures f )
    where
      f (F_Other o) = Just o
      f _           = Nothing

      fileFeatures = [("AC",AC), ("AM",AM), ("AU",AU), ("CC",CC),
                      ("DC",DC), ("DE",DE), ("DR",DR), ("GA",GA),
                      ("ID",ID), ("KW",KW), ("NC",NC), ("NE",NE),
                      ("NL",NL), ("PI",PI), ("RA",RA), ("RC",RC),
                      ("RL",RL), ("RM",RM), ("RN",RN), ("RT",RT),
                      ("SE",SE), ("SQ",SQ), ("TC",TC), ("TP",TP)]


-- | Parse and show column annotations.
parseClmnFeature :: ClmnFeatureLoc a => ParseFeature (ColumnAnnotation a)
parseClmnFeature = mkParseFeature removeSuffix clmnFeatures C_Other
    where
      removeSuffix feat phantom =
          let suffix = clmnFeatureSuffix phantom
              (f, s) = B.splitAt (B.length feat - B.length suffix) feat
          in if suffix == s then f else ""

showClmnFeature  :: ClmnFeatureLoc a => ShowFeature  (ColumnAnnotation a)
showClmnFeature = mkShowFeature addSuffix clmnFeatures f
    where
      f (C_Other o) = Just o
      f _           = Nothing

      addSuffix feat phantom = feat `B.append` clmnFeatureSuffix phantom

clmnFeatures :: [(B.ByteString, ColumnAnnotation a)]
clmnFeatures = [("AS",AS), ("IN",IN), ("LI",LI), ("PAS",PAS), ("PP",PP),
                ("SA",SA), ("SAS",SAS), ("SS",SS), ("TM",TM)]

class ClmnFeatureLoc a where
    clmnFeatureSuffix :: b a -> B.ByteString
instance ClmnFeatureLoc InSeq where
    clmnFeatureSuffix _ = ""
instance ClmnFeatureLoc InFile where
    clmnFeatureSuffix _ = "_cons"



----------------------------------------------------------------------
-- Specilized Maps.

type DiffMap a b = M.Map a [b]

insertDM :: Ord a => (a, b) -> DiffMap a b -> DiffMap a b
insertDM (key, val) = M.insertWith' (\_ old -> val:old) key [val]

finishDM :: (b -> L.ByteString) -> DiffMap a b -> M.Map a L.ByteString
finishDM f = fmap (L.concat . map f . reverse)

type AnnMap d = DiffMap d L.ByteString

insertAnn :: Ord d => Ann d -> AnnMap d -> AnnMap d
insertAnn (Ann key val) = insertDM (key, val)

finishAnn :: AnnMap d -> [Ann d]
finishAnn m = [Ann a b | (a, b) <- M.toList (finishDM id m)]

type SeqAnnMap d = M.Map B.ByteString (AnnMap d)

insertSM :: Ord d => B.ByteString -> Ann d -> SeqAnnMap d -> SeqAnnMap d
insertSM sq ann = M.alter (just . insertAnn ann . fromMaybe M.empty) sq
    where
      just !x = Just x

finishSM :: SeqAnnMap d -> M.Map B.ByteString [Ann d]
finishSM = fmap finishAnn

data PartialAnns =
    PartialAnns { paFileAnns    :: !(AnnMap FileAnnotation)
                , paFileColAnns :: !(AnnMap (ColumnAnnotation InFile))
                , paSeqAnns     :: !(SeqAnnMap SequenceAnnotation)
                , paSeqColAnns  :: !(SeqAnnMap (ColumnAnnotation InSeq))
                }

emptyPA :: PartialAnns
emptyPA = PartialAnns M.empty M.empty M.empty M.empty

insertPA_GF ::                 Ann (FileAnnotation         ) -> PartialAnns -> PartialAnns
insertPA_GC ::                 Ann (ColumnAnnotation InFile) -> PartialAnns -> PartialAnns
insertPA_GS :: B.ByteString -> Ann (SequenceAnnotation     ) -> PartialAnns -> PartialAnns
insertPA_GR :: B.ByteString -> Ann (ColumnAnnotation InSeq ) -> PartialAnns -> PartialAnns
insertPA_GF    ann pa = pa { paFileAnns    = insertAnn ann (paFileAnns pa)     }
insertPA_GC    ann pa = pa { paFileColAnns = insertAnn ann (paFileColAnns pa)  }
insertPA_GS sq ann pa = pa { paSeqAnns     = insertSM sq ann (paSeqAnns pa)    }
insertPA_GR sq ann pa = pa { paSeqColAnns  = insertSM sq ann (paSeqColAnns pa) }




----------------------------------------------------------------------
-- [Event <-> Document] conversion functions


-- | Conduit that parses 'Event'@s@ into documents 'Stockholm'.
parseDoc :: C.Resource m => C.Conduit Event m Stockholm
parseDoc = C.conduitState LookingForHeader push close
    where

      -- FIXME: Nice exceptions

      close LookingForHeader              = return []
      close (InsideStockholm annots seqs) = return [makeStockholm annots seqs]

      push state (EvComment _) =
          return (state, C.Producing [])

      push LookingForHeader EvHeader =
          continue (emptyPA, M.empty)
      push LookingForHeader x =
          fail $ "parseDoc: unexpected " ++ show x ++ " before header"

      push (InsideStockholm _ _) EvHeader =
          fail "parseDoc: unexpected header"
      push (InsideStockholm annots seqs) EvEnd =
          return (LookingForHeader, C.Producing [makeStockholm annots seqs])
      push (InsideStockholm annots seqs) (EvSeqData label data_) =
          continue (annots, insertDM (label, l data_) seqs)
      push (InsideStockholm annots seqs) (EvGF feat data_) =
          continue (insertPA_GF (Ann (parseFileFeature feat) (l data_)) annots, seqs)
      push (InsideStockholm annots seqs) (EvGC feat data_) =
          continue (insertPA_GC (Ann (parseClmnFeature feat) (l data_)) annots, seqs)
      push (InsideStockholm annots seqs) (EvGS sq feat data_) =
          continue (insertPA_GS sq (Ann (parseSeqFeature feat) (l data_)) annots, seqs)
      push (InsideStockholm annots seqs) (EvGR sq feat data_) =
          continue (insertPA_GR sq (Ann (parseClmnFeature feat) (l data_)) annots, seqs)

      continue (annots, seqs) = return (InsideStockholm annots seqs, C.Producing [])
      {-# INLINE continue #-}

      l = L.fromChunks . return

data ParseDoc = LookingForHeader
              | InsideStockholm
                  { pdAnnots :: {-# UNPACK #-} !PartialAnns
                  , pdSeqs   :: {-# UNPACK #-} !(DiffMap B.ByteString L.ByteString)
                  }


-- | Glue everything into place, as the Stockholm format lets
--   everything be everywhere and split in any number of parts.
makeStockholm :: PartialAnns -> DiffMap B.ByteString L.ByteString -> Stockholm
makeStockholm annots seqsDM =
    let fileAnns_   = finishAnn (paFileAnns    annots)
        fileColAnns = finishAnn (paFileColAnns annots)
        seqAnns_    = finishSM  (paSeqAnns     annots)
        seqColAnns  = finishSM  (paSeqColAnns  annots)

        stseqs = [StSeq (SeqLabel $ l sq) (SeqData dt) (f sq seqAnns_) (f sq seqColAnns)
                   | (sq, dt) <- M.toList (finishDM id seqsDM)]
            where
              f = M.findWithDefault []
              l = L.fromChunks . return
    in Stockholm fileAnns_ fileColAnns stseqs


-- | Conduit that renders 'Stockholm'@s@ into 'Event'@s@.
renderDoc :: C.Resource m => C.Conduit Stockholm m Event
renderDoc = CL.concatMap toEvents
    where
      toEvents (Stockholm file clmn seqs) =
          (EvHeader:) $
          toEventsFileAnns file $
          toEventsSeqs     seqs $
          toEventsFileClmn clmn $
          [EvEnd]

      toEventsFileAnns []     = id
      toEventsFileAnns (a:as) =
          (EvGF (showFileFeature $ feature a) (strict $ text a) :) .
          toEventsFileAnns as

      toEventsFileClmn []     = id
      toEventsFileClmn (a:as) =
          wrap (EvGC (showClmnFeature $ feature a)) (text a) .
          toEventsFileClmn as

      toEventsSeqs (StSeq (SeqLabel name) (SeqData seqd) sa ca : xs) =
          wrap (EvSeqData name') seqd .
          toEventsSeqAnns name' sa .
          toEventsSeqClmn name' ca .
          toEventsSeqs xs
              where name' = strict name
      toEventsSeqs [] = id

      toEventsSeqAnns _ []     = id
      toEventsSeqAnns n (a:as) =
          (EvGS n (showSeqFeature $ feature a) (strict $ text a) :) .
          toEventsSeqAnns n as

      toEventsSeqClmn _ []     = id
      toEventsSeqClmn n (a:as) =
          wrap (EvGR n (showClmnFeature $ feature a)) (text a) .
          toEventsSeqClmn n as

      wrap :: (B.ByteString -> b) -> L.ByteString -> [b] -> [b]
      wrap mk bs = case L.splitAt 70 bs of
                     (x, "") -> (mk (strict x) :)
                     (x, xs) -> (mk (strict x) :) . wrap mk xs

      strict = B.concat . L.toChunks
