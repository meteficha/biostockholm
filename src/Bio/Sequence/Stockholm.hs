{-# LANGUAGE CPP, EmptyDataDecls, DeriveDataTypeable, OverloadedStrings #-}

-- | Parsing and pretty printing of files in Stockholm 1.0
-- format.  See:
--
--    - <http://sonnhammer.sbc.su.se/Stockholm.html>
--
--    - <ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/relnotes.txt>
--
--    - <http://en.wikipedia.org/wiki/Stockholm_format>

module Bio.Sequence.Stockholm
    (-- * Data types
     Stockholm(..)
    ,StockholmSeq(..)
    ,Ann(..)
    ,FileAnnotation(..)
    ,SequenceAnnotation(..)
    ,ColumnAnnotation(..)
    ,InFile
    ,InSeq
    ,findAnn

     -- * Parsing
    ,parseStockholm
    ,StockholmExc(..)

     -- * Printing
    ,prettyPrintStockholm

#ifdef TEST
     -- * Test cases
    ,test_Stockholm
#endif
    )
    where

-- from base
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.DeepSeq (NFData(..))
import Control.Monad (mplus)
import Data.Char (isSpace)
import Data.List (foldl', find)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Text.Show (showParen, showString)

-- from containers
import qualified Data.Map as M

-- from bytestring
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)

-- from biocore
import Bio.Core.Sequence

-- from explicit-exception
import Control.Monad.Exception.Synchronous (Exceptional, throw)


#ifdef TEST
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit
#endif


-- | An Stockholm 1.0 formatted file represented in memory.
data Stockholm = Stockholm [Ann FileAnnotation]
                           [Ann (ColumnAnnotation InFile)]
                           [StockholmSeq]
                 deriving (Show, Eq, Typeable)

instance NFData Stockholm where
    rnf (Stockholm file clmn seqs) = rnf file `seq` rnf clmn `seq` rnf seqs

-- | A sequence in Stockholm 1.0 format.
data StockholmSeq = StSeq !SeqLabel
                          !SeqData
                          [Ann SequenceAnnotation]
                          [Ann (ColumnAnnotation InSeq)]
                    deriving (Eq, Typeable)

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
    seqlength (StSeq _ sd _ _) = Offset $ B.length (unSD sd)

-- | A generic annotation.
data Ann d = Ann { feature :: !d
                 , text    :: !ByteString
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

  | F_Other !ByteString -- ^ Other file annotation.
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

  | C_Other !ByteString -- ^ Other column annotation.
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

  | S_Other !ByteString -- ^ Other sequence annotation.
    deriving (Show, Eq, Ord, Typeable)


-- | Class used internally below just to simplify the code.
class IsAnnotation a where
    parseAnn :: ByteString -> a
    showAnn  :: a -> ByteString

mkParseAnn :: (ByteString -> a -> ByteString) -> [(ByteString, a)]
           -> (ByteString -> a) -> ByteString -> a
mkParseAnn modify anns mkOther =
    let annots = M.fromList anns
    in \feat -> let featMod = modify feat (error "mkParseAnn: never here")
                in fromMaybe (mkOther feat) $ M.lookup featMod annots

mkShowAnn :: Ord a => (ByteString -> a -> ByteString) -> [(ByteString, a)]
          -> (a -> Maybe ByteString) -> a -> ByteString
mkShowAnn modify anns fromOther =
    let annots = M.fromList [(a,b) | (b,a) <- anns]
    in \ann -> fromMaybe (error "mkShowAnn: never here 2") $
               fromOther ann `mplus` (mod' <$> M.lookup ann annots)
             where mod' = flip modify (error "mkShowAnn: never here 1")

instance IsAnnotation SequenceAnnotation where
    parseAnn = mkParseAnn const seqAnns S_Other
    showAnn  = mkShowAnn  const seqAnns f
        where f (S_Other o) = Just o
              f _           = Nothing

seqAnns :: [(ByteString, SequenceAnnotation)]
seqAnns = [("LO",LO), ("OC",OC), ("OS",OS),
           ("AC",S_AC), ("DE",S_DE), ("DR",S_DR)]


instance IsAnnotation FileAnnotation where
    parseAnn = mkParseAnn const fileAnns F_Other
    showAnn  = mkShowAnn  const fileAnns f
        where f (F_Other o) = Just o
              f _           = Nothing

fileAnns :: [(ByteString, FileAnnotation)]
fileAnns = [("AC",AC), ("AM",AM), ("AU",AU), ("CC",CC),
            ("DC",DC), ("DE",DE), ("DR",DR), ("GA",GA),
            ("ID",ID), ("KW",KW), ("NC",NC), ("NE",NE),
            ("NL",NL), ("PI",PI), ("RA",RA), ("RC",RC),
            ("RL",RL), ("RM",RM), ("RN",RN), ("RT",RT),
            ("SE",SE), ("SQ",SQ), ("TC",TC), ("TP",TP)]


instance ClmnAnnLoc a => IsAnnotation (ColumnAnnotation a) where
    parseAnn = mkParseAnn removeSuffix clmnAnns C_Other
        where
          removeSuffix feat phantom =
              let suffix = clmnAnnSuffix phantom
                  (f, s) = B.splitAt (B.length feat - B.length suffix) feat
              in if suffix == s then f else ""
    showAnn = mkShowAnn addSuffix clmnAnns f
        where
          f (C_Other o) = Just o
          f _           = Nothing
          addSuffix feat phantom = feat `B.append` clmnAnnSuffix phantom

clmnAnns :: [(ByteString, ColumnAnnotation a)]
clmnAnns = [("AS",AS), ("IN",IN), ("LI",LI), ("PAS",PAS), ("PP",PP),
            ("SA",SA), ("SAS",SAS), ("SS",SS), ("TM",TM)]

class ClmnAnnLoc a where
    clmnAnnSuffix :: b a -> ByteString
instance ClmnAnnLoc InSeq where
    clmnAnnSuffix _ = ""
instance ClmnAnnLoc InFile where
    clmnAnnSuffix _ = "_cons"

parseAnn' :: IsAnnotation a => ByteString -> ByteString -> Ann a
parseAnn' = Ann . parseAnn




#ifdef TEST
test_parseAnnots :: Specs
test_parseAnnots =
    describe "parse*" $ do
      it "1"  $ parseAnn "AC" @?= AC
      it "2"  $ parseAnn "SQ" @?= SQ
      it "3a" $ parseAnn "SS" @?= (SS :: ColumnAnnotation InSeq)
      it "3b" $ parseAnn "SS" @?= (C_Other "SS" :: ColumnAnnotation InFile)
      it "4a" $ parseAnn "SS_cons" @?= (SS :: ColumnAnnotation InFile)
      it "4b" $ parseAnn "SS_cons" @?= (C_Other "SS_cons" :: ColumnAnnotation InSeq)
      it "5a" $ parseAnn "SS_CONS" @?= (C_Other "SS_CONS" :: ColumnAnnotation InSeq)
      it "5b" $ parseAnn "SS_CONS" @?= (C_Other "SS_CONS" :: ColumnAnnotation InFile)
      it "6"  $ parseAnn "LO" @?= LO
#endif


-- | Find an annotation.  For example, you may use @'findAnn' 'SS'@
-- to find the secondary of an Stockholm file.
findAnn :: Eq d => d -> [Ann d] -> Maybe ByteString
findAnn x = fmap text . find ((== x) . feature)


-- | Exceptions that may happen while parsing a Stockholm file.
class StockholmExc e where
    -- | File is empty.
    emptyFileExc :: e

    -- | Header is missing.
    headerExc :: e

    -- | Malformed annotation.  The line is passed as argument.
    malformedAnnExc :: ByteString -> e

    -- | Unknown annotation type.
    unknownAnnTypeExc :: Char -> e

    -- | Malformed sequence line. The line is passed as argument.
    malformedSeqDataExc :: ByteString -> e

instance StockholmExc () where
    emptyFileExc          = ()
    headerExc             = ()
    malformedAnnExc     _ = ()
    unknownAnnTypeExc   _ = ()
    malformedSeqDataExc _ = ()

instance StockholmExc ByteString where
    emptyFileExc = "parseStockholm: empty file."
    headerExc = "parseStockholm: header is missing."
    malformedAnnExc line =
        B.concat ["parseStockholm: malformed annotation '", line, "'."]
    unknownAnnTypeExc typ =
        B.concat ["parseStockholm: unknown annotation type '", B.pack [typ], "'."]
    malformedSeqDataExc line =
        B.concat ["parseStockholm: malformed sequence data line '", line, "'."]
















-- | Type (F, C, S or R), corresponding sequence, name and data.
type ParseAnnRet = ((Char, Maybe SeqLabel, ByteString), ByteString)

-- | @parseAnnotation line@ tries to parse a line as an annotation.
parseAnnotation :: (StockholmExc e) => ByteString -> Exceptional e ParseAnnRet
parseAnnotation line
    | not (B.isPrefixOf "#=G" line) || B.length line < 5 =
        throw (malformedAnnExc line)
parseAnnotation line =
  let Just (typ, rest) = B.uncons $ B.drop 3 line
      (word1, text1) = second dropSpace . B.break isSpace $ dropSpace rest
      (word2, text2) = second dropSpace . B.break isSpace $ text1
      global = ((typ, Nothing,               word1), text1)
      seqspe = ((typ, Just (SeqLabel word1), word2), text2)
  in case typ of
       'F' -> return global
       'C' -> return global
       'S' -> return seqspe
       'R' -> return seqspe
       _ -> throw (unknownAnnTypeExc typ)

dropSpace :: ByteString -> ByteString
dropSpace = B.dropWhile isSpace


-- | @parseSeqData line@ tries to parse a line as some data of a sequence.
parseSeqData :: (StockholmExc e) => ByteString
             -> Exceptional e (SeqLabel, SeqData)
parseSeqData str = case B.words str of
                     [ident, sq] -> return (SeqLabel ident, SeqData sq)
                     _ -> throw (malformedSeqDataExc str)


-- | @parseStockholm@ parses a file in Stockholm 1.0 format.
--
--   Each file must be completely read before it is used because
--   the Stockholm format allows information to be given in any
--   part of the file.  However, there may be multiple
--   \"Stockholm files\" concatenated in a single \"filesystem
--   file\".  These multiple files are read independently, which
--   is why we return a list of 'Exceptional'@s@.
--
--   If you prefer to read the whole file in one go, use
--   @'sequence' (parseStockholm input)@, which will fail if any
--   family fails.
parseStockholm :: (StockholmExc e) => ByteString
               -> [Exceptional e Stockholm]
parseStockholm = map parseStockholm' . split .
                 filter (not . B.all isSpace) . B.lines
    where
      split [] = []
      split xs = case break (B.isPrefixOf "//") xs of
                   (y, ys) -> y : split (tail ys)

parseStockholm' :: (StockholmExc e) => [ByteString]
                -> Exceptional e Stockholm
parseStockholm' = header . filter (not . B.null)
    where
      -- Find
      header (h:hs)
          | h == stockholm = do (annots,seqs) <- go initial hs
                                return (makeStockholm annots seqs)
          | otherwise      = throw headerExc
          where stockholm = "# STOCKHOLM 1.0"
                initial   = (M.empty, M.empty)
      header [] = throw emptyFileExc

      -- End of file
      go acc [] = return acc

      -- Annotation
      go (annots,seqs) (line:ls) | B.take 2 line == "#=" = do
        annot <- parseAnnotation line
        go (insertDM annot annots, seqs) ls

      -- Comment
      go (annots,seqs) (l:ls) | B.head l == '#' =
        go (annots,seqs) ls

      -- Otherwise a sequence
      go (annots,seqs) (line:ls) = do
        seqData <- parseSeqData line
        go (annots, insertDM seqData seqs) ls


type DiffMap a b = M.Map a ([b] -> [b])

insertDM :: Ord a => (a,b) -> DiffMap a b -> DiffMap a b
insertDM (key,val) = M.insertWith (flip (.)) key (val:)

finishDM :: (b -> ByteString) -> DiffMap a b -> M.Map a ByteString
finishDM f = fmap (B.concat . map f . ($ []))

-- | Glue everything in place, as the Stockholm format lets
--   everything be everywhere and split in any number of parts.
makeStockholm :: DiffMap (Char, Maybe SeqLabel, ByteString) ByteString
              -> DiffMap SeqLabel SeqData -> Stockholm
makeStockholm annotsDM seqsDM =
    let annots = finishDM id   annotsDM
        seqs   = finishDM unSD seqsDM

        -- Separate the global from the sequence annotations
        go ('F', Nothing, feat) txt (f,c,r) = (parseAnn' feat txt:f,c,r)
        go ('C', Nothing, feat) txt (f,c,r) = (f,parseAnn' feat txt:c,r)
        go (typ, Just sq, feat) txt (f,c,r) = (f,c,(typ,sq,feat,txt):r)
        go _ _ _ = error "makeStockholm: not here, ever"

        -- Separate the sequence annotations to each annotation
        add ('S',sq,feat,txt) = flip M.adjust sq $ \(StSeq sl sd sa ca) ->
                                  StSeq sl sd (parseAnn' feat txt : sa) ca
        add ('R',sq,feat,txt) = flip M.adjust sq $ \(StSeq sl sd sa ca) ->
                                  StSeq sl sd sa (parseAnn' feat txt : ca)
        add _ = error "makeStockholm: not here either"

        -- Glue everything
        (file, clmn, rest) = M.foldWithKey go ([],[],[]) annots
        plainseqs = M.mapWithKey (\k s -> StSeq k (SeqData s) [] []) seqs
    in Stockholm file clmn (M.elems $ foldl' (flip add) plainseqs rest)




-- | Pretty-prints an Stockholm file.  We follow Rfam preferences
-- and do not wrap lines.
prettyPrintStockholm :: Stockholm -> B.ByteString
prettyPrintStockholm (Stockholm file clmn seqs) =
    let showAnnF :: IsAnnotation a => Char -> Ann a -> (ByteString, ByteString)
        showAnnF t ann = (B.concat [B.pack ("#=G" ++ t : " "),
                                    showAnn (feature ann)], text ann)
        showAnnS :: IsAnnotation a => ByteString -> Char -> Ann a -> (ByteString, ByteString)
        showAnnS s t ann = (B.unwords [B.pack ("#=G" ++ [t]), s,
                                       showAnn (feature ann)], text ann)

        fileLines = map (showAnnF 'F') file
        clmnLines = map (showAnnF 'C') clmn
        sequences = do
          StSeq (SeqLabel name) (SeqData seqd) sa ca <- seqs
          (name, seqd) : map (showAnnS name 'R') ca
                      ++ map (showAnnS name 'S') sa

        allLines    = fileLines ++ sequences ++ clmnLines
        firstColLen = maximum $ map (B.length . fst) allLines
        mkLine (col1, col2) = B.concat [col1, B.replicate n ' ', col2]
            where n = 1 + firstColLen - B.length col1
    in B.unlines ("# STOCKHOLM 1.0" : map mkLine allLines ++ ["//"])



#ifdef TEST
stockFile :: B.ByteString
stockFile = B.unlines [
  "# STOCKHOLM 1.0",
  "#=GF AU Infernal 1.0",
  "",
  "#=GS Purine1 DE Number 1 :)",
  "Purine1      AAAAUUGAAUAUCGUUUUACUUGUUUAUGUC-GUGAAU-UGGCAC-GACG",
  "Purine2      AAAAUUUAAUAA-GAAGCACUCAUAUAAUCCCGAGAAUAUGGCUCGGGAG",
  "Purine3      UGGCAGUAACUAGCGUCACUUCGUAUAACCCCAGUGAUAUGGAUUGGGGG",
  "#=GC SS_cons :::::::::::::::::((((((((,,,<<<-<<<_______>>>->>>,",
  "",
  "# We may have comments =)",
  "",
  "Purine1      UUUCUACAAGGUG-CCGGAA--CACCUAACAAUAAGUAAGUCAGCAGUGA",
  "Purine2      UCUCUACCGAACAACCGUAAAUUGUUCGACUAUGAGUGAAAGUGUACCUA",
  "Purine3      UCUCUACCAGGAACCAAUAA--AUCCUGAUUACGAAGAGUUUAGUGCUUU",
  "#=GC SS_cons ,,,,,,,<<<<<<_________>>>>>>,,))))))))::::::::::::",
  "",
  "Purine1      GAU",
  "Purine2      GGG",
  "Purine3      AGU",
  "#=GC SS_cons :::",
  "// "]

purine1, purine2, purine3 :: SeqData
ss_cons :: ByteString
purine1 = SeqData "AAAAUUGAAUAUCGUUUUACUUGUUUAUGUC-GUGAAU-UGGCAC-GACGUUUCUACAAGGUG-CCGGAA--CACCUAACAAUAAGUAAGUCAGCAGUGAGAU"
purine2 = SeqData "AAAAUUUAAUAA-GAAGCACUCAUAUAAUCCCGAGAAUAUGGCUCGGGAGUCUCUACCGAACAACCGUAAAUUGUUCGACUAUGAGUGAAAGUGUACCUAGGG"
purine3 = SeqData "UGGCAGUAACUAGCGUCACUUCGUAUAACCCCAGUGAUAUGGAUUGGGGGUCUCUACCAGGAACCAAUAA--AUCCUGAUUACGAAGAGUUUAGUGCUUUAGU"
ss_cons =         ":::::::::::::::::((((((((,,,<<<-<<<_______>>>->>>,,,,,,,,<<<<<<_________>>>>>>,,)))))))):::::::::::::::"

result :: [Stockholm]
result = [Stockholm file clmn seqs]
    where
      file = [Ann AU "Infernal 1.0"]
      clmn = [Ann SS ss_cons]
      seqs = [mkStock "Purine1" purine1 [Ann S_DE "Number 1 :)"],
              mkStock "Purine2" purine2 [],
              mkStock "Purine3" purine3 []]
      mkStock name data_ sa = StSeq name data_ sa []

stockFile2 :: B.ByteString
stockFile2 = B.unlines [stockFile, stockFile]

result2 :: [Stockholm]
result2 = result ++ result

returnExc :: a -> Exceptional B.ByteString a
returnExc = return

test_parseStockholm :: Specs
test_parseStockholm =
    describe "parseStockholm" $ do
      it "correctly parses test file 1" $ parseStockholm stockFile  @?= returnExc result
      it "correctly parses test file 2" $ parseStockholm stockFile2 @?= returnExc result2

test_prettyPrintStockholm :: Specs
test_prettyPrintStockholm =
    describe "parseStockholm/prettyPrintStockholm" $ do
      it "parses printed test file 1" $ parseStockholm (func result)  @?= returnExc result
      it "parses printed test file 2" $ parseStockholm (func result2) @?= returnExc result2
    where func = B.unlines . map prettyPrintStockholm
#endif


#ifdef TEST
test_Stockholm :: Specs
test_Stockholm = describe "Bio.Sequence.Stockholm" $ do
                   test_parseAnnots
                   test_parseStockholm
                   test_prettyPrintStockholm
#endif
