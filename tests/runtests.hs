{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- from base
import Control.Applicative
import Control.Monad (zipWithM_)
import Data.List (sort)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)

-- from containers
import qualified Data.Map as M

-- from bytestring
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- from biocore
import Bio.Core.Sequence

-- from transformers
import Control.Monad.IO.Class (liftIO)

-- from conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Lazy as CZ
import qualified Data.Conduit.List as CL

-- from QuickCheck
import Test.QuickCheck

-- from hspec
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)
import Test.HUnit

-- from this package
import Bio.Sequence.Stockholm
import Bio.Sequence.Stockholm.Document
import Bio.Sequence.Stockholm.Stream


main :: IO ()
main =
  hspecX $ do
    describe "parseStockholm" $ do
      it "correctly parses test file 1" $ do
        ret <- strictParse stockFile
        ret @?= result
      it "correctly parses test file 2" $ do
        ret <- strictParse stockFile2
        ret @?= result2

    describe "renderStockholm/parseStockholm" $ do
      it "parses rendered test file 1" $ do
        rendered <- strictRender result
        again    <- strictParse  rendered
        again @?= result
      it "parses rendered test file 2" $ do
        rendered <- strictRender result2
        again    <- strictParse  rendered
        again @?= result2
      prop "passes QuickCheck property" $ \(sto :: [Stockholm]) ->
        unsafePerformIO $ do
          rendered <- strictRender sto
          again    <- strictParse  rendered
          return (canonical again == canonical sto)

    describe "parseStockholm/renderStockholm/parseStockholm" $ do
      it "roundtrips RFAM" $ do
        rfamFp <- getEnv "RFAM"
        C.runResourceT $ do
          parsed1 <- CZ.lazyConsume $ CB.sourceFile rfamFp C.$= parseStockholm
          parsed2 <- CZ.lazyConsume $ CL.sourceList parsed1 C.$= renderStockholm C.$= parseStockholm
          liftIO (zipWithM_ (@?=) parsed2 parsed1)

    describe "renderEvents/parseEvents" $ do
      prop "passes QuickCheck property" $ \(events :: [Event]) ->
        unsafePerformIO $ do
          rendered <- C.runResourceT $ CL.sourceList events   C.$$ renderEvents C.=$ CL.consume
          again    <- C.runResourceT $ CL.sourceList rendered C.$$ parseEvents C.=$ CL.consume
          return (canonical again == canonical events)

    -- -- Needs a better Arbitrary instance for [Event], since
    -- -- eventList may generate, for example, annotations for
    -- -- sequences that don't exist.
    -- describe "parseDoc/renderDoc" $ do
    --   prop "passes QuickCheck property" $ forAll eventList $ \(events :: [Event]) ->
    --     unsafePerformIO $ do
    --       rendered <- C.runResourceT $ CL.sourceList events   C.$$ parseDoc  C.=$ CL.consume
    --       again    <- C.runResourceT $ CL.sourceList rendered C.$$ renderDoc C.=$ CL.consume
    --       return (canonical again == canonical events)

    describe "renderDoc/parseDoc" $ do
      prop "passes QuickCheck property" $ \(docs :: [Stockholm]) ->
        unsafePerformIO $ do
          rendered <- C.runResourceT $ CL.sourceList docs     C.$$ renderDoc C.=$ CL.consume
          again    <- C.runResourceT $ CL.sourceList rendered C.$$ parseDoc  C.=$ CL.consume
          return (canonical again == canonical docs)





----------------------------------------------------------------------


stockFile :: L.ByteString
stockFile = L.unlines [
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
ss_cons :: L.ByteString
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

stockFile2 :: L.ByteString
stockFile2 = L.unlines [stockFile, stockFile]

result2 :: [Stockholm]
result2 = result ++ result

sourceLBS :: C.Resource m => L.ByteString -> C.Source m B.ByteString
sourceLBS = CL.sourceList . L.toChunks

strictParse :: L.ByteString -> IO [Stockholm]
strictParse lbs = C.runResourceT $
                    sourceLBS lbs C.$=
                    parseStockholm C.$$
                    CL.consume

strictRender :: [Stockholm] -> IO L.ByteString
strictRender stos = fmap L.fromChunks $
                    C.runResourceT $
                      CL.sourceList stos C.$=
                      renderStockholm C.$$
                      CL.consume


----------------------------------------------------------------------


instance Arbitrary Event where
    arbitrary = frequency
      [ (3,  EvComment <$> arbitrary)
      , (10, EvSeqData <$> seqlabel <*> seqdata)
      , (2,  EvGF      <$> feature <*> arbitrary)
      , (2,  EvGC      <$> feature <*> arbitrary)
      , (2,  EvGS      <$> seqlabel <*> feature <*> arbitrary)
      , (2,  EvGR      <$> seqlabel <*> feature <*> arbitrary)
      ]
        where seqlabel = strict . unSL <$> arbitrary
              seqdata  = strict . unSD <$> arbitrary
              feature  = B.pack <$> listOf1 (elements alpha)
              strict = B.concat . L.toChunks

eventList :: Gen [Event]
eventList = sized $ \s -> frequency [ (100, single)
                                    , (s, (++) <$> single <*> (resize (s `div` 2) eventList)) ]
    where
      single = (\xs -> EvHeader : xs ++ [EvEnd]) <$> listOf arbitrary

instance Arbitrary Stockholm where
    arbitrary = sized $ \s -> resize (min s 15) $
                Stockholm <$> arbitrary <*> arbitrary <*> arbitrary
    shrink (Stockholm fileanns clmnanns stseqs) =
        Stockholm <$> shrink fileanns <*> shrink clmnanns <*> shrink stseqs


instance Arbitrary StockholmSeq where
    arbitrary = StSeq <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink (StSeq label data_ seqanns clmnanns) =
        StSeq label data_ <$> shrink seqanns <*> shrink clmnanns

instance Arbitrary d => Arbitrary (Ann d) where
    arbitrary = Ann <$> arbitrary <*> arbitrary

instance Arbitrary FileAnnotation where
    arbitrary = annArbitraryHelper list F_Other
        where
          list = [ AC, ID, DE, AU, SE, GA, TC, NC, TP, SQ, AM, DC
                 , DR, RC, RN, RM, RT, RA, RL, PI, KW, CC, NE, NL ]

instance Arbitrary (ColumnAnnotation a) where
    arbitrary = annArbitraryHelper list C_Other
        where
          list = [SS, SA, TM, PP, LI, AS, PAS, SAS, IN]

instance Arbitrary SequenceAnnotation where
    arbitrary = annArbitraryHelper list S_Other
        where
          list = [S_AC, S_DE, S_DR, OS, OC, LO]

annArbitraryHelper :: Arbitrary b => [a] -> (b -> a) -> Gen a
annArbitraryHelper list other =
  frequency $ (1, other <$> arbitrary) :
              [(5, pure x) | x <- list]

instance Arbitrary SeqLabel where
    arbitrary = SeqLabel <$> (L.cons <$> elements alpha <*> (L.filter (/= ' ') <$> arbitrary))

instance Arbitrary SeqData where
    arbitrary = SeqData . L.pack <$> listOf1 (elements ['A', 'T', 'C', 'G'])

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> (c3 <$> elements alpha
                               <*> listOf1 (elements $ alpha ++ " .?!|:[]{}")
                               <*> elements alpha)
        where c3 a b c = a : b ++ [c]

instance Arbitrary L.ByteString where
    arbitrary = L.fromChunks <$> arbitrary

alpha :: String
alpha = ['a'..'z'] ++ ['A'..'Z']


----------------------------------------------------------------------

class Ord a => Canonical a where
    canonical :: a -> a
    canonical = id

    canonicalList :: [a] -> [a]
    canonicalList = sort . map canonical

instance Canonical FileAnnotation where
instance Canonical (ColumnAnnotation a) where
instance Canonical SequenceAnnotation where
instance Canonical SeqLabel where
instance Canonical SeqData where
instance Canonical B.ByteString where
instance Canonical L.ByteString where

instance Canonical Event where
    canonicalList = concat . map (glue . sort) . separate
        where
          separate (EvHeader : xs) =
              case break (== EvEnd) xs of
                (before, EvEnd : after) -> (EvHeader : before ++ [EvEnd]) : separate after
                (before, rest)          -> (EvHeader : before)            : separate rest
          separate (x:xs) = [x] : separate xs
          separate []     = []
          (<>) = B.append
          glue (EvSeqData sq1 data1 : EvSeqData sq2 data2 : xs)
              | sq1 == sq2 = glue (EvSeqData sq1 (data1 <> data2) : xs)
          glue (EvGF feat1 data1 : EvGF feat2 data2 : xs)
              | feat1 == feat2 = glue (EvGF feat1 (data1 <> data2) : xs)
          glue (EvGC feat1 data1 : EvGC feat2 data2 : xs)
              | feat1 == feat2 = glue (EvGC feat1 (data1 <> data2) : xs)
          glue (EvGS sq1 feat1 data1 : EvGS sq2 feat2 data2 : xs)
              | sq1 == sq2 && feat1 == feat2 = glue (EvGS sq1 feat1 (data1 <> data2) : xs)
          glue (EvGR sq1 feat1 data1 : EvGR sq2 feat2 data2 : xs)
              | sq1 == sq2 && feat1 == feat2 = glue (EvGR sq1 feat1 (data1 <> data2) : xs)
          glue (x1:x2:xs) = x1 : glue (x2:xs)
          glue rest       = rest


instance Canonical Stockholm where
    canonical (Stockholm fileanns clmnanns stseqs) =
        Stockholm (canonical fileanns) (canonical clmnanns) (canonical stseqs)

instance Canonical StockholmSeq where
    canonical (StSeq label data_ seqanns clmnanns) =
        StSeq (canonical label) (canonical data_) (canonical seqanns) (canonical clmnanns)

instance (Ord a, Canonical a) => Canonical [a] where
    canonical = canonicalList

instance Ord d => Canonical (Ann d) where
    canonicalList = map mk . M.toList . toMap . map unMk
        where
          mk = uncurry Ann
          unMk (Ann f d) = (f, d)
          toMap = M.fromListWith (flip L.append)
