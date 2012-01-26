{-# LANGUAGE OverloadedStrings #-}

-- from base
import Control.Applicative
import System.IO.Unsafe (unsafePerformIO)

-- from bytestring
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- from biocore
import Bio.Core.Sequence

-- from conduit
import qualified Data.Conduit as C
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

    describe "parseStockholm/renderStockholm" $ do
      it "parses rendered test file 1" $ do
        rendered <- strictRender result
        again    <- strictParse  rendered
        again @?= result
      it "parses rendered test file 2" $ do
        rendered <- strictRender result2
        again    <- strictParse  rendered
        again @?= result2
      prop "passes QuickCheck property" $ \sto ->
        unsafePerformIO $ do
          rendered <- strictRender sto
          again    <- strictParse  rendered
          return (again == sto)


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

strictParse :: L.ByteString -> IO [Stockholm]
strictParse lbs = C.runResourceT $
                    CL.sourceList (L.toChunks lbs) C.$=
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
      [ (1,  pure EvHeader)
      , (1,  pure EvEnd)
      , (3,  EvComment <$> arbitrary)
      , (10, EvSeqData <$> arbitrary <*> arbitrary)
      , (2,  EvGF      <$> arbitrary <*> arbitrary)
      , (2,  EvGC      <$> arbitrary <*> arbitrary)
      , (2,  EvGS      <$> arbitrary <*> arbitrary <*> arbitrary)
      , (2,  EvGR      <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance Arbitrary Stockholm where
    arbitrary = Stockholm <$> arbitrary <*> arbitrary <*> arbitrary
    shrink (Stockholm fileanns clmnanns stseqs) =
        Stockholm <$> shrink fileanns <*> shrink clmnanns <*> shrink stseqs


instance Arbitrary StockholmSeq where
    arbitrary = StSeq <$> (SeqLabel <$> arbitrary)
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
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

instance Arbitrary SeqData where
    arbitrary = SeqData . L.pack <$> listOf1 (elements ['A', 'T', 'C', 'G'])

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " .?!|:[]{}")

instance Arbitrary L.ByteString where
    arbitrary = L.fromChunks <$> arbitrary
