{-# LANGUAGE OverloadedStrings #-}

-- from bytestring
import qualified Data.ByteString.Lazy.Char8 as L

-- from biocore
import Bio.Core.Sequence

-- from conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

-- from hspec
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit

-- from this package
import Bio.Sequence.Stockholm


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
