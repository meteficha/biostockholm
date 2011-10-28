-- from hspec
import Test.Hspec.Monadic (hspecX)

-- from this package
import Bio.Sequence.Stockholm (test_Stockholm)

main :: IO ()
main = hspecX test_Stockholm
