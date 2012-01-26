import Bio.Sequence.Stockholm
import Control.Monad.Exception.Synchronous (Exceptional(..))
import qualified Data.ByteString.Lazy as L

main = L.getContents >>= mapM_ printName . parseStockholm
    where
      printName (Success (Stockholm file _ _)) = print (findAnn AC file)
      printName (Exception ())                 = putStr "---------------------"
