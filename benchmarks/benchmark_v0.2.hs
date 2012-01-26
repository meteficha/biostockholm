{-# LANGUAGE OverloadedStrings #-}
import Bio.Sequence.Stockholm
import Bio.Sequence.Stockholm.Stream
import Control.Monad.Exception.Synchronous (Exceptional(..))
import System.IO (stdin)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

main = main_doc

main_doc = C.runResourceT $ CB.sourceHandle stdin C.$$ parseStockholm C.=$ CL.mapM_ printName
    where
      printName (Stockholm file _ _) = print (findAnn AC file)

main_event = C.runResourceT $ CB.sourceHandle stdin C.$$ parseEvents C.=$ CL.mapM_ printName
    where
      printName (EvGF "AC" name) = print name
      printName _                = return ()
