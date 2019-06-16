import           Test.Hspec
import           Paths_canonix (getDataFileName)
import           Format (format)
import           System.Directory (listDirectory)
import           Control.Monad (forM_)
import           Data.List (isSuffixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL


main :: IO ()
main = hspec $ do
  describe "formatter" $ do
    it "passes all golden tests" $ do
      runGoldenTests "tests/golden/" (format False)


runGoldenTests :: FilePath -> (BS.ByteString -> IO BL.ByteString) -> Expectation
runGoldenTests reldir f = do
  dir <- getDataFileName reldir
  files <- filter (".input.nix" `isSuffixOf`) <$> listDirectory dir
  forM_ files $ \file -> do
    input <- BS.readFile (dir <> file)
    output <- f input
    expected <- BL.readFile (dir <> outputFile file)
    output `shouldBe` expected
  where
    outputFile path =
      take (length path - 10) path <> ".output.nix"
