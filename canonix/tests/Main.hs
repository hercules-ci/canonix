module Main
  ( main
  )
where

import           Control.Monad                  ( forM_ )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.List                      ( isSuffixOf )
import           Format                         ( format )
import           Paths_canonix                  ( getDataFileName )
import           System.Directory               ( listDirectory )
import           Test.Hspec


main :: IO ()
main = do
  let debug = False
  t <- describeGoldenTests "tests/golden/" (format debug)
  hspec $ do
    describe "formatter" $ do
      describe "golden tests" $ do
        t

describeGoldenTests
  :: FilePath -> (BS.ByteString -> IO BL.ByteString) -> IO Spec
describeGoldenTests reldir f = do
  dir   <- liftIO $ getDataFileName reldir
  files <- filter (".input.nix" `isSuffixOf`) <$> liftIO (listDirectory dir)
  pure $ do
    it "are found" $ do
      length files >= 1 `shouldBe` True
    forM_ files $ \file -> do
      let basename   = take (length file - 10) file
          outputFile = basename <> ".output.nix"
      it basename $ do
        input    <- BS.readFile (dir <> file)
        output   <- f input
        expected <- BL.readFile (dir <> outputFile)
        output `shouldBe` expected
