{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.List                      ( isSuffixOf )
import           Format                         ( format )
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
      it "Can throw a parse error" $ do
        format False "*inline*" "/*" `shouldThrow` errorCall "*inline*:1: parse error"

describeGoldenTests
  :: FilePath -> (FilePath -> BS.ByteString -> IO BL.ByteString) -> IO Spec
describeGoldenTests dir f = do
  files <- filter (".input.nix" `isSuffixOf`) <$> listDirectory dir
  pure $ do
    it "are found" $ do
      length files >= 1 `shouldBe` True
    forM_ files $ \file -> do
      let basename   = take (length file - 10) file
          outputFile = basename <> ".output.nix"
      it basename $ do
        input    <- BS.readFile (dir <> file)
        output   <- f (dir <> file) input
        expected <- BL.readFile (dir <> outputFile)
        output `shouldBe` expected
        -- test for idempotency
        output2  <- f (dir <> file) (BL.toStrict output)
        output2 `shouldBe` output
