module Main (main) where

import           Control.Monad
import           Control.Concurrent.Async   (forConcurrently_)
import qualified Data.ByteString as BS
import           Options.Applicative
import           System.IO (hPutStrLn, stderr)

import           Format

main :: IO ()
main = join $ execParser opts

opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Formats Nix code"
     <> header "canonix - Nix formatter" )

parser :: Parser (IO ())
parser =
  runPipe <$ flag' () (long "pipe" <> help "Read from stdin, write to stdout")
  <|>
  runInPlace <$> many (argument str (metavar "NIX_FILE")) -- TODO completion

runPipe :: IO ()
runPipe = do
  BS.putStr =<< format =<< BS.getContents

runInPlace :: [FilePath] -> IO ()
runInPlace paths = do
  when (null paths) $ do
    hPutStrLn stderr "No file arguments provided. Did you mean --pipe?"
  forConcurrently_ paths $ \path ->
    BS.writeFile path =<< format =<< BS.readFile path
