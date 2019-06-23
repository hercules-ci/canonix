module Main (main) where

import           Control.Monad
import           Control.Concurrent.Async   (forConcurrently_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Function ((&))
import           Options.Applicative
import           System.IO (hPutStrLn, stderr)

import           Format

main :: IO ()
main = join $ execParser opts

opts :: ParserInfo (IO ())
opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Formats Nix code"
     <> header "canonix - Nix formatter" )

parser :: Parser (IO ())
parser =
  (&) <$> optFlags
      <*>
  (runPipe <$ flag' () (long "pipe" <> help "Read from stdin, write to stdout")
  <|> runInPlace <$> many (argument str (metavar "NIX_FILE")) -- TODO completion
  )
  where
    -- TODO: --version
    optFlags = switch (long "debug" <> short 'd' <> help "Debug mode")

runPipe :: Bool -> IO ()
runPipe debug = do
  BL.putStr =<< format debug =<< BS.getContents

runInPlace :: [FilePath] -> Bool -> IO ()
runInPlace paths debug = do
  when (null paths) $ do
    hPutStrLn stderr "No file arguments provided. Did you mean --pipe?"
  forConcurrently_ paths $ \path ->
    BL.writeFile path =<< format debug =<< BS.readFile path
