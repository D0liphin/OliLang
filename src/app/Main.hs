module Main where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Parse
import Tokenize (TokenizeError (..), tokenize)

main :: IO ()
main = do
  f <- T.readFile "example.ol"
  case tokenize (0, f) of
    Ok toks -> mapM_ print toks
    Err i -> print $ T.unpack (T.take (i + 1) f)