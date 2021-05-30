{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.String.Interpolate (i)
import           System.Environment      (getArgs)
import qualified Web.Client              as C
import qualified Web.Server              as S

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--server":_) -> S.run
    ("--client":_) -> C.run
    ("--help":_) -> help
    _   -> S.run

help :: IO ()
help = putStrLn [i|
Usage (app arguments):
  -server [default] - to start the HTTP server
  -client           - to start the HTTP client
|]
