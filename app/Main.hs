module Main (main) where

import PremiumizeCli.CLI (parseOptions, runCommand)

main :: IO ()
main = do
  (opts, cmd) <- parseOptions
  runCommand opts cmd
