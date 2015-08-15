module Main (
    main
) where

import Data.List (stripPrefix)

-- | Simple function to create a hello message.
-- prop> stripPrefix "Hello " (hello s) == Just s
hello s = "Hello " ++ s

main = putStrLn (hello "World")

