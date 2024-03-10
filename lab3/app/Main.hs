module Main (main) where

import Lib
import CmdParams

main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO()
work params = do
    