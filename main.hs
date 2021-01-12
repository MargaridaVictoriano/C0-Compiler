module Main where

import Lexer
import Parser
import Icode

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

main :: IO ()
main = do
     string <- getContents
     print ( evalState (intermediate (parser $ alexScanTokens string) Map.empty) (0,0,0,0))
