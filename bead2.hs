#!/usr/bin/env runghc
import qualified Data.Text.IO as Text
import Text.Pandoc
import Text.Pandoc.JSON
import Data.Char
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Control.Monad.State
import System.Environment
import Text.Pandoc.Builder
import Data.String

getMeta :: Meta -> Maybe String
getMeta meta = case (lookupMeta "liftheader" meta) of
  Just (MetaString s) -> Just s
  Nothing -> Nothing

retrieveMaybeInt :: Maybe String -> Int
retrieveMaybeInt val = case val of
  Just (val) -> read val
  Nothing -> 0

f :: Pandoc -> Pandoc
f p@(Pandoc m _) = walk (elevateHeader (retrieveMaybeInt liftheaderMeta)) p
  where
    liftheaderMeta = getMeta m

elevateHeader :: Int -> Block -> Block
elevateHeader increment (Header level attribs contents) = Header (level + increment) attribs contents 
elevateHeader increment x = x

main :: IO () 
main = toJSONFilter f
