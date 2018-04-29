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

getIncrement :: Meta -> Int
getIncrement meta = case (lookupMeta "liftheader" meta) of
  Just (MetaString s) -> read s
  Nothing -> 0

getMaxLevel :: Meta -> Int
getMaxLevel meta = case (lookupMeta "maxheader" meta) of
  Just (MetaString s) -> read s
  Nothing -> maxBound :: Int

raiseToMax :: Int -> Int -> Block -> Maybe Block
raiseToMax inc max (Header lvl attribs contents)
  | lvl + inc > max = Nothing
  | otherwise       = Just (Header (lvl + inc) attribs contents)
raiseToMax inc max block = Just block

runMaybeBlock :: Maybe Pandoc -> Pandoc
runMaybeBlock block = case block of
  Just block -> block
  Nothing -> doc(para(fromString "Hiba történt."))

f :: Pandoc -> Pandoc
f p@(Pandoc m _) = runMaybeBlock (walkM (raiseToMax getincmeta getmaxmeta) p)
  where
    getincmeta = getIncrement m
    getmaxmeta = getMaxLevel m

--elevateHeader :: Int -> Block -> Block
--elevateHeader increment (Header level attribs contents) = Header (level + increment) attribs contents 
--elevateHeader increment x = x

main :: IO () 
main = toJSONFilter f
