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
import Data.List

type ChapterState = State [Int]

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

abcFilter :: Pandoc -> Pandoc
abcFilter p@(Pandoc m _) = runMaybeBlock (walkM (raiseToMax getincmeta getmaxmeta) p)
  where
    getincmeta = getIncrement m
    getmaxmeta = getMaxLevel m

f :: [Int] -> Int -> [Int]
f oldState currentLevel
  | (length oldState) < currentLevel = oldState ++ [1]
  | (length oldState) > currentLevel = (take (currentLevel-1) oldState) ++ [last (take currentLevel oldState) + 1]
  | otherwise                        = (take currentLevel oldState) ++ [(last oldState + 1)]

writeState :: [Int] -> String
writeState state = (intercalate "." $ map show state) ++ "."

enumerateChapter :: Block -> [Int] -> (Block, [Int])
enumerateChapter (Header lvl attribs contents) state 
  = ((Header lvl attribs (toList (str (writeState(f state lvl))) ++ contents)), f state lvl)
enumerateChapter b state = (b, state)

enumApply :: Block -> State [Int] Block
enumApply b = state $ enumerateChapter b

enumFilter :: Pandoc -> Pandoc
enumFilter p = evalState (walkM enumApply p) [0]



--elevateHeader :: Int -> Block -> Block
--elevateHeader increment (Header level attribs contents) = Header (level + increment) attribs contents 
--elevateHeader increment x = x

main :: IO () 
main = toJSONFilter enumFilter
