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
import Control.Monad.Trans.Except

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


-- D feladat
stepNumbering :: [Int] -> Int -> [Int]
stepNumbering oldState currentLevel
  | (length oldState) < currentLevel = oldState ++ (replicate (currentLevel - length(oldState)-1) 0) ++ [1]
  | (length oldState) > currentLevel = (take (currentLevel-1) oldState) ++ [(oldState !! (currentLevel-1)) + 1]
  | otherwise                        = (take (currentLevel-1) oldState) ++ [(last oldState + 1)]

writeState :: [Int] -> String
writeState state = (intercalate "." $ map show state) ++ ". "

enumerateChapter' :: Block -> [Int] -> Block
enumerateChapter' (Header lvl attribs contents) numbering
  = (Header lvl attribs (toList (str (writeState numbering)) ++ contents))
enumerateChapter' b state = b

enumApply :: Block -> State [Int] Block
enumApply b@(Header lvl attribs contents) = do
  prevNumbering <- get
  put $ stepNumbering prevNumbering lvl
  newNumbering <- get
  pure $ enumerateChapter' b newNumbering
enumApply b = pure b

enumFilter :: Pandoc -> Pandoc
enumFilter p = evalState (walkM enumApply p) [0]

--- E feladat
fromMeta :: MetaValue -> String
fromMeta (MetaString s) = s

getMaxNum :: Meta -> Int -> Int
getMaxNum m lvl = 
  let x = read <$> fromMeta <$> lookupMeta ("maxnum_" ++ show lvl) m
    in maybe maxBound id x

fromNumberingToString :: [Int] -> String
fromNumberingToString [] = ""
fromNumberingToString (x:xs) = (show x ++ ".") ++ (fromNumberingToString xs)

checkMetaMaxNum :: Meta -> Block -> ExceptT [String] (State [Int]) Block
checkMetaMaxNum m b@(Header lvl attribs contents) = do
  prevNumbering <- lift get
  let newNumbering = stepNumbering prevNumbering lvl
  let maxNumbering = getMaxNum m lvl
  lift $ put newNumbering
  if (last newNumbering) > maxNumbering 
    then throwE ([show lvl] ++ [fromNumberingToString newNumbering])
    else pure $ enumerateChapter' b newNumbering
checkMetaMaxNum m x = pure x

eFilter :: Pandoc -> Pandoc
eFilter p@(Pandoc m _) = case (evalState (runExceptT (walkM (checkMetaMaxNum m) p)) []) of
  (Left int) -> doc(para(fromString $ "Túl sok " ++ (show (head int)) ++ ". szintű fejezet: " ++ show (last int)))
  (Right pd) -> pd

main :: IO () 
main = toJSONFilter eFilter