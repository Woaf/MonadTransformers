import qualified Data.Text.IO as Text
import Text.Pandoc
import Data.Char
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Control.Monad.State
import System.Environment

newId :: State Int Int
newId = state $ \s -> (s+1, s+1)

number_line :: String -> State Int String
number_line s = do
  i <- newId
  return $ "#" ++ (show i)

--changetox :: Pandoc -> Pandoc
--changetox (Str s) = return $ (foldr (++) "x")

count' :: Block -> State (Int, Int) Block
count' (CodeBlock attrib lines') = do
  (codeblocks, letters) <- get
  put (codeblocks + 1, codeblocks+1)
  return $ (CodeBlock attrib lines')
count' any = return any

count_letters_and_words :: Pandoc -> (Int, Int)
count_letters_and_words parsed = execState (walkM count' parsed) (0, 0)

enumerate_codelines :: Block -> State (Int,Int) Block
enumerate_codelines (CodeBlock attribs lines') = do
  (numberat, nothing) <- get
  let numbered = head (evalState (mapM number_line (lines lines')) nothing)
  put (numberat -1, numberat -1)
  return $ CodeBlock attribs numbered
enumerate_codelines any = return any

modify_codeblocks :: Pandoc -> (Pandoc, (Int, Int))
modify_codeblocks parsed = do
  let (k,p) = count_letters_and_words parsed 
  runState (walkM enumerate_codelines parsed) (k, p)

main :: IO ()
main = do
  argsList <- getArgs
  read_md <- Text.readFile $ head argsList
  parsed <- runIOorExplode $ readMarkdown (def {readerExtensions = pandocExtensions}) read_md

  let (numbered, (k, p))  = modify_codeblocks parsed

  write_md  <- runIOorExplode $ writeMarkdown (def {writerExtensions = pandocExtensions}) numbered
  Text.writeFile (head (tail argsList)) $ write_md
