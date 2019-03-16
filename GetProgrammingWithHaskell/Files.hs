{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.Environment
import System.IO

main :: IO ()
main = do
  helloFile <- openFile "../txts/hello.txt" ReadMode
  firstLine <- hGetLine helloFile
  putStrLn firstLine
  secondLine  <- hGetLine helloFile
  goodbyeFile <- openFile "../txts/goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine
  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"

eofMain :: IO ()
eofMain = do
  helloFile     <- openFile "../txts/hello.txt" ReadMode
  hasLine       <- hIsEOF helloFile
  firstLine     <- if not hasLine then hGetLine helloFile else return "empty"
  hasSecondLine <- hIsEOF helloFile
  secondLine    <- if not hasSecondLine then hGetLine helloFile else return ""
  hClose helloFile
  putStrLn "done!"

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
 where
  charCount = length input
  wordCount = (length . words) input
  lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) =
  unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc]

{-
Because hGetContents is lazy, the value stored in input isn't used until it's needed
hClose executes immediately
summary is lazy
appendFile is strict
-}
countMain :: IO ()
countMain = do
  args <- getArgs
  let fileName = head args
  file  <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = countsText . getCounts $ input
  putStrLn summary
  hClose file
  appendFile "../txts/stats.dat" $ mconcat [fileName, " ", summary, "\n"]

strictGetCounts :: Text.Text -> (Int, Int, Int)
strictGetCounts input =
  (Text.length input, length . Text.words $ input, length . Text.lines $ input)

strictCountsText :: (Int, Int, Int) -> Text.Text
strictCountsText (cc, wc, lc) = Text.pack
  $ unwords ["chars: ", scc, " words: ", swc, " lines: ", slc]
  where [scc, swc, slc] = map show [cc, wc, lc]

strictCountMain :: IO ()
strictCountMain = do
  args <- getArgs
  let fileName = head args
  input <- Text.IO.readFile fileName
  let summary = strictCountsText . strictGetCounts $ input
  Text.IO.appendFile "../txts/stats.dat"
    $ mconcat [Text.pack fileName, " ", summary, "\n"]
  Text.IO.putStrLn summary

{-
use lazy evaluation when reading a single file or doing little I/O
use strict evaluation when it's moderately complex, involving reading and writing files, or
  operations for which order is important
-}

-- Q24.1 cp, copy a file and allow you to rename it

cp :: IO ()
cp = do
  args <- getArgs
  let source = args !! 0
  let dest   = args !! 1
  input <- Text.IO.readFile source
  Text.IO.writeFile dest input

 -- Q24.2 capitalize, read a file, rewrite it to be capitalized
capitalize :: IO ()
capitalize = do
  args <- getArgs
  let fileName = head args
  input <- Text.IO.readFile fileName
  Text.IO.writeFile fileName (Text.toUpper input)
