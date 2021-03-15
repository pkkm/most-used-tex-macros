#!/usr/bin/env stack
{- stack runhaskell --resolver lts-11.2 --install-ghc
     --package filemanip --package counter
     --package attoparsec --package formatting
     --package filepath
     -- -Wall -Wextra -Wno-unused-do-bind -}
-- Using `runhaskell` instead of the newer `script --optimize` because we need
-- the `counter` package which is not in the lts-11.2 snapshot.

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (foldM)
import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import qualified Data.Attoparsec.Text as P
import qualified System.IO as IO
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text (Text)
import System.FilePath ((</>))
import System.FilePath.Find ((==?), (&&?), (||?))
import qualified System.FilePath.Find as Find
import qualified Data.Counter as Counter
import qualified Formatting.Formatters as Format
import Formatting (format, (%))

-- * Parsing TeX files.

-- In TeX, a macro can have 3 forms: an active character, a control word ("\"
-- followed by a string of letters) and a control symbol ("\" followed by a
-- non-letter). Moreover, the category of a character (e.g. whether it's a
-- letter or not) can change during document execution.

-- Since the purpose of this analysis is to find commonly typed long macros,
-- we'll only parse control words. To simplify further, we'll use a static
-- definition of a letter instead of executing the input documents.

-- For more information about macros in TeX, see:
--   - https://tex.stackexchange.com/questions/422966/which-characters-are-technically-legal-in-macro-names-with-t1
--   - https://en.wikibooks.org/wiki/TeX/catcode
--   - https://tex.stackexchange.com/questions/8351/what-do-makeatletter-and-makeatother-do

-- If we need more advanced parsing later, there are several LaTeX libraries for
-- Haskell, e.g.:
--   - https://github.com/Daniel-Diaz/HaTeX
--   - https://github.com/synsem/texhs

data Macro = Macro Text deriving (Eq, Show)

parseMacroName :: P.Parser Text
parseMacroName = P.takeWhile1 $ P.inClass "a-zA-Z@"

parseEnv :: P.Parser Macro
parseEnv = do
  P.string "\\begin{"
  name <- parseMacroName
  P.char '}'
  return $ Macro $ "begin{" <> name <> "}"

parseMacro :: P.Parser Macro
parseMacro = do
  P.char '\\'
  name <- parseMacroName
  if name == "begin" || name == "end"
    then fail "\\begin and \\end are handled by another parser"
    else return $ Macro name

-- Get all matches of a parser (like in a regular expression library).
allMatches :: P.Parser a -> P.Parser [a]
allMatches parser = P.many' loop
  where loop = parser <|> (P.anyChar >> loop)

parseTexFile :: P.Parser [Macro]
parseTexFile = allMatches $ parseEnv <|> parseMacro

-- * Macro statistics data structure.

type MacroStats = Counter.Counter Text Integer

initStats :: MacroStats
initStats = Counter.empty

updateStats :: MacroStats -> Macro -> MacroStats
updateStats stats (Macro name) = Counter.update name stats

updateStatsFromText :: MacroStats -> Text -> MacroStats
updateStatsFromText stats text =
  case P.parseOnly parseTexFile text of
    Left err -> error $ "allMatches didn't match (should never happen): " <> err
    Right results -> foldl' updateStats stats results

-- * I/O.

printStats :: MacroStats -> IO ()
printStats stats = do
  --putStrLn "Most popular macros:"
  mapM_ printLine tuples
    where tuples = sortBy (flip $ comparing snd) $ Map.toList stats
          printLine (macro, count) =
            TLIO.putStrLn $
              format (Format.left 8 ' ' % "  " % Format.stext) count macro

-- Like TIO.readFile, but uses UTF-8 instead of the system locale and ignores
-- wrong byte sequences instead of erroring out. We can use this for TeX files
-- because they're UTF-8 or ASCII, but to make this general, we'd need to detect
-- the encoding (e.g. using <https://stackoverflow.com/a/21966822>).
readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path =
  IO.withFile path IO.ReadMode $ \handle -> do
    encoding <- IO.mkTextEncoding "UTF-8//IGNORE"
    IO.hSetEncoding handle encoding
    TIO.hGetContents handle

updateStatsFromFile :: MacroStats -> FilePath -> IO MacroStats
updateStatsFromFile stats path = do
  contents <- readFileUtf8 path
  return $ updateStatsFromText stats contents

texFiles :: IO [FilePath]
texFiles = Find.find (return True) isTexFile $ "repos" </> "ready"
  where isTexFile = Find.fileType ==? Find.RegularFile &&?
          (Find.extension ==? ".tex" ||? Find.extension ==? ".TEX")

finalStats :: IO MacroStats
finalStats = texFiles >>= foldM updateStatsFromFile initStats

main :: IO ()
main = finalStats >>= printStats
