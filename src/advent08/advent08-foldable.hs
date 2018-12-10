{-# LANGUAGE OverloadedStrings #-}

-- import Data.List

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA


data Tree a = Node [Tree a] a deriving (Eq, Show)

instance Foldable Tree where
    foldMap f (Node (t: ts) m) = f m <> foldMap (foldMap f) ts


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent08-small.txt"
        let treeSpec = successfulParse text
        let (tree, _) = parseTree treeSpec
        print $ foldMap id tree
        print $ part1 tree
        print $ part2 tree

-- part1 = foldMap sum
part1 = metadataOfTree

part2 = valueOfTree


parseTree (c:m:spec) = (Node children metadata, remainder')
    where (children, remainder) = parseManyTree c spec
          metadata = take m remainder
          remainder' = drop m remainder

parseManyTree n spec 
    | n == 0 = ([], spec)
    | otherwise = ((tree:otherTrees), remainder')
    where (tree, remainder) = parseTree spec
          (otherTrees, remainder') = parseManyTree (n-1) remainder

metadataOfTree (Node trees metadata) = metadata ++ (concatMap metadataOfTree trees)

valueOfTree (Node trees metadata) 
    | null trees = sum metadata
    | otherwise = sum selectedValues
    where childValues = map valueOfTree trees
          selectedValues = map (\v -> childValues!!(v-1)) $ filter (<= (length trees)) metadata


-- Parse the input file

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal

treeFileP = many integer

successfulParse :: Text -> [Int]
successfulParse input = 
        case parse treeFileP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right treeSpec -> treeSpec