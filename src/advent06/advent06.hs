    {-# LANGUAGE OverloadedStrings #-}

    import Data.List

    import Data.Text (Text)
    import qualified Data.Text.IO as TIO

    import Data.Void (Void)

    import Text.Megaparsec
    import Text.Megaparsec.Char
    import qualified Text.Megaparsec.Char.Lexer as L
    import qualified Control.Applicative as CA

    import qualified Data.Map.Strict as M

    type Coord = (Integer, Integer) -- x, y
    type Bounds = (Integer, Integer, Integer, Integer) -- minX, maxX, minY, maxY
    type Region = M.Map Coord Int

    main :: IO ()
    main = do 
            text <- TIO.readFile "data/advent06.txt"
            let coords = successfulParse text
            let boundingBox = findBounds coords
            print $ length coords
            print boundingBox
            print $ part1 coords boundingBox
            print $ part2 coords boundingBox


    part1 coords bounds = largestRegion $ regionSizes $ finite edgeLabels regions
        where regions = findRegions coords bounds
              edgeLabels = infinite regions bounds

    part2 coords bounds = M.size $ M.filter (< 10000) $ safeCells coords bounds

    findRegions :: [Coord] -> Bounds -> Region
    findRegions coords (minX, maxX, minY, maxY) = M.fromList labelledCells
        where cells = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY] ]
              starts = zip [1..] coords
              labelledCells = map (\c -> (c, nearestStart 0 c starts)) cells

    nearestStart :: Int -> Coord -> [(Int, Coord)] -> Int
    nearestStart tieLabel cell starts = nearestLabel
        where distances = sort $ map (\(l, s) -> (distance s cell , l)) starts
              nearestLabel = if fst (distances!!0) == fst (distances!!1)
                             then tieLabel
                             else snd (distances!!0)


    safeCells :: [Coord] -> Bounds -> Region
    safeCells coords (minX, maxX, minY, maxY) = M.fromList distanceCells
        where cells = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY] ]
              distanceCells = map (\c -> (c, fromIntegral $ sumDistance c coords) ) cells


    sumDistance :: Coord -> [Coord] -> Integer
    sumDistance here others = sum $ map (\c -> distance here c) others


    infinite :: Region -> Bounds -> [Int]
    infinite regions (minX, maxX, minY, maxY) = nub $ sort $ M.elems $ M.filterWithKey onEdge regions
        where onEdge (x, y) _ = (x == minX) || (x == maxX) || (y == minY) || (y == maxY)

    finite :: [Int] -> Region -> Region
    finite excluded regions = M.filter (\r -> r `notElem` excludedTied) regions
        where excludedTied = (0:excluded)


    regionSizes :: Region -> [(Int, Int)]
    regionSizes regions = map (\g -> (g!!0, length g)) $ group $ sort $ M.elems regions


    largestRegion :: [(Int, Int)] -> Int
    largestRegion = maximum . map snd


    findBounds :: [Coord] -> (Integer, Integer, Integer, Integer)
    findBounds coords = ( minX - (maxY - minY) `div` 2 -- small x edge
                        , maxX + (maxY - minY) `div` 2 -- large x edge
                        , minY - (maxX - minX) `div` 2-- small x edge
                        , maxY + (maxX - minX) `div` 2 -- large y edge
                        )
        where maxX = maximum $ map fst coords
              minX = minimum $ map fst coords
              maxY = maximum $ map snd coords
              minY = minimum $ map snd coords

    -- Manhattan distance
    distance :: Coord -> Coord -> Integer
    distance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))


    -- Parse the input file

    type Parser = Parsec Void Text

    sc :: Parser ()
    sc = L.space (skipSome spaceChar) CA.empty CA.empty
    -- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

    lexeme  = L.lexeme sc
    integer = lexeme L.decimal
    symb = L.symbol sc

    commaP = symb ","


    coordFileP = many coordP
    coordP = (,) <$> integer <* commaP <*> integer

    successfulParse :: Text -> [Coord]
    successfulParse input = 
            case parse coordFileP "input" input of
                    Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                    Right coords -> coords