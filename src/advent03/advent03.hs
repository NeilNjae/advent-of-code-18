    {-# LANGUAGE OverloadedStrings #-}

    import Data.List
    import Data.Tuple (swap)

    import Data.Text (Text)
    import qualified Data.Text.IO as TIO

    import Data.Void (Void)

    import Text.Megaparsec
    import Text.Megaparsec.Char
    import qualified Text.Megaparsec.Char.Lexer as L
    import qualified Control.Applicative as CA

    import qualified Data.Map.Strict as M
    -- import Data.Map.Strict ((!))


    type Position = (Int, Int) -- row, column
    data Claim = Claim { claimId :: Int, claimTopLeft :: Position, claimWidth :: Int, claimHeight :: Int } deriving (Show, Eq)
    type Fabric = M.Map Position Int

    main :: IO ()
    main = do 
            text <- TIO.readFile "data/advent03.txt"
            let claims = successfulParse text
            let fabric = foldl' addClaim M.empty claims
            print $ part1 fabric
            print $ part2 fabric claims


    part1 :: Fabric -> Int
    part1  = M.size . overclaimed 

    part2 :: Fabric -> [Claim] -> Int
    part2 fabric claims = claimId $ head $ filter noOverlap' claims
        where noOverlap' claim = noOverlap fabric claim


    claimedSquares :: Claim -> [Position]
    claimedSquares claim = [(r, c) | r <- [r0 .. (r0 + h - 1)]
                                   , c <- [c0 .. (c0 + w - 1)]
                           ]
        where (r0, c0) = claimTopLeft claim
              h = claimHeight claim
              w = claimWidth claim

    addClaim :: Fabric -> Claim -> Fabric
    addClaim fabric claim = foldl' addSquare fabric squares 
        where squares = claimedSquares claim

    addSquare :: Fabric -> Position -> Fabric
    addSquare fabric square = M.insert square (currentClaims + 1) fabric
        where currentClaims = M.findWithDefault 0 square fabric

    overclaimed :: Fabric -> Fabric
    overclaimed = M.filter ( > 1)

    noOverlap fabric claim = M.null $ overclaimed existing
        where claimedFabric = addClaim M.empty claim
              existing = fabric `M.intersection` claimedFabric

    -- Parse the input file

    type Parser = Parsec Void Text

    sc :: Parser ()
    sc = L.space (skipSome spaceChar) CA.empty CA.empty
    -- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

    lexeme  = L.lexeme sc
    integer = lexeme L.decimal
    symb = L.symbol sc

    hashP = symb "#"
    atP = symb "@"
    colonP = symb ":"
    commaP = symb ","
    exP = "x"

    claimsFileP = many claimP

    idP = hashP *> integer
    leftTopP = (,) <$> integer <* commaP <*> integer
    widthHeightP = (,) <$> integer <* exP <*> integer

    claimP = claimify <$> idP <* atP <*> leftTopP <* colonP <*> widthHeightP
        where claimify cid cr (w, h) = Claim { claimId = cid, claimTopLeft = swap cr, claimWidth = w, claimHeight = h }

    successfulParse :: Text -> [Claim]
    successfulParse input = 
            case parse claimsFileP "input" input of
                    Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                    Right claims -> claims