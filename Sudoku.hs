module Sudoku where

import Test.QuickCheck
import Data.Char(intToDigit, digitToInt)
import Data.List(nub, transpose)
------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

sudokuSize = 9

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate sudokuSize (replicate sudokuSize Nothing)


-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku sud) = ((length sud) == sudokuSize) && all ((== sudokuSize).length) sud && all(all isValidCell)sud
  where 
    isValidCell :: Cell -> Bool
    isValidCell Nothing = True
    isValidCell (Just n) = n <= sudokuSize && n >= 0
  


-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku sud)=  all(all (/=Nothing))sud

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku sud) = do putStrLn $ unlines $ map (map showCell) sud
  where
    showCell :: Cell -> Char
    showCell Nothing = '.'
    showCell (Just n) = intToDigit n 

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = 
  do 
    d <- readFile path 
    let sud = Sudoku (map (map (readCell)) $ lines d)
    if not (isSudoku sud) 
      then error "Aids"
      else return sud
      where 
        readCell :: Char -> Cell
        readCell '.' = Nothing
        readCell n = Just (digitToInt n)

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(9, elements [Nothing]), (1, elements [(Just x) | x  <- [1..sudokuSize]])]


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
    let Sudoku sud = allBlankSudoku
    row <- vectorOf 9 cell
    return $ Sudoku (map (\r -> row) sud)


 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s= isSudoku s
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock b = let l = [n |(Just n) <- (filter (/= Nothing) b)] in (length l == length (nub l))


-- * D2

blocks :: Sudoku -> [Block]
blocks (Sudoku sud) =  sud ++ transpose sud ++ buildBlocks sud
    where 
      buildBlocks :: [[Cell]] -> [[Cell]]
      buildBlocks [] = []
      buildBlocks l = takeBlocks (transpose (take 3 l)) ++ buildBlocks (drop 3 l)

      takeBlocks :: [[Cell]] -> [[Cell]]
      takeBlocks [] = []
      takeBlocks l = concat (take 3 l) : takeBlocks (drop 3 l)
      


prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths (Sudoku sud) = length (blocks (Sudoku sud)) == 3*sudokuSize  && all (\b -> length b ==sudokuSize) sud

-- * D3

isOkay :: Sudoku -> Bool
isOkay s = all (isOkayBlock) (blocks s)


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
