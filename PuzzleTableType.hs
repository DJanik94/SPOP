
module PuzzleTableType where
data PuzzleTable = PuzzleTable [[(Char, Bool)]] deriving (Show, Read)


initializePuzzleTable :: String -> [[(Char, Bool)]]
--chcialem z map, ale nie dziala
initializePuzzleTable [] = []
initializePuzzleTable table = initializeRows (lines table)

--inicjalizacja pojednyczego wiersza tabeli to postaci 
initializeRows :: [String] -> [[(Char, Bool)]]
initializeRows [] = []
initializeRows (x:xs) = [createRow x] ++ initializeRows xs

createRow :: [Char] -> [(Char, Bool)]
createRow [] = []
createRow [a] = [(a, False)]
createRow (x:xs) = [(x, False)] ++ createRow xs

