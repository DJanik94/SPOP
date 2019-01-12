module PuzzleSolver where

import Data.List as DL
import PuzzleTableType
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.ByteString as BS


--konwersja rezultatu do pojedynczego stringa
solutionToString :: [String] -> String
solutionToString [] = []
solutionToString (x:xs) = x ++ ['\n'] ++ solutionToString xs

--główna funckcja
solve :: String -> String -> String
solve tableString listString = let
                                    table = initializePuzzleTable tableString
                                    wordlist = DL.map clearWord (lines listString)
                                    stage1 = solveHorizontally table wordlist
                                    stage2 = solveVertically stage1 wordlist
                                    stage3 = solveDiagonallyUp stage2 wordlist
                                    stage4 = solveDiagonallyDown stage3 wordlist
                                in puzzleTableToString stage4
                                    --solutionToString (snd stage4)
                        
                        
showRemainigLetters :: PuzzleTable -> [String]
showRemainigLetters [] = []
showRemainigLetters (x:xs) = [showRemainigLettersInRow x] ++ showRemainigLetters xs

showRemainigLettersInRow :: [(Char, Bool)] -> String
showRemainigLettersInRow [] = []
showRemainigLettersInRow (x:xs) = [crossLetter x] ++ showRemainigLettersInRow xs



--zmiana stanu wykreslonej litery
markLetter :: (Char, Bool) -> (Char, Bool)
markLetter (ch, s) = (ch, True)

--oznaczenie znakow w tablicy w przypadku znalezienia słowa argumenty: lista par (znak, stan), indeks poczatkowy, liczba znaków do oznaczenia
markWord :: [(Char, Bool)] -> Int -> Int -> [(Char, Bool)]
markWord word _ 0 = word
markWord (x:xs) 0 n = [markLetter x] ++ markWord xs 0 (n-1)
markWord word s n = (DL.take (s) word) ++(markWord (DL.drop(s) word) 0 n) 

--Wykreślanie w poziomie
solveHorizontally :: PuzzleTable -> [String] -> PuzzleTable
solveHorizontally [] wordList = []
solveHorizontally (x:xs) wordList = let
                                        next = solveHorizontally xs wordList
                                        state = lookForWordsInSingleRow x wordList
                                    in [state]++next

--Wykreślanie w pionie                                  
solveVertically  :: PuzzleTable -> [String] -> PuzzleTable
solveVertically table wordList = let
                                    transTable = DL.transpose table
                                    transResult = solveHorizontally transTable wordList
                                in DL.transpose transResult 

--Wykreślanie po przekątnej w górę
solveDiagonallyUp :: PuzzleTable -> [String] -> PuzzleTable
solveDiagonallyUp table wordList =   let 
                                        rowsNum = DL.length table
                                        columnsNum = DL.length (DL.head table)
                                        diagTable = diagonalsUpper table
                                        result = solveHorizontally diagTable wordList
                                    in restoreFromDiagonals result rowsNum columnsNum

-- Wykreślanie po przekątnej w dół                               
solveDiagonallyDown :: PuzzleTable -> [String] -> PuzzleTable
solveDiagonallyDown table wordList = let 
                                        rowsNum = DL.length table
                                        columnsNum = DL.length (DL.head table)
                                        diagTable = diagonalsUpper (DL.reverse table)
                                        result = solveHorizontally diagTable wordList
                                    in DL.reverse (restoreFromDiagonals result rowsNum columnsNum)
                                        
                                        
-- Funkcja zwracjająca listę przekątnych
diagonalsUpper :: [[a]] -> [[a]]
diagonalsUpper [] = []
diagonalsUpper ([]:xs) = xs
diagonalsUpper xs = DL.zipWith (++) (DL.map ((:[]).DL.head) xs ++ repeat []) ([]:(diagonalsUpper (DL.map DL.tail xs)))


-- Funkcja związana z odtwarzaniem tablicy z przekątnych - odtworzenie pojedynczego wiersza
restoreRow :: [[a]] -> Int -> [a]
restoreRow list 0 = []
restoreRow ([]:xs) n = restoreRow xs n
restoreRow (x:xs) n = [(DL.last x)] ++ restoreRow xs (n-1)

-- Funkcja związana z odtwarzaniem tablicy z przekątnych - usunięcie odtworzonego wiersza
deleteLasts :: [[a]] -> Int -> [[a]]
deleteLasts [] _ = []
deleteLasts list 0 = list
deleteLasts ([]:xs) n =  deleteLasts xs n
deleteLasts (x:xs) n = [(DL.init x)] ++ (deleteLasts xs (n-1))

-- Główna funckcja przywracająca tablicę z listy przekątnych; wejścia: tablica, liczba wierszy, liczba kolumn
restoreFromDiagonals :: [[a]] -> Int -> Int -> [[a]]
restoreFromDiagonals [] k n = []
restoreFromDiagonals (x:xs) 1 n = [restoreRow (x:xs) n]
restoreFromDiagonals (x:xs) k n =   let 
                                        newRow = restoreRow (x:xs) n
                                        (y:ys) = deleteLasts (x:xs) n
                                    in [newRow] ++ restoreFromDiagonals ys (k-1) n
                            

-- Poszukiwanie słowa w danej linii
checkWord :: [(Char, Bool)] -> String -> Maybe Int
checkWord row word = BS.findSubstring (C.pack word) (C.pack (tableRowToString row))

-- Szukanie słowa w pojedynczej linii
lookForSinleWordInSingleRow :: [(Char, Bool)] -> String -> [(Char, Bool)]
lookForSinleWordInSingleRow row [] = row --chyba zbędne
lookForSinleWordInSingleRow row word    | checkWord row word == Nothing = row
                                        | otherwise = markWord row (fromJust (checkWord row word)) (DL.length word)

-- Szukanie słów w pojedynczej linii
lookForWordsInSingleRow :: [(Char, Bool)] -> [String] -> [(Char, Bool)]
lookForWordsInSingleRow row [] = row   
lookForWordsInSingleRow row [x] = lookForSinleWordInSingleRow row x
lookForWordsInSingleRow row (x:xs) = let  
                                        next = lookForWordsInSingleRow row xs 
                                        state = lookForSinleWordInSingleRow next x
                                        in state

                                    

                        
