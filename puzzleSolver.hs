module PuzzleSolver where

import Data.List as DL
import PuzzleTableType
--import qualified Data.ByteString.Char8 as C
import Data.Maybe
--import Data.ByteString as BS


--konwersja rezultatu do pojedynczego stringa
solutionToString :: [String] -> String
solutionToString [] = []
solutionToString (x:xs) = x ++ ['\n'] ++ solutionToString xs

--główna funckcja
solve :: String -> String -> String
solve tableString listString = let
                                    table = initializePuzzleTable tableString
                                    wordlist = sortByLength (DL.map clearWord (lines listString))
                                    stage1 = solveHorizontally (table, wordlist)
                                    stage2 = solveVertically stage1 
                                    stage3 = solveDiagonallyUp stage2
                                    stage4 = solveDiagonallyDown stage3 
                                in puzzleTableToString (fst stage4)
                                --in solutionToString (snd stage4)
                        
                        
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
markWord word s n = (DL.take (s) word) ++  (markWord (DL.drop (s) word) 0 n) 

--Wykreślanie w poziomie
solveHorizontally :: (PuzzleTable, [String]) -> (PuzzleTable, [String])
solveHorizontally ([], wordlist) = ([], wordlist)
solveHorizontally ((x:xs), wordlist) = let
                                        next = solveHorizontally (xs, wordlist)
                                        state = lookForWordsInSingleRow (x, (snd next))
                                    in ([(fst state)]++(fst next), (snd state))

--Wykreślanie w pionie                                  
solveVertically  :: (PuzzleTable, [String]) -> (PuzzleTable, [String])
solveVertically (table, list) = let
                                    transTable = DL.transpose table
                                    transResult = solveHorizontally (transTable, list)
                                in ((DL.transpose (fst transResult)), (snd transResult)) 

--Wykreślanie po przekątnej w górę
solveDiagonallyUp :: (PuzzleTable, [String]) -> (PuzzleTable, [String])
solveDiagonallyUp (table, list) =   let 
                                        rowsNum = DL.length table
                                        columnsNum = DL.length (DL.head table)
                                        diagTable = diagonalsUpper table
                                        result = solveHorizontally (diagTable, list)
                                    in ((restoreFromDiagonals (fst result) rowsNum columnsNum), (snd result))

-- Wykreślanie po przekątnej w dół                               
solveDiagonallyDown :: (PuzzleTable, [String]) -> (PuzzleTable, [String])
solveDiagonallyDown (table, list) = let 
                                        rowsNum = DL.length table
                                        columnsNum = DL.length (DL.head table)
                                        diagTable = diagonalsUpper (DL.reverse table)
                                        result = solveHorizontally (diagTable, list)
                                    in ((DL.reverse (restoreFromDiagonals (fst result) rowsNum columnsNum)), (snd result))
                                        
                                        
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
                            


-- Szukanie słowa w pojedynczej linii
lookForSinleWordInSingleRow :: [(Char, Bool)] -> String -> ([(Char, Bool)], [String])
lookForSinleWordInSingleRow row [] = (row, []) --chyba zbędne
lookForSinleWordInSingleRow row word    | checkWord' row word 0 == -1 = (row, [word])
										| otherwise = ((markWord row {-(fromJust-} (checkWord' row word 0){-)-} (DL.length word)), [])

-- Szukanie słów w pojedynczej linii
lookForWordsInSingleRow :: ([(Char, Bool)], [String]) -> ([(Char, Bool)], [String])
lookForWordsInSingleRow (row, []) = (row, [])   
lookForWordsInSingleRow (row, [x]) =    let 
                                            state = lookForSinleWordInSingleRow row x
                                        in ((fst state), (snd state))  --to i tak wywola sie dwa razy (chyba)
lookForWordsInSingleRow (row, (x:xs)) = let  
                                            next = lookForWordsInSingleRow (row, xs) 
                                            state = lookForSinleWordInSingleRow (fst next) x
                                        in ((fst state), (snd next)++(snd state))  --to i tak wywola sie dwa razy (chyba)

                                    


equalStrings :: [(Char, Bool)] -> String -> Bool
equalStrings [] _ = False
equalStrings _ [] = False
equalStrings [(ch, s)] [w] = (ch == w)
equalStrings ((ch, s) : ls) (w:ws) = (ch == w) && equalStrings ls ws 

alreadyCrossed :: [(Char, Bool)] -> Bool
alreadyCrossed [] = False
alreadyCrossed [(ch, s)] = s
alreadyCrossed ((ch, s) : ls) = s  && alreadyCrossed ls             

-- Poszukiwanie słowa w danej linii
checkWord' :: [(Char, Bool)] -> String -> Int -> Int
checkWord' [] _  _= -1
checkWord' (l:ls) word n	| DL.length word > DL.length (l:ls) = -1
							| (equalStrings (DL.take (DL.length word) (l:ls)) word) && not(alreadyCrossed (DL.take (DL.length word) (l:ls))) == True = n
							| otherwise = checkWord' ls word (n+1)
							
							
						
-- Sortowanie lsity słow względem długości
sortByLength :: [[a]] -> [[a]]
sortByLength [] = []
sortByLength (x:xs) = sortByLength (DL.filter (\xs-> DL.length xs < DL.length x ) xs) ++ [x] ++ sortByLength (DL.filter (\xs-> DL.length xs >= DL.length x ) xs)


