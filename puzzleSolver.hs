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
--solve :: [[(Char, Bool)]] -> [String] -> String
solve :: String -> String -> String
solve tableString listString = do 
									let	table = initializePuzzleTable tableString
									let wordlist = DL.map clearWord (lines listString)
									let stage1 = solveHorizontally (table, wordlist)
									let stage2 = solveVertically stage1
									let stage3 = solveDiagonallyUp stage2
									let stage4 = solveDiagonallyDown stage3
									puzzleTableToString (fst stage4)
									--solutionToString (snd stage2)
						
						
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
markWord word _ 0 = word --zwrocic uwage na to czy wykreslane zostaja wsztskie znaki, ktore maja byc wykreslone
-- markWord [] _ _ - tu chyba mozna rzucic jakis wyjatek
markWord (x:xs) 0 n = [markLetter x] ++ markWord xs 0 (n-1)
markWord word s n = (DL.take (s) word) ++  (markWord (DL.drop (s) word) 0 n) 

--Wykreślanie w poziomie
solveHorizontally :: (PuzzleTable, [String]) -> (PuzzleTable, [String])
-- po przeszukaniu calego stanu
solveHorizontally ([], wordlist) = ([], wordlist)
solveHorizontally ((x:xs), wordlist) = do
										let next = solveHorizontally (xs, wordlist)
										let state = lookForWordsInSingleRow (x, (snd next))
										([(fst state)]++(fst next), (snd state))

--Wykreślanie w pionie									
solveVertically  :: (PuzzleTable, [String]) -> (PuzzleTable, [String])
solveVertically (table, list) = do 
									let transTable = DL.transpose table
									let transResult = solveHorizontally (transTable, list)
									((DL.transpose (fst transResult)), (snd transResult)) 

--Wykreślanie po przekątnej w górę
solveDiagonallyUp :: (PuzzleTable, [String]) -> (PuzzleTable, [String])
solveDiagonallyUp (table, list) =	let 
										rowsNum = DL.length table
										columnsNum = DL.length (DL.head table)
										diagTable = diagonalsUpper table
										result = solveHorizontally (diagTable, list)
									in ((restoreFromDiagonals (fst result) rowsNum columnsNum), (snd result))

--									
solveDiagonallyDown :: (PuzzleTable, [String]) -> (PuzzleTable, [String])
solveDiagonallyDown (table, list) =	let 
										rowsNum = DL.length table
										columnsNum = DL.length (DL.head table)
										diagTable = diagonalsUpper (DL.reverse table)
										result = solveHorizontally (diagTable, list)
									in ((DL.reverse (restoreFromDiagonals (fst result) rowsNum columnsNum)), (snd result))
										
										
-- trzeba to zrefaktoryzowac
--diagonalsUpper :: [a] -> [a]
diagonalsUpper [] = []
diagonalsUpper ([]:xs) = xs
diagonalsUpper xs = DL.zipWith (++) (DL.map ((:[]).DL.head) xs ++ repeat []) ([]:(diagonalsUpper (DL.map DL.tail xs)))


-- funkcja związana z odtwarzaniem tablicy z przekątnych - odtworzenie pojedynczego wiersza
restoreRow :: [[a]] -> Int -> [a]
restoreRow list 0 = []
restoreRow ([]:xs) n = restoreRow xs n
--restoreRow ([x]:xs) n = [x] ++ restoreRow xs (n-1)
restoreRow (x:xs) n = [(DL.last x)] ++ restoreRow xs (n-1)

-- funkcja związana z odtwarzaniem tablicy z przekątnych - usunięcie odtworzonego wiersza
deleteLasts :: [[a]] -> Int -> [[a]]
deleteLasts [] _ = []
deleteLasts list 0 = list
deleteLasts ([]:xs) n =  deleteLasts xs n
deleteLasts (x:xs) n = [(DL.init x)] ++ (deleteLasts xs (n-1))

--wejścia: tablica, liczba wierszy, liczba kolumn
restoreFromDiagonals :: [[a]] -> Int -> Int -> [[a]]
restoreFromDiagonals [] k n = []
restoreFromDiagonals (x:xs) 1 n = [restoreRow (x:xs) n]
restoreFromDiagonals (x:xs) k n =	let 
								newRow = restoreRow (x:xs) n
								(y:ys) = deleteLasts (x:xs) n
							in [newRow] ++ restoreFromDiagonals ys (k-1) n
							


{-
--może być niepotrzebne, bo jest potężny findSubString
checkWord :: [(Char, Bool)] -> String -> Bool
checkWord list word = isInfixOf word (tableRowToString list)-}

--nawa xfunc do zmiany, ale nie mam juz pomysłow
xfunc :: [(Char, Bool)] -> String -> Maybe Int
xfunc row word = BS.findSubstring (C.pack word) (C.pack (tableRowToString row))

--szukanie słow w pojedynczej linii
lookForWords :: ([(Char, Bool)], [String]) -> ([(Char, Bool)], [String])
lookForWords (row, []) = (row, []) --chyba zbędne
--nawa xfunc do zmiany, ale nie mam juz pomysłow
lookForWords (row, [x])	| xfunc row x == Nothing = (row, [x])
						| otherwise = ((markWord row (fromJust (xfunc row x)) (DL.length x)), [])
						
lookForSinleWordInSingleRow :: [(Char, Bool)] -> String -> ([(Char, Bool)], [String])
lookForSinleWordInSingleRow row [] = (row, []) --chyba zbędne
--nawa xfunc do zmiany, ale nie mam juz pomysłow
lookForSinleWordInSingleRow row word	| xfunc row word == Nothing = (row, [word])
							| otherwise = ((markWord row (fromJust (xfunc row word)) (DL.length word)), [])


lookForWordsInSingleRow :: ([(Char, Bool)], [String]) -> ([(Char, Bool)], [String])
lookForWordsInSingleRow (row, []) = (row, [])	
lookForWordsInSingleRow (row, [x]) = do	
								let state = lookForSinleWordInSingleRow row x
								((fst state), (snd state))	--to i tak wywola sie dwa razy (chyba)
lookForWordsInSingleRow (row, (x:xs)) = do	
								let next = lookForWordsInSingleRow (row, xs) 
								let state = lookForSinleWordInSingleRow (fst next) x
								((fst state), (snd next)++(snd state))	--to i tak wywola sie dwa razy (chyba)

									

						
