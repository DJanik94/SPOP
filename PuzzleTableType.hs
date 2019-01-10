module PuzzleTableType where

import Data.Char (toLower, toUpper)
import Data.Tuple



{-typ przechowyjacy aktualny stan tablicy: przechowywane w liscie list krotek zawierajacych litery tablicy i binarną informację mówiącą czy dana litera jest juz skeslona (True - skreslona, False - nieskreslona) -}
type PuzzleTable = [[(Char, Bool)]]

--Zastąpienie oznaczonych liter znakami '-'
crossLetter :: (Char, Bool) -> Char
--state: True = skreslony
crossLetter (letter, state)	| state == True = '-'
							| otherwise = letter

--przepisanie tablicy znaków do tablicy par (znak, stan)
initializePuzzleTable :: String -> [[(Char, Bool)]]
initializePuzzleTable [] = []
initializePuzzleTable table = initializeRows (lines table)

--inicjalizacja pojednyczego wiersza tabeli to postaci (znak, stan)
initializeRows :: [String] -> [[(Char, Bool)]]
initializeRows [] = []
initializeRows (x:xs) = [createRow x] ++ initializeRows xs

createRow :: [Char] -> [(Char, Bool)]
createRow [] = []
createRow [a] = [( toUpper a, False)]
createRow (x:xs) = [(toUpper x, False)] ++ createRow xs

--usuniecie z hasel znaków niebędących literami
clearWord :: String -> String
clearWord [] = []
clearWord (x:xs)  | (x>='A' && x<='Z') || (x>='a' && x<='z') = [(toUpper x)] ++ clearWord xs
				   | otherwise = clearWord xs
			   
				   
--utworzenie Stringa z pojedynczego wiersza tablicy stanu bez uwzglenienia stanu znaku (skreslony/nieskreslony)
tableRowToString :: [(Char, Bool)] -> String
tableRowToString [] = []
tableRowToString (x:xs) = [fst x] ++ tableRowToString xs

--przepisanie stanu do stringa /TODO: zapewnic zgodnosc z glowna funkcja
puzzleTableToString :: PuzzleTable -> String
puzzleTableToString [] = []
puzzleTableToString (x:xs) = (rowToString x) ++['\n'] ++ puzzleTableToString xs
					where
						rowToString :: [(Char, Bool)] -> String
						rowToString [] = []
						rowToString (y:ys) = (map crossLetter x) ++ rowToString ys