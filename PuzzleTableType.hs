module PuzzleTableType where

import Data.Char (toLower, toUpper)
import Data.Tuple



{-typ przechowyjacy aktualny stan tablicy: przechowywane w liscie list krotek zawierajacych litery tablicy i binarną informację mówiącą czy dana litera jest juz skeslona (True - skreslona, False - nieskreslona) -}
type PuzzleTable = [[(Char, Bool)]]

--Zastąpienie oznaczonych liter znakami '-', state: True = skreslony
crossLetter :: (Char, Bool) -> Char
crossLetter (letter, state) | state == True = '-'
                            | otherwise = letter

-- Przepisanie tablicy znaków do tablicy par (znak, stan)
initializePuzzleTable :: String -> PuzzleTable
initializePuzzleTable [] = []
initializePuzzleTable table = initializeRows (lines table)

-- Inicjalizacja pojednyczego wiersza tabeli to postaci (znak, stan)
initializeRows :: [String] -> PuzzleTable
initializeRows [] = []
initializeRows (x:xs) = [createRow x] ++ initializeRows xs where
                                                createRow :: [Char] -> [(Char, Bool)]
                                                createRow [] = []
                                                createRow [a] = [( toUpper a, False)]
                                                createRow (x:xs) = [(toUpper x, False)] ++ createRow xs

-- Usuniecie z haseł znaków niebędących literami
clearWord :: String -> String
clearWord [] = []
clearWord (x:xs)    | (x>='A' && x<='Z') || (x>='a' && x<='z') = [(toUpper x)] ++ clearWord xs
                    | otherwise = clearWord xs
               
                   
-- Utworzenie Stringa z pojedynczego wiersza tablicy stanu bez uwzglenienia stanu znaku (skreslony/nieskreslony)
tableRowToString :: [(Char, Bool)] -> String
tableRowToString [] = []
tableRowToString (x:xs) = [fst x] ++ tableRowToString xs

-- Przepisanie stanu do stringa
puzzleTableToString :: PuzzleTable -> String
puzzleTableToString [] = []
puzzleTableToString (x:xs) = (rowToString x) ++['\n'] ++ puzzleTableToString xs
                    where
                        rowToString :: [(Char, Bool)] -> String
                        rowToString [] = []
                        rowToString (y:ys) = (map crossLetter [y]) ++ rowToString ys