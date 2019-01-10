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
--solve :: [[(Char, Bool)]] -> [String] -> [String]
--solve table list = solveHorizontally table list

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
markWord word s n = (DL.take (s-1) word) ++ (markWord (DL.drop (s-1) word) 0 n) 

--przeszukiwanie w poziomie
solveHorizontally :: (PuzzleTable, [String]) -> (PuzzleTable, [String])
-- po przeszukaniu calego stanu
solveHorizontally ([], wordlist) = ([], wordlist)
-- przerwac gdy lista slow jest pusta -zle chyba
--solveHorizontally (row, []) = (row, [])
solveHorizontally ((x:xs), wordlist) = do
										let next = solveHorizontally (xs, wordlist)
										let state = lookForWordsInSingleRow (x, (snd next))
										([(fst state)]++(fst next), (snd state))

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

									

						