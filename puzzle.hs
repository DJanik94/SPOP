import Prelude
import System.IO
import System.IO.Error
import Control.Exception


import PuzzleTableType
import PuzzleSolver

{- TODO:
-przeszukiwanie slow
	- pierwszy krok: szukanie w poziomie
- rozwazyc wywolanie inicjalizacji tablicy stanu do solvera zeby nie importowac 
- sprobowac wywalic zbedne funckje, ktore mozna zastapic map
- posortowac slowa od nadluzszego do najkrotszego aby zapobiec sytuacji znalezienia slowa bedacego częścią innego słowa z listy w miejscu gdzie powinno zostac wykreslone to dluższe (np. BROOM i BROOMSTICK) - albo odwrtotnie, w zależności od kolejności pobierania elemntów z listy słów
- w wyjsciu dodac liste nieznalezionych słów
- rozważyć stosowanie wyłącznie ByteStringów
- naprawić bug: zawsze szuka tylko pierwszego wystapinia np. dla sampleTable = initializeRows ["ABCABC", "DEFGHXY", "IJKLMN"]
sampleRow = sampleTable !! 0
sampleWordList = ["C", "ABCA","XY"] skersli tylko pierwsze wystapienia poszczegolnych słów
- soznaczana jest o jedna litera za wczesnie
-}
--zbior mozliwych bledow pliku
fileError :: IOError -> Bool
fileError e = isDoesNotExistError e || isAlreadyInUseError e || isPermissionError e || isEOFError e

--load_word_list2 :: String -> IO ()

-- to poniżej działa, ale od tego parsowania to kurwicy można dostać
defaultPath1 = "input_data/1_table.txt"
defaultPath2 = "input_data/1_words.txt"
--to też do smieci

-- tu się zaczyna zabawa
loadAndSolve tableFiLeName wordlistFileName  = catch
	( do 
				handle <- openFile tableFiLeName ReadMode
				table <- hGetContents handle
				putStrLn  table
				putStrLn  " "
				--let puzzleTable = initializePuzzleTable table
				hClose handle
				handle <- openFile wordlistFileName ReadMode
				words <- hGetContents handle
				putStrLn words
				--let wordlist = lines words
				--let clearWordlist = map clearWord wordlist
				hClose handle
				putStrLn (solve table words)
	) errorHandler
			where
				    errorHandler e =
						if (fileError e)
							then putStrLn ("Nie mozna otworzyc pliku ")
						else return ()
main = puzzle
-- głowna funkcja
puzzle :: IO()
puzzle = do
  putStrLn "Podaj sicezke pliku z tablica, \"d\" aby wczytac domyslna konfiugracja lub \"q\" aby wyjsc:"
  line1 <- getLine
  case line1 of
    ['q'] -> return ()
    ['d']  -> do
				putStrLn "d"  
				loadAndSolve defaultPath1 defaultPath2
				puzzle
    path1 -> do
				putStrLn "Podaj sciezke pliku z lista slow"  
				path2 <- getLine
				loadAndSolve path1 path2
				puzzle

				
-- TESTY
sampleTable = initializeRows ["ABCABC", "DEFGHXY", "IJKLMN"]
sampleRow = sampleTable !! 0
sampleWordList = ["C","XY", "EGFH", "TY", "IJK", "A", "BXM"]
resultTest = lookForWordsInSingleRow (sampleRow, sampleWordList)
r11 = solveHorizontally (sampleTable, sampleWordList)
r10 = putStrLn ("ABCABC"++['\n']++"DEFGHXY"++['\n']++"IJKLMN")
r12 = putStrLn (puzzleTableToString (fst r11))