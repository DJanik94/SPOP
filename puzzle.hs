import Prelude
import System.IO
import System.IO.Error
import Control.Exception

import PuzzleTableType


		
--zbior mozliwych bledow pliku
fileError :: IOError -> Bool
fileError e = isDoesNotExistError e || isAlreadyInUseError e || isPermissionError e || isEOFError e

--load_word_list2 :: String -> IO ()

-- to poniżej działa, ale od tego parsowania to kurwicy można dostać
defaultPath1 = "input_data/1_table.txt"
defaultPath2 = "input_data/1_words.txt"
--to też do smieci

-- tu się zaczyna zabawa
loadAndProceed tableFiLeName wordlistFileName  = catch
	( do 
				handle <- openFile tableFiLeName ReadMode
				table <- hGetContents handle
				putStrLn  table
				putStrLn  " "
				let puzzleTable = initializePuzzleTable table
				hClose handle
				handle <- openFile wordlistFileName ReadMode
				words <- hGetContents handle
				putStrLn words
				let wordlist = lines words
				hClose handle
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
  putStrLn "Wpisz Nazwe pliku z tablica, \"d\" aby wczytac domyslna konfiugracja lub \"q\" aby wyjsc:"
  line1 <- getLine
  case line1 of
    ['q'] -> return ()
    ['d']  -> do
				putStrLn "dd"  
				loadAndProceed defaultPath1 defaultPath2
				puzzle
    path1 -> do
				putStrLn "Wpisz Nazwe pliku z lista slow"  
				path2 <- getLine
				loadAndProceed path1 path2
				puzzle
