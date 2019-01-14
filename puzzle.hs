import Prelude
import System.IO
import System.IO.Error
import Control.Exception

import PuzzleTableType
import PuzzleSolver

--SPOP 18Z Dawid Janik, Wojciech Sobczyk


-- Zbiór mozliwych bledow pliku
fileError :: IOError -> Bool
fileError e = isDoesNotExistError e || isAlreadyInUseError e || isPermissionError e

-- Domyślne ściezki plkiów wejściowych
defaultPath1 = "input_data/2_table.txt"
defaultPath2 = "input_data/2_words.txt"


-- Główna fukcja programu - wczytuje pliki podane przez użytkownika, wywołuje główną fukcję solvera
loadAndSolve tableFiLeName wordlistFileName  = catch
    ( do 
                handle <- openFile tableFiLeName ReadMode
                table <- hGetContents handle
                putStrLn  table
                putStrLn  " "
                hClose handle
                handle <- openFile wordlistFileName ReadMode
                words <- hGetContents handle
                putStrLn words
                hClose handle
                putStrLn (solve table words)
    ) errorHandler
            where
                    errorHandler e =
                        if (fileError e)
                            then putStrLn ("Nie mozna otworzyc pliku ")
                        else return ()
main = puzzle
-- Funkcja wejściowa - pobiera dane wejściowe od użytkownika i wywyłuje główną funckję programu
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

                

                


