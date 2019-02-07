module Main where

import System.Console.Readline
import Data.List.Split
import Text.Parsec.Error

import Eval
import Parser
import Tipos
---------------------------------------------------------

main :: IO()
main = do putStrLn "Interprete de formulas quimicas inorganicas:"
          readprinteval

readprinteval :: IO()
readprinteval = do maybeline <- readline ("λ>")
                   case maybeline of
                    Nothing -> do putStrLn ""; putStrLn "Saliendo del intérprete...";return ()
                    Just ":q" -> do putStrLn "Saliendo del intérprete..."; return ()
                    Just line -> do let sp = splitOn " " line
                                    addHistory line
                                    case sp of
                                     (":f" : ":eo" : args)  -> runFile (head args) evalEo
                                     (":eo": ":f" : args) -> runFile (head args) evalEo
                                     (":f" : args)  -> runFile (head args) eval 
                                     (":eo":args) -> run (head args) evalEo
                                     (":h":_)  -> printHelp 
                                     [arg]     -> run arg eval
                                     _         -> do putStrLn "Argumentos desconocidos"
                                                     putStrLn "Ejecute :h para imprimir la ayuda"
                                    readprinteval

-- Toma una formula y devuelve su nombre segun el evaluador evalFunc 
run :: String -> (Elem -> String) -> IO ()
run s evalFunc = do case parseElem s of
                      Right v -> putStrLn $ evalFunc v
                      Left e ->  print e

-- Permite correr en nomenclator desde una archivo 
runFile :: String -> (Elem -> String) -> IO ()
runFile s evalFunc = do s' <- readFile s
                        let l = lines s'
                            p = map (parseElem) l
                            e = map evalFunc (rightParse p)
                            t = zip l e
                        printName t
               

--Imprime la formula y sus nombres
printName :: [(String,String)] -> IO ()
printName [] = return ()
printName ((x,y):xs) = do putStrLn x
                          putStrLn y
                          printName xs

-- Devuelve una lista con los elementos que se parsearon correctamente
rightParse :: [Either ParseError Elem] -> [Elem]
rightParse [] = []
rightParse (x:xs) = case x of 
                    Right e -> e : rightParse xs
                    _       -> rightParse xs  

-- Imprime la ayuda para el intérprete
printHelp :: IO ()
printHelp = do putStrLn "\n  Ayuda para el intérprete de formulas quimicas:\n"
               putStrLn "\t:f [file]: Permite leer formulas desde el archivo \"file\". \n"
               putStrLn "\t:eo [form]: Devuelve, ádemas de los nombres de \"form\",\n\t\t    el estado de oxidacion con el que trabajan sus elementos.\n"
               putStrLn "\t[form]: Lee la formula \"form\" y devuelve sus nombres.\n"
               putStrLn "\t:q : Salir del intérprete."

