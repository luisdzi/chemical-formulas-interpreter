module Test where

import Test.HUnit
import Data.List.Split
import System.Directory
import System.IO
import System.Environment (getArgs)

import EvalStock
import EvalSist
import EvalTrad
import Tipos
import Parser

-------------------------------------------------------------------------------
--                         Modulo encargado del testeo
--------------------------------------------------------------------------------

-- Si le paso -d, le doy como argumento el nombre de la carpeta donde se encuentran los 
--   archivos a testear sino, le doy los nombres de los archivos que me interesan 
--    testear. La carpeta anteriormente mencionada se tiene que encontrar en la 
--                          carpeta del proyecto final

main :: IO () 
main = do (x:ys) <- getArgs
          case x of 
            "-d" -> do runDir (head ys) 
                       putStrLn "------------------------------------------------"
            _    -> do runFiles (x:ys) 
                       putStrLn "------------------------------------------------"

-- runDir se fija que el directorio fp exista, si es asi, deja los archivos que 
-- va a leer y le agrega el nombre de la carpeta a la cual pertenecen como prefijo.
-- Esto es necesario para que el programa sepa a que carpeta pertenecen. Luego, llama
-- a la funcion runFiles.

runDir :: FilePath -> IO ()      
runDir fp = do b <- doesDirectoryExist fp 
               case b of 
                 True -> do ss <- getDirectoryContents fp
                            let ss' = filter (\s-> if s == "." || s == ".." then False else True) ss
                                x = map (\s -> fp ++ "/" ++ s ) ss'
                            runFiles x

                 _ -> error ("Directorio " ++ fp ++ " no encontrado")

-- La funcion runFiles imprime de una forma "bonita", el resultado de cada uno de los tests
-- y llama a la funcion run File.

runFiles :: [String] -> IO()
runFiles [] = return ()
runFiles (f:ff) = do putStrLn "------------------------------------------------"
                     putStrLn f
                     runFile f
                     runFiles ff 

-- runFile, abre el archivo a leer, llama a la funcion runTestFile con el handler 
-- del archivo que queremos leer, corre los tests dentro del archivo y cierra
-- el handler.

runFile :: String -> IO ()
runFile f = do h <- openFile f ReadMode
               tests <- runTestFile h
               runTestTT tests
               hClose h

-- runTestFile llama a la funcion runTestFile' y deja a su resultado como una 
-- TestList preparada para que la funcion runTestTT pueda correr los tests
-- asociados en la funcion runFile.

runTestFile :: Handle -> IO Test
runTestFile h = do tlist <- runTestFile' h
                   return (TestList tlist)

-- runTestFile' chequea que se haya llegado al fin del archivo, en caso de que no 
-- lee una linea, separa lo que lee en una lista de 4 elementos, y luego 
-- realiza el TestCase, donde compara que el nombre dado sea igual que al que da
-- como resultado evalTest, segun el evaluador que le estemos pasando como parametro.
-- Luego le da una etiqueta y vuelve a llamar a runTestFile'.
-- Devuelvo una lista de Tests.

runTestFile' :: Handle -> IO [Test]
runTestFile' h = do t <- hIsEOF h 
                    if t then return [] else 
                      do l <- hGetLine h
                         let [form, nom, par, evalFunc] = splitOn "." l
                             tcase = (TestCase (assertEqual form nom (evalTest form par evalFunc))) 
                             tlabel = TestLabel form tcase
                         tlist <- runTestFile' h
                         return (tlabel: tlist)

-- Evalua la formula s segun el evaluador que elija

evalTest :: String -> String -> String -> String
evalTest s p "evalStock" = runOutSnd $ evalStock (evalParseTest s) (read p) 
evalTest s p "evalSist" = runOutSnd $ evalSist (evalParseTest s) (read p) 
evalTest s _ "evalTrad" = runOutSnd $ evalTrad (evalParseTest s) 

-- Permite obtener la traza que voy llevando en la monada Output

runOutSnd :: Output String b -> String
runOutSnd o = snd $ runOut o

-- Parsea la formula s

evalParseTest :: String -> Elem
evalParseTest s = case parseElem s of
                    Right v -> v
                    _       -> error "Elemento mal parseado en evalParseTest"


