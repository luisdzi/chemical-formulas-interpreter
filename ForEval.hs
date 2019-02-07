module ForEval where  

import Data.Maybe
import Data.List
import qualified Data.Map as Map

import Tipos
import Elementos
------------------------------------------------------------------------------
--         Funciones necesarias para realizar los evaluadores 
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--                             Sufijos y prefijos
------------------------------------------------------------------------------

-- sufix deja el nombre del compuesto preparado para agregar un sufijo
sufix :: Nombre -> Nombre
sufix "nitrogeno" = "nitr"
sufix "oro"    = "aur"
sufix "hierro" = "ferr"
sufix "plata"  = "argent"
sufix "azufre" = "sulfur"
sufix "plomo"  = "plumb"
sufix "manganeso" = "mangan"
sufix "selenio"   = "selen"
sufix "arsenico"  = "arseni"
sufix "antimonio" = "antimoni"
sufix "silicio"   = "silici"
sufix "cerio"     = "ceri"
sufix "rodio"     = "rodi"
sufix "cobre"     = "cupr"
sufix "titanio"   = "titani"
sufix "vanadio"   = "vanadi"
sufix "uranio"    = "urani"
sufix "mercurio"  = "mercuri"
sufix s = sufixaux s

-- Consume todas las vocales que estan al final de un nombre

sufixaux:: Nombre -> Nombre
sufixaux xs = if isVowel (last xs) then sufixaux (init xs) else xs

-- sufixAux maneja la nomenclatura sistematica del selenio, del
-- azufre y el fosforo en los hidracidos

sufixAux :: String -> String
sufixAux "selenio" = "seleni"
sufixAux "azufre" = "sulf"
sufixAux "fosforo" = "fosf"
sufixAux s = (sufix s)

-- prefixEsp permite realizar la nomenclatura tradicional de los acidos especiales
-- del fosforo, arsenico y antimonio
prefixEsp :: Integer -> Nombre -> String -> String
prefixEsp i nm s | nm == "P" || nm == "As" || nm == "Sb" || nm == "B" = prefixEsp' i s
                 | True = s

prefixEsp' :: Integer -> String -> String
prefixEsp' 1 s =  s ++ "meta" -- Una molecula de agua
prefixEsp' 4 s =  s ++ "piro" -- Dos moleculas de agua
prefixEsp' 3 s =  s ++ "orto" -- Tres moleculas de agua 

--Devuelve el nombre del elemento con el prefijo y sufijo correspendiente segun su EO
sufixPrefix :: Nombre ->[EstOx] -> EstOx -> Nombre
sufixPrefix s eos eo = icoOso s (queEo eos eo) (contEo eos)

--icoOso s indice cantEo 
icoOso :: Nombre -> Integer -> Integer -> Nombre    
icoOso s 0 1 = doubleVowel s "ico"
icoOso s 0 2 = doubleVowel s "oso"
icoOso s 1 2 = icoOso s 0 1 -- ico
icoOso s 0 3 = doubleVowel "hipo" (icoOso s 0 2) -- hipo--oso
icoOso s 1 3 = icoOso s 0 2 -- oso 
icoOso s 2 3 = icoOso s 0 1 -- ico
icoOso s 0 4 = icoOso s 0 3 -- hipo -- oso 
icoOso s 1 4 = icoOso s 0 2 -- oso
icoOso s 2 4 = icoOso s 0 1 -- ico
icoOso s 3 4 = doubleVowel "per" (icoOso s 0 1) -- per -- ico
icoOso s 0 5 = icoOso s 0 3 -- hipo -- oso
icoOso s 1 5 = icoOso s 0 2 -- oso
icoOso s 2 5 = icoOso s 0 1 -- ico
icoOso s 3 5 = icoOso s 3 4 -- per -- ico
icoOso s 4 5 = doubleVowel "hi" (icoOso s 3 4) -- hiper -- ico

--Deja el nombre s listo con los pref-suf para los oxoácidos y oxosales
sufixPrefixOxo :: Nombre -> [EstOx] -> EstOx -> Nombre
sufixPrefixOxo s eos eo = atoIto s (queEo eos eo) (contEo eos)

-- atoIto nombre indice cantEo
atoIto :: Nombre -> Integer -> Integer -> Nombre    
atoIto s 0 1 = doubleVowel s "ato"
atoIto s 0 2 = doubleVowel s "ito"
atoIto s 1 2 = atoIto s 0 1 -- ico
atoIto s 0 3 = doubleVowel "hipo" (atoIto s 0 2) -- hipo--oso
atoIto s 1 3 = atoIto s 0 2 -- oso 
atoIto s 2 3 = atoIto s 0 1 -- ico
atoIto s 0 4 = atoIto s 0 3 -- hipo -- oso 
atoIto s 1 4 = atoIto s 0 2 -- oso
atoIto s 2 4 = atoIto s 0 1 -- ico
atoIto s 3 4 = doubleVowel "per" (atoIto s 0 1) -- per -- ico
atoIto s 0 5 = atoIto s 0 3 -- hipo -- oso
atoIto s 1 5 = atoIto s 0 2 -- oso
atoIto s 2 5 = atoIto s 0 1 -- ico
atoIto s 3 5 = atoIto s 3 4 -- per -- ico
atoIto s 4 5 = doubleVowel "hi" (atoIto s 3 4) -- hiper -- ico


-- Devuelve el prefijo segun el subindice
cantSub :: Integer -> String
cantSub 1 = "mono"
cantSub 2 = "di"
cantSub 3 = "tri"
cantSub 4 = "tetra"
cantSub 5 = "penta"
cantSub 6 = "hexa"
cantSub 7 = "hepta"

-- Devuelve el prefijo segun el subindice, pero si es 1 devuelvo la cadena sin caracteres
cantSub' :: Integer -> String
cantSub' 1 = ""
cantSub' n = cantSub n

-- Se fija si la ultima letra de s1 es igual a la primera de s2, si son iguales
-- deja una y une las cadenas, sino une las cadenas directamente.
-- Sirve para evitar redundiancias, como por ejemplo "monooxido".
doubleVowel :: String -> String -> String
doubleVowel [] s2 = s2
doubleVowel s1 [] = s1
doubleVowel s1 s2 = if last s1 == head s2 then s1 ++ (tail s2) else s1 ++ s2   

urode :: Nombre -> Nombre
urode s = s ++ "uro de "

uro :: Nombre -> Nombre
uro s = s ++ "uro "

ato :: Nombre -> Nombre
ato s = s ++ "ato"

-----------------------------------------------------------------------------
--                  Funciones para los estados de oxidación
------------------------------------------------------------------------------

-- posibleEo comprueba que el compuesto este bien formado y devuelve el estado 
-- de oxidacion con el que esta trabajando el elemento.
-- 
posibleEo :: ThreeSubIndex -> [EstOx] -> (EstOx,EstOx) -> Maybe EstOx
posibleEo _ [] _  = Nothing
posibleEo (i,j,k) (v1:xs) (v2,v3) = if i*v1 + j*v2 + k*v3 == 0 then Just v1 else posibleEo (i,j,k) xs (v2,v3) 

--probarEo prueba con que estados de oxidación estan trabajando el metal y el no metal de las sales neutras hasta
--encontrar cuales son los eos correctos.

probarEo :: (Integer,Integer) -> [EstOx] -> [EstOx] -> Maybe (EstOx, EstOx)
probarEo p ys zs = let xss = [(a,b) | a <- ys, b <- zs]
                           in probarEo' p xss

probarEo' :: (Integer,Integer) -> [(EstOx,EstOx)] -> Maybe (EstOx,EstOx)
probarEo' _ [] = Nothing
probarEo' (i,j) ((m,nm):xss) = if m*i + nm*j == 0 then Just (m,nm) else probarEo' (i,j) xss

--Devuelve la cantidad de EO con las que puede trabajar el elemento.
contEo :: [EstOx] -> Integer
contEo xs = fromIntegral $ length xs

--Devuelve el indice en que se encuentra el EO con el que el elemento trabaja.
queEo :: [EstOx] -> EstOx -> Integer
queEo xs x = case elemIndex x xs of
              Just v -> fromIntegral v
              Nothing -> error "Index not found"


-- Devuelve el nombre del elemento y sus estados de oxidacion posible.
elemento :: Nombre -> Compuestos -> (Nombre, [EstOx])
elemento e dict = case Map.lookup e dict of
                        Just v ->  v
                        Nothing -> error "Elemento no encontrado" 

--Convierte un numero entero (EO en este caso) en su equivalente en numeros romanos como cadena.
numRom :: Integer -> String
numRom i | i == 1 = "(I)"
         | i == 2 = "(II)"
         | i == 3 = "(III)"
         | i == 4 = "(IV)"
         | i == 5 = "(V)"
         | i == 6 = "(VI)"
         | i == 7 = "(VII)"
         | i == 8 = "(VIII)"

prefGriegos :: Integer -> String
prefGriegos i | i == 2 = "bis-"
              | i == 3 = "tris-"
              | i == 4 = "tetrakis-"
              | i == 5 = "pentakis-"
              | i == 6 = "hexakis-"
              | i == 7 = "heptakis-"
              | i == 8 = "octakis-"
              | otherwise = ""  

-------------------------------------------------------------------------------
--                            Funciones auxiliares
-------------------------------------------------------------------------------
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "No es Right"

oneElem :: [a] -> Bool
oneElem [_] = True
oneElem _   = False

isVowel :: Char -> Bool
isVowel c | c =='a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = True
          | otherwise = False
             
--Devuelve el nombre de los compuestos que tienen un elemento con eo variable
nombreUnComp :: Elem -> Nombre
nombreUnComp (OxidoMet m _ _ _)      = nombre m metalElems
nombreUnComp (Hidruro m _ _)         = nombre m metalElems
nombreUnComp (Hidroxido m _ _ _)     = nombre m metalElems
nombreUnComp (Peroxido m _ _ _)      = nombre m metalElems
nombreUnComp (Anhidrido nm _ _ _)    = nombre nm noMetalElems
nombreUnComp (Hidracido _ _ nm)      = nombre nm noMetalElems
nombreUnComp (Oxoacido _ _ nm _ _ _) = nombre nm noMetalElems                             
nombreUnComp (HidruroVolatil nm _ _) = nombre nm noMetalElems

--Devuelve los nombres de los compuestos que tienen dos elementos con eo variable
nombreDosComp :: Elem -> (Nombre,Nombre)
nombreDosComp (SalNeutra m _ nm _)    = (nombre m metalElems,nombre nm noMetalElems)  
nombreDosComp (Oxosal m _ nm _ _ _ _) = (nombre m noMetalElems,nombre nm noMetalElems)

-- Busca en el diccionario el nombre de un compuesto
nombre :: Nombre -> Compuestos -> Nombre
nombre n dict = fst (fromJust $ Map.lookup n dict)

testEo :: ThreeSubIndex -> [EstOx] -> (EstOx,EstOx) -> Bool
testEo _ [] _  = False
testEo (i,j,k) (v1:xs) (v2,v3) = if i*v1 + j*v2 + k*v3 == 0 then True else testEo (i,j,k) xs (v2,v3) 

oxidoDe :: String
oxidoDe = "oxido de "

peroxidoDe :: String
peroxidoDe = "peroxido de "

hidruroDe :: String
hidruroDe = "hidruro de "

hidroxidoDe :: String
hidroxidoDe = "hidroxido de "

deHidrogeno :: String
deHidrogeno = " de hidrogeno"

oxidoSolo :: String
oxidoSolo = "oxido "

peroxidoSolo :: String
peroxidoSolo = "peroxido "

hidruroSolo :: String
hidruroSolo = "hidruro "

hidroxidoSolo :: String
hidroxidoSolo = "hidroxido "

anhidridoSolo :: String
anhidridoSolo = "anhidrido "

hidrico :: String
hidrico = "hidrico"

acid :: String
acid = "acido "

hidrogen :: String
hidrogen = "hidrogeno"

oxo :: String
oxo = "oxo"

ico :: String 
ico = "ico"

de :: String
de = " de "

nothing :: String
nothing = ""

space :: String
space = " "

