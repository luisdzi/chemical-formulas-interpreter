module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Data.Char
import qualified Data.Map as Map
import Elementos
import Tipos
import ForEval

-- Funcion de ejecucion del parser
parseElem :: String -> Either ParseError Elem
parseElem = parse (totParser elemparse) ""

--Funcion de parseo de formulas de elementos
elemparse :: Parser Elem
elemparse = try (do hidrox <- hidroxido
                    return hidrox)
            <|> try (do oxoac <- oxoacido
                        return oxoac)
            <|> try (do ox <- oxosales
                        return ox)
            <|> try (do hidr <- hidracido
                        return hidr)
            <|> try (do hidrvol <- hidruroVolatil
                        return hidrvol)
            <|> try (do hid <- hidruro
                        return hid)
            <|> try (do saln <- salneutra
                        return saln)
            <|> try (do oxianh <- oxidoOanhidrido
                        return oxianh)
            <|> try (do perox <- peroxido
                        return perox)
            <|> fail ":h para obtener ayuda."
-------------------------------------------------------------------------------
--                            Parser de hidroxidos
-------------------------------------------------------------------------------

hidroxido :: Parser Elem
hidroxido = do m <- metal 
               try (do (o,h) <- oxhidrilo
                       return (Hidroxido m o h 1))
                <|> try (do (o,h) <- parens lis oxhidrilo
                            i <- natural lis
                            return (Hidroxido m o h i))



oxhidrilo :: Parser (Oxigeno, Hidrogeno)
oxhidrilo = do o <- oxigeno
               h <- hidrogeno
               return (o,h)

-------------------------------------------------------------------------------
--                     Parsers de oxoacidos y oxosales
-------------------------------------------------------------------------------

oxoacido:: Parser Elem
oxoacido = do h <- hidrogeno
              try (do i <- natural lis
                      (nm,j,o,k) <- oxosales2
                      return (Oxoacido h i nm j o k))
               <|> try (do (nm,j,o,k) <- oxosales2
                           return (Oxoacido h 1 nm j o k))

-- Parsea el metal que da comienzo a la oxosal y devuelve la misma
oxosales :: Parser Elem
oxosales =  try (do m <- metal
                    (i, nm, j, o, k, t) <- oxosales1
                    return (Oxosal m i nm j o k t))

-- Parsea el subindice del metal que corresponde a la oxosal y el subindice del 
-- ion que acompaña al metal
oxosales1 :: Parser (Integer, NoMetal, Integer, Oxigeno, Integer, Integer)
oxosales1 = try (do i <- natural lis
                    (nm, j, o, k) <- parens lis oxosales2
                    t <- natural lis
                    return (i, nm, j, o, k, t))
            <|> try (do (nm, j, o, k) <- parens lis oxosales2
                        t <- natural lis
                        return (1, nm, j, o, k, t))
            <|> try (do i <- natural lis
                        (nm, j, o, k) <- oxosales2
                        return (i, nm, j, o, k, 1))
            <|> try (do (nm, j, o, k) <- oxosales2
                        return (1, nm, j, o, k, 1))

-- Parsea el nometal, el oxigeno y los subindices de los mismos
oxosales2 :: Parser (NoMetal, Integer, Oxigeno, Integer)
oxosales2 = do nm <- nometal
               try (do j <- natural lis 
                       o <- oxigeno 
                       k <- natural lis
                       return (nm, j, o, k)) 
                <|> try (do j <- natural lis 
                            o <- oxigeno 
                            return (nm, j, o, 1))
                <|> try (do o <- oxigeno 
                            k <- natural lis
                            return (nm, 1, o, k))  
                <|> try (do o <- oxigeno 
                            return (nm, 1, o, 1))

-------------------------------------------------------------------------------
--                         Parser de hidracidos
-------------------------------------------------------------------------------

hidracido:: Parser Elem
hidracido = do h <- hidrogeno
               try (do i <- natural lis
                       nm <- hidra
                       return (Hidracido h i nm))
                <|> (do nm <- hidra
                        return (Hidracido h 1 nm))

hidra :: Parser Metal
hidra = do x <- parseForm
           case Map.member x hidraElems of
            True -> return x
            _    -> fail ":h para obtener ayuda."


-------------------------------------------------------------------------------
--                         Parser de hidruros volatiles
-------------------------------------------------------------------------------


hidruroVolatil:: Parser Elem
hidruroVolatil = do nm <- hidruroVolatil'
                    h <- hidrogeno 
                    i <- natural lis 
                    return (HidruroVolatil nm h i)

hidruroVolatil' :: Parser NoMetal
hidruroVolatil' = do x <- parseForm
                     case Map.member x hidruroVolatilElems of
                       True -> return x
                       _    -> fail ":h para obtener ayuda."

-------------------------------------------------------------------------------
--                            Parser de hidruros
-------------------------------------------------------------------------------

hidruro :: Parser Elem
hidruro = do m <- metal
             try (do h <- hidrogeno
                     i <- natural lis
                     return (Hidruro m h i))
               <|> try (do h <- hidrogeno
                           return (Hidruro m h 1))

-------------------------------------------------------------------------------
--                         Parser de sales neutras
-------------------------------------------------------------------------------

salneutra :: Parser Elem
salneutra = do m <- metal
               try (do i <- natural lis
                       nm <- salNeutra'
                       j <- natural lis
                       return (SalNeutra m i nm j))
                 <|> try (do nm <- salNeutra'
                             j <- natural lis
                             return (SalNeutra m 1 nm j))
                 <|> try (do i <- natural lis 
                             nm <- salNeutra'
                             return (SalNeutra m i nm 1))   
                 <|> try (do nm <- salNeutra'
                             return (SalNeutra m 1 nm 1))

salNeutra' :: Parser NoMetal
salNeutra' = do x <- parseForm
                case Map.member x salNeutraElems of
                  True -> return x
                  _    -> fail ":h para obtener ayuda."


-------------------------------------------------------------------------------
--                         Parser de oxidos metalicos/anhidridos
-------------------------------------------------------------------------------

-- Se agrego la funcion testEo para chequear los estados con los que trabaja
-- el metal, ya que podria parsearse igual que un anhidro y no serlo. Esto es
-- debido a que hay elementos metálicos que presentan caracter no metálico.

oxidoOanhidrido :: Parser Elem
oxidoOanhidrido = do try (do m <- metal
                             i <-natural lis
                             o <- oxigeno
                             j <- natural lis
                             let eos = snd $ elemento m metalElems
                             case testEo (i,j,0) eos (-2,0) of
                               False -> fail ":h para obtener ayuda."
                               _    -> return (OxidoMet m i o j))
                      <|> try (do nm <- nometal
                                  i <-natural lis
                                  o <- oxigeno
                                  j <- natural lis
                                  return (Anhidrido nm i o j))
                      <|> try (do m <- metal
                                  o <- oxigeno
                                  j <- natural lis
                                  let eos = snd $ elemento m metalElems
                                  case testEo (1,j,0) eos (-2,0) of
                                   False -> fail ":h para obtener ayuda."
                                   _    -> return (OxidoMet m 1 o j))
                      <|> try (do nm <- nometal
                                  o <- oxigeno
                                  j <- natural lis
                                  return (Anhidrido nm 1 o j))
                      <|> try (do m <- metal
                                  i <- natural lis
                                  o <- oxigeno
                                  let eos = snd $ elemento m metalElems
                                  case testEo (i,1,0) eos (-2,0) of
                                   False -> fail ":h para obtener ayuda."
                                   _    -> return (OxidoMet m i o 1))
                      <|> try (do nm <- nometal
                                  i <-natural lis
                                  o <- oxigeno
                                  return (Anhidrido nm i o 1))
                      <|> try (do m <- metal
                                  o <- oxigeno
                                  let eos = snd $ elemento m metalElems
                                  case testEo (1,1,0) eos (-2,0) of
                                   False -> fail ":h para obtener ayuda."
                                   _    -> return (OxidoMet m 1 o 1))
                      <|> try (do nm <- nometal
                                  o <- oxigeno
                                  return (Anhidrido nm 1 o 1))

-------------------------------------------------------------------------------
--                            Parser de peroxidos
-------------------------------------------------------------------------------

peroxido :: Parser Elem
peroxido = do m <- metal
              try (do i <-natural lis
                      parens lis (do oxigeno
                                     string "2")
                      j <- natural lis
                      return (Peroxido m i "O" j))
               <|> try (do parens lis (do oxigeno
                                          string "2")
                           j <- natural lis
                           return (Peroxido m 1 "O" j))
               <|> try (do i <-natural lis
                           parens lis (do oxigeno
                                          string "2")
                           return (Peroxido m i "O" 1))
               <|> try (do parens lis (do oxigeno
                                          string "2")
                           return (Peroxido m 1 "O" 1))

-------------------------------------------------------------------------------
--                           Parsers auxiliares
-------------------------------------------------------------------------------

metal :: Parser Metal
metal = do x <- parseForm
           case Map.member x metalElems of
             True -> return x
             _    -> fail ":h para obtener ayuda."

nometal :: Parser NoMetal
nometal = do x <- parseForm
             case Map.member x noMetalElems of
               True -> return x
               _    -> fail ":h para obtener ayuda."

               
oxigeno :: Parser Oxigeno
oxigeno = string "O"
 

hidrogeno :: Parser Hidrogeno
hidrogeno = string "H"

parseForm :: Parser String
parseForm = try (do x <- satisfy isUpper
                    y <- satisfy isLower
                    return [x,y])
            <|> try (do x <- satisfy isUpper
                        return [x])


lis :: TokenParser u
lis = makeTokenParser (emptyDef)


totParser :: Parser a -> Parser a
totParser p = do  whiteSpace lis
                  t <- p
                  whiteSpace lis
                  eof
                  return t
