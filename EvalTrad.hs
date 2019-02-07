module EvalTrad where 

import ForEval
import Tipos
import Elementos
------------------------------------------------------------------------------
-- Evaluador encargado de obtener el nombre segun la nomenclatura tradicional
------------------------------------------------------------------------------

evalTrad :: Elem -> Output String (Either String (EstOx,EstOx))
evalTrad (OxidoMet m i _ j)      = evalTradAux m oxidoSolo (i,j,0) (-2,0) metalElems
evalTrad (Hidruro m _ j)         = evalTradAux m hidruroSolo (1,j,0) (-1,0) metalElems
evalTrad (Hidroxido m _ _ j)     = evalTradAux m hidroxidoSolo (1, j,0) (-1,0) metalElems
evalTrad (Anhidrido nm i _ j)    = evalTradAux nm anhidridoSolo (i,j,0) (-2,0) noMetalElems
evalTrad (Oxoacido _ j nm i _ k) = evalTradAux nm (prefixEsp j nm acid) (i,j,k) (1,-2) noMetalElems
evalTrad (Peroxido m i _ j)      = evalTradAux m peroxidoSolo (i,j,0) (-2,0) metalElems
evalTrad (Hidracido _ j nm)      = evalTradHidra nm acid (1,j,0) (-1,0) hidraElems
evalTrad s@(SalNeutra _ _ _ _)   = evalTradSalNeutra s
evalTrad (HidruroVolatil nm _ _) = do write nm'
                                      return (Right (eonm !! 0,0))
                                     where (nm', eonm) = elemento nm hidruroVolatilElems                                         
evalTrad (Oxosal m i nm j _ k t) = do case oxoacido (i,j,k,t) eosm eosnm (-2) of 
                                        Just p@(eom,eonm) -> do write (sufixPrefixOxo (sufixAux nm') eosnm eonm)
                                                                write space
                                                                write (sufixPrefix (sufix m') eosm eom) 
                                                                return (Right p)
                                        _ -> throw
                                      where (m',eosm) = elemento m metalElems
                                            (nm',eosnm) = elemento nm noMetalElems


-------------------------------------------------------------------------------
-- El objetivo de los evaluadores auxiliares, que se encuentran debajo, es 
--      intentar que la evaluacion tradicional sea mas comprensible 
-------------------------------------------------------------------------------

-- evalTradAux toma como argumento el simbolo químico e de una elemento, 
-- una oracion o palabra a escribir, subindices asociados a la cantidad de 
-- átomos de cada elemento en la formula, dos est. de oxidación fijos y 
-- un diccionario en donde encontrar el nombre y est. de oxidación de e. 

evalTradAux :: Nombre -> String -> ThreeSubIndex -> (EstOx, EstOx) -> Compuestos -> Output String (Either String (EstOx,EstOx))
evalTradAux e s (i,j,k) (v2,v3) dict = do  write s
                                           case posibleEo (i,j,k) eos (v2,v3) of
                                             Just eo -> do write (sufixPrefix (sufix n) eos eo) 
                                                           return (Right (eo,0))
                                             _       -> throw
                                  where (n,eos) = elemento e dict

-- Evaluador auxiliar de las sales neturas

evalTradSalNeutra :: Elem -> Output String (Either String (EstOx,EstOx))
evalTradSalNeutra (SalNeutra m i nm j) = do let (m', eosm) = elemento m metalElems 
                                                (nm', eosnm) = elemento nm salNeutraElems
                                                s = (uro $ (sufixAux nm'))
                                            case probarEo (i,j) eosm eosnm of
                                              Just (x,y) -> do write s
                                                               write (sufixPrefix (sufix m') eosm x) 
                                                               return (Right (x,y))                                     
                                              _          -> throw

-- Evaluador auxiliar de los hidrácidos

evalTradHidra :: Nombre -> String -> ThreeSubIndex -> (EstOx, EstOx) -> Compuestos -> Output String (Either String (EstOx,EstOx))
evalTradHidra e s (i,j,k) (v2,v3) dict = do write s
                                            case posibleEo (i,j,k) eos (v2,v3) of
                                              Just eo  -> do write $ sufixAux n 
                                                             write hidrico
                                                             return (Right (eo,0))
                                              _       -> throw
                                  where (n,eos) = elemento e dict

--Evaluadores adicionales del oxoacido
--oxoacido obtiene todas las posibles combinaciones entre los eos del metal y los del no metal y se los da
-- a oxoacido' 
oxoacido :: (SubIndex, SubIndex, SubIndex, SubIndex) -> [EstOx] -> [EstOx] -> EstOx -> Maybe (EstOx,EstOx)
oxoacido subindex eosm eosnm val = let xss = [(a,b) | a <- eosm, b <- eosnm]
                           in oxoacido' subindex xss val

-- i es el subindice del m, j es el subindice del nm y k el subindice del oxigeno,
-- v2 es el eo del O, es decir, (-2). Además, xss es una lista de pares de eos que 
-- representan todas las posibles combinaciones de los eos del metal y del no metal.
-- oxoacido' obtiene los eos correspondientes al metal y al no metal que hacen que 
-- la carga del compuesto sea 0.

oxoacido' :: (SubIndex, SubIndex, SubIndex, SubIndex) -> [(EstOx,EstOx)] -> EstOx -> Maybe (EstOx,EstOx)
oxoacido' _ [] _ = Nothing
oxoacido' p1@(i,j,k,t) ((m,nm):xss) v2 = if m*i + (nm*j + k *v2)*t == 0 then Just (m,nm) else oxoacido' p1 xss v2   
