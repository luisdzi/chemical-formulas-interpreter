module EvalSist where

import ForEval
import Tipos
import Elementos

evalSist :: Elem -> (EstOx,EstOx) -> Output String (EstOx,EstOx) 
evalSist (OxidoMet m i _ j) p    = evalSistAux m p oxidoDe (i,j) metalElems
evalSist (Anhidrido nm i _ j) p  = evalSistAux nm p oxidoDe (i,j) noMetalElems
evalSist (Peroxido m i _ j) p    = evalSistPer m p peroxidoDe (i,j) metalElems
evalSist (Hidruro m _ j) p       = evalSistAux m p hidruroDe  (1,j) metalElems
evalSist (HidruroVolatil nm _ j) p = evalSistAux nm p hidruroDe (1,j) noMetalElems 
evalSist (Hidroxido m _ _ j) p   = evalSistAux m p hidroxidoDe (1,j) metalElems
evalSist oxoac@(Oxoacido _ _ _ _ _ _) p = evalSistOxoacido oxoac p
evalSist oxo@(Oxosal _ _ _ _ _ _ _) p = evalSistOxosal oxo p 
evalSist (SalNeutra m i nm j ) p = evalSistAux m p ((urode $ sufixAux nm')) (i,j) metalElems
                                     where (nm',_) = elemento nm salNeutraElems
evalSist (Hidracido _ _ nm) p      = do write $ urode (sufixAux nm')
                                        write hidrogen
                                        return p 
                                      where (nm',_) = elemento nm hidraElems

-------------------------------------------------------------------------------
-- El objetivo de los evaluadores auxiliares, que se encuentran debajo, es 
--      intentar que la evaluacion sistemática sea mas comprensible 
-------------------------------------------------------------------------------


evalSistAux :: Nombre -> (EstOx,EstOx) -> String -> (SubIndex,SubIndex) -> Compuestos -> Output String (EstOx,EstOx)
evalSistAux e p s (i,j) dict = do write (doubleVowel (cantSub j) s)
                                  if i == 1 then write e' 
                                            else write (doubleVowel (cantSub i) e')
                                  return p 
                                    where (e',_) = elemento e dict

evalSistPer :: Nombre -> (EstOx,EstOx) -> String -> (SubIndex,SubIndex) -> Compuestos -> Output String (EstOx,EstOx)
evalSistPer e p s (i,j) dict = do if j == 1 then write s
                                            else write (doubleVowel (cantSub j) s)
                                  if i == 1 then write e' 
                                            else write (doubleVowel (cantSub i) e')
                                  return p 
                                    where (e',_) = elemento e dict


evalSistOxosal :: Elem -> (EstOx,EstOx) -> Output String (EstOx,EstOx)
evalSistOxosal (Oxosal m i nm j _ k t) p@(_,v2) = do write $ prefGriegos t
                                                     write $ cantSub k
                                                     write oxo
                                                     if j == 1 then write (ato $ sufixAux nm')
                                                               else write (ato $ doubleVowel (cantSub j) (sufixAux nm'))
                                                     write $ numRom v2
                                                     write de
                                                     if i == 1 then write nothing else write (cantSub i)
                                                     write m'
                                                     return p
                                            where (m',_) = elemento m metalElems
                                                  (nm',_) = elemento nm noMetalElems

evalSistOxoacido :: Elem -> (EstOx,EstOx) -> Output String (EstOx,EstOx)
evalSistOxoacido (Oxoacido _ _ nm i _ k) p@(v1,_) = do write $ doubleVowel (cantSub' k) oxo
                                                       write (ato $ doubleVowel (cantSub' i) (sufixAux nm'))
                                                       write $ numRom v1
                                                       write deHidrogeno
                                                       return p
                                            where (nm',_) = elemento nm noMetalElems


