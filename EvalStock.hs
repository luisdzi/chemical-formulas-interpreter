module EvalStock where 

import ForEval
import Tipos
import Elementos

evalStock :: Elem -> (EstOx,EstOx) -> Output String (EstOx,EstOx)
evalStock e p = evalStock' e p 

evalStock' :: Elem -> (EstOx,EstOx) -> Output String (EstOx, EstOx)
evalStock' (OxidoMet m _ _ _) p = evalStockAux m oxidoDe p metalElems
evalStock' (Peroxido m _ _ _) p = evalStockAux m peroxidoDe p metalElems
evalStock' (Hidruro m _ _) p    = evalStockAux m hidruroDe p metalElems
evalStock' (Hidroxido m _ _ _) p = evalStockAux m hidroxidoDe p metalElems
evalStock' (Anhidrido nm _ _ _) p  = evalStockAux nm oxidoDe p noMetalElems
evalStock' oxoac@(Oxoacido _ _ _ _ _ _) p = evalStockOxoacido oxoac p
evalStock' (HidruroVolatil _ _ _) p = do write "Los hidruros volatiles carecen de nomenclatura de stock"
                                         return p 
evalStock' (Hidracido _ _ _ ) p = do write "Los hidracidos carecen de nomenclatura de stock"
                                     return p                                  
evalStock' (SalNeutra m _ nm _) p = do let (nm',_) = elemento nm noMetalElems 
                                           s = (urode (sufixAux nm'))
                                       evalStockAux m s p metalElems
                                       return p
evalStock' (Oxosal m _ nm _ _ _ _) p@(v1,v2) = do write (sufixPrefixOxo (sufixAux nm') eonm v2)
                                                  write de
                                                  write m'
                                                  if oneElem eom then do write nothing 
                                                                 else do write $ numRom v1
                                                  return p    
                                          where (nm',eonm) = elemento nm noMetalElems
                                                (m',eom)  = elemento m metalElems
 
-------------------------------------------------------------------------------
-- El objetivo de los evaluadores auxiliares, que se encuentran debajo, es 
--      intentar que la evaluacion de stock sea mas comprensible 
-------------------------------------------------------------------------------


evalStockAux :: Nombre -> String -> (EstOx, EstOx) -> Compuestos -> Output String (EstOx,EstOx)
evalStockAux e s p@(v1,_) dict = do write s
                                    write n
                                    if oneElem eo then do write nothing 
                                                  else do write $ numRom v1
                                    return p
                                  where (n,eo) = elemento e dict
                                    

evalStockOxoacido :: Elem -> (EstOx,EstOx) -> Output String (EstOx, EstOx)
evalStockOxoacido (Oxoacido _ _ nm i _ k) p@(v1,_) = do let (n,_) = elemento nm noMetalElems
                                                        write acid
                                                        write (cantSub' k)
                                                        write oxo
                                                        write (cantSub' i)
                                                        write (doubleVowel (sufix n) ico)
                                                        write $ numRom v1
                                                        return p

                                               


