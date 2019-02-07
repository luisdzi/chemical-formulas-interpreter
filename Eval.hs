module Eval where

import EvalTrad 
import EvalStock
import EvalSist
import ForEval
import Tipos

--eval obtiene los nombres del compuesto e
eval :: Elem -> String
eval e = let (p1,p2) = runOut $ eval' e 
          in case p1 of
              Right _ -> p2
              Left er -> er


eval' :: Elem -> Output String (Either String (EstOx,EstOx))
eval' e = do case fst $ runOut $ evalTrad e of
              Right p -> do write "Nomenclatura tradicional: "
                            evalTrad e
                            write "\n"
                            write "Nomenclatura de stock: "
                            evalStock e p
                            write "\n"
                            write "Nomenclatura sistematica: "
                            evalSist e p
                            return (Right p)
              Left er -> return (Left er)
                        
-------------------------------------------------------------------------------

--evalEo obtiene los nombres del compuesto y además los eos de\los elemento\s.
evalEo :: Elem -> String
evalEo e = let (p1,p2) = runOut $ evalEo' e 
            in case p1 of
              Right _ -> p2
              Left er -> er

evalEo' :: Elem -> Output String (Either String (EstOx,EstOx))
evalEo' e = do case p' of 
                Right p@(p1,_) -> do write s
                                     write "\nEl estado de oxidacion del "
                                     case e of 
                                       (Oxosal _ _ _ _ _ _ _) -> writeEos ns p 
                                       (SalNeutra _ _ _ _) -> writeEos ns p
                                       _                   -> do write n
                                                                 write " es "
                                                                 write (show p1)
                                                                 return (Right p)
                Left er -> return (Left er)
            where (p', s) =  runOut $ eval' e
                  ns = nombreDosComp e
                  n  = nombreUnComp e

--Permite escribir los eos de dos elementos distintos              
writeEos :: (Nombre,Nombre) -> (EstOx,EstOx) -> Output String (Either String (EstOx,EstOx))
writeEos (n1,n2) p@(p1,p2) = do write n1
                                write " es "
                                write (show p1)
                                write " y el del "
                                write n2
                                write " es "
                                write (show p2)               
                                return (Right p)