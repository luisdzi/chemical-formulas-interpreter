module Tipos where

import Control.Monad (liftM, ap)
import qualified Data.Map as Map

-------------------------------------------------------------------------------
--  Tipos relacionados con la representacion de formulas químicas inorganicas
-------------------------------------------------------------------------------

data Elem = OxidoMet Metal Integer Oxigeno Integer
          | Oxosal Metal Integer NoMetal Integer Oxigeno Integer Integer
          | Hidruro Metal Hidrogeno Integer
          | SalNeutra Metal Integer NoMetal Integer
          | Hidroxido Metal Oxigeno Hidrogeno Integer
          | Anhidrido NoMetal Integer Oxigeno Integer
          | Hidracido Hidrogeno Integer NoMetal 
          | Oxoacido Hidrogeno Integer NoMetal Integer Oxigeno Integer 
          | HidruroVolatil NoMetal Hidrogeno Integer
          | Peroxido Metal Integer Oxigeno Integer  
             deriving Show 

-- La idea de los renombres es que sea mas facil entender que hacen las 
-- distintas funciones a lo largo del programa

type Nombre = String
type Metal   = String
type NoMetal = String
type Oxigeno = String 
type Hidrogeno = String
type EstOx = Integer
type SubIndex = Integer
type ThreeSubIndex = (SubIndex, SubIndex, SubIndex)

-- Diccionario de formulas, nombres y estados de oxidacion

type Compuestos = Map.Map String (Nombre, [EstOx])

-------------------------------------------------------------------------------
--    Monada encargada de ir llevando el nombre del compuesto y 
--                      sus estados de oxidación
-------------------------------------------------------------------------------

newtype Output w a = Out (a,w) deriving Show

instance Monoid w => Monad (Output w) where
  return = \x -> Out(x, mempty)
  (Out (a,w)) >>= f = let Out(b,w1) = f a
                      in Out(b, mappend w w1) 

instance Monoid w => Functor (Output w) where
  fmap = liftM

instance Monoid w => Applicative (Output w) where
  pure = return
  (<*>) = ap

-- Devuelve un mensaje de error en caso de que la evaluacion sea erronea
throw :: Output String (Either String (EstOx,EstOx))
throw = return (Left "La carga del compuesto no es 0" )

-- Desencapsula el los datos dentro de la monada Output
runOut :: Output a b -> (b,a)
runOut (Out (x,y)) = (x,y)
 
-- Escribe en la traza de la monada Output
write :: Monoid w => w -> Output w ()
write w = Out ((), w)

