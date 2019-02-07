module Elementos where 

import Tipos
import qualified Data.Map as Map

-------------------------------------------------------------------------------
--           Diccionarios con los distintos tipos de elementos
-------------------------------------------------------------------------------

-- Los numeros ubicados a la izquierda de cada fila representan los grupos a los 
--              que partenecen cada elemento en la tabla periódica

-- Elementos metálicos
metalElems:: Compuestos
metalElems = Map.fromList [
                {- 1 -}  ("H",("hidrogeno",[1])),("Li",("litio",[1])), ("Na", ("sodio", [1])), ("K", ("potasio", [1])), 
                         ("Rb",("rubidio",[1])), ("Cs",("cesio",[1])), ("Fr",("francio",[1])),
                {- 2 -}  ("Be",("berilio",[2])), ("Mg",("magnesio",[2])), ("Ca",("calcio",[2])), ("Sr",("estroncio",[2])), 
                         ("Ba",("bario",[2])), ("Ra",("radio",[2])),
                {- 3 -}  ("Sc",("escandio",[3])), ("Y",("itrio",[3])), ("La",("lantano",[3])), ("Ac",("actinio",[3])), 
                {- 4 -}  ("Ti",("titanio",[2,3,4])), ("Zr",("circonio",[4])), ("Hf",("hafnio",[4])), ("Ce",("cerio",[3,4])),
                         ("Th",("torio",[4])),
                {- 5 -}  ("V" ,("vanadio",[2,3,4,5])), ("Nb",("niobio",[2,3,4,5])), ("Ta",("tantalio",[5])), 
                         ("Pr",("praseodimio",[3,4])), ("Pa",("protactinio",[4,5])), 
                {- 6 -}  ("Cr",("cromo",[2,3])), ("Mo",("molibdeno",[6,5,4,3,2])), ("W",("tungsteno",[2,3,4,5,6])),
                         ("Nd",("neodimio",[3])), ("U",("uranio",[3,4,5,6])),  
                {- 7 -}  ("Mn",("manganeso", [2,3])), ("Re",("renio",[4,5,7])), 
                {- 8 -}  ("Fe",("hierro", [2,3])), ("Ru",("rutenio",[2,3,4,6,8])), ("Os",("osmio",[2,3,4,6,8])),
                         ("Sm",("samario",[2,3])),
                {- 9 -}  ("Co",("cobalto",[2,3])), ("Rh",("rodio",[2,3,4])), ("Ir",("iridio",[2,3,4,6])), 
                         ("Eu",("europio",[2,3])),
                {- 10 -} ("Ni",("niquel",[2,3])), ("Pd",("paladio",[2,4])), ("Pt",("platino",[2,4])),
                         ("Gd",("gadolinio",[3])),
                {- 11 -} ("Cu",("cobre",[1,2])), ("Ag",("plata",[1])), ("Au",("oro",[1,3])), ("Tb",("terbio",[3,4])),
                {- 12 -} ("Zn",("cinc",[2])), ("Cd",("cadmio",[2])), ("Hg",("mercurio",[1,2])), ("Dy",("disprosio",[3])), 
                {- 13 -} ("Al",("aluminio",[3])), ("Ga",("galio",[3])), ("In",("indio",[3])), ("Tl",("talio",[1,3])), 
                         ("Ho",("holmio",[3])),
                {- 14 -} ("Ge",("germanio",[4])), ("Sn",("estaño",[2,4])), ("Pb",("plomo",[2,4])), ("Er",("erbio",[3])), 
                {- 15 -} ("N",("nitrogeno",[2,4])),("Sb",("antimonio",[3,5])), ("Bi",("bismuto",[3,5])), 
                         ("Tm",("tulio",[2,3])),
                {- 16 -} ("Po",("polonio",[2,4,6])), ("Yb",("iterbio",[2,3])),
                {- 17 -} ("Lu",("lutecio",[3])) 
                          ]

-- Elementos no metálicos
noMetalElems:: Compuestos
noMetalElems = Map.fromList [
                {- 6 -}  ("Cr",("cromo",[4,6])),
                {- 7 -}  ("Mn", ("manganeso", [0,4,6,7])),
                {- 13 -} ("B",("boro",[3])), 
                {- 14 -} ("C", ("carbono", [2,4])), ("Si",("silicio",[4])), 
                {- 15 -} ("N",("nitrogeno",[1,3,5])), ("P",("fosforo",[3,5])), ("As",("arsenico",[3,5])),
                         ("Sb",("antimonio",[3])),
                {- 16 -} ("S", ("azufre",[2,4,6])), ("Se",("selenio",[4,6])), ("Te",("telurio",[4,6])),
                {- 17 -} ("F",("fluor",[-1])),("Cl",("cloro",[1,3,5,7])), ("Br",("bromo",[1,3,5,7])), ("I",("yodo",[1,3,5,7])), 
                         ("At",("astato",[1,3,5,7]))
                             ]

-- Elementos no metálicos que forman sales neutras
salNeutraElems :: Compuestos 
salNeutraElems = Map.fromList [
                {- 13 -} ("B", ("boro",[-3])),
                {- 14 -} ("C",("carbono",[-4])), ("Si",("silicio",[-4])),
                {- 15 -} ("N",("nitrogeno",[-3])), ("P",("fosforo",[-3])), ("As", ("arsenico", [-3])), ("Sb",("antimonio",[-3])),
                {- 16 -} ("S",("azufre",[-2])), ("Te",("telurio",[-2])), ("Se",("selenio",[-2])),
                {- 17 -} ("F", ("fluor", [-1])), ("Cl", ("cloro", [-1])) , ("Br",("bromo", [-1])), 
                         ("I",("yodo",[-1])), ("At",("astato",[-1]))
                              ] 

-- Elementos que forman hidruros volatiles
hidruroVolatilElems :: Compuestos
hidruroVolatilElems = Map.fromList [("N",("amoniaco", [3])), ("P",("fosfina",[3])), ("As",("arsina",[3])), 
                                    ("Sb",("estibina", [3])), ("C",("metano",[4])), ("Si",("silano",[4])), 
                                    ("B",("borano",[3]))]

--Elementos que forman hidracidos
hidraElems :: Compuestos
hidraElems = Map.fromList [("F",("fluor",[1])),("Cl",("cloro",[1])), ("Br",("bromo",[1])), ("I",("yodo",[1])),
                           ("S",("azufre",[2])), ("Te",("telurio",[2])), ("Se",("selenio",[2])) ]