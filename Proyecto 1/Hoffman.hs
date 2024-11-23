{-# LANGUAGE InstanceSigs #-}
import qualified Data.Map as Map
data Hoffman = Hoja Char | Nodo Hoffman Hoffman
  deriving Eq

instance Show Hoffman where
  show (Hoja a) = "Hoja " ++ show a
  show (Nodo izq der) = "Nodo (" ++ show izq ++ ") (" ++ show der ++ ")"

nuevoHoffman :: Char -> Hoffman
nuevoHoffman a = Hoja a

fusionHoffman :: Hoffman -> Hoffman -> Hoffman
fusionHoffman izq der = Nodo izq der

obtenerCaracter :: Hoffman -> Char
obtenerCaracter (Hoja a) = a

arbolIzquierdo :: Hoffman -> Hoffman
arbolIzquierdo (Nodo izq _) = izq

arbolDerecho :: Hoffman -> Hoffman
arbolDerecho (Nodo _ der) = der

codificacion :: Hoffman -> Map.Map Char String
codificacion arbol = construirMapa arbol ""

construirMapa :: Hoffman -> String -> Map.Map Char String
construirMapa (Hoja a) camino = Map.singleton a camino
construirMapa (Nodo izq der) camino =
  Map.union (construirMapa izq (camino ++ "0"))
            (construirMapa der (camino ++ "1"))

