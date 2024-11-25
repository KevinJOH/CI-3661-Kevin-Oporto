module RARisimo
  ( frecuencias,
    ganadores,
    hoffman,
    rarisimo,
  )
where

import Data.List qualified as List
import Data.Map qualified as Map
import Frecuencia (Frecuencia (..), contar, frecuencia, iniciarFrecuencia)
import Hoffman (Hoffman (..), codificacion, fusionHoffman, nuevoHoffman)

frecuencias :: String -> [Frecuencia Hoffman]
frecuencias str = map (\c -> frecuencia (nuevoHoffman c) (countOcurrencias c str)) caracteresUnicos
  where
    caracteresUnicos = List.nub str
    countOcurrencias :: Char -> String -> Int
    countOcurrencias c = length . filter (== c)

ganadores :: [Frecuencia a] -> Maybe (Frecuencia a, Frecuencia a, [Frecuencia a])
ganadores xs
  | length xs < 2 = Nothing
  | otherwise = Just (min1, min2, resto)
  where
    ordenados = List.sortOn frecuencia xs
    min1 = head ordenados
    min2 = ordenados !! 1
    resto = drop 2 ordenados

hoffman :: String -> Maybe Hoffman
hoffman str
  | null str = Nothing
  | otherwise = Just $ construirArbol (frecuencias str)
  where
    construirArbol [x] = valor x
    construirArbol xs = construirArbol (nuevoElemento : resto)
      where
        Just (min1, min2, resto) = ganadores xs
        nuevoElemento = Frecuencia (frecuencia min1 + frecuencia min2) (fusionHoffman (valor min1) (valor min2))

rarisimo :: String -> Map.Map Char String
rarisimo str = case hoffman str of
  Nothing -> Map.empty
  Just arbol -> generarCodigos arbol
  where
    generarCodigos (Hoja c) = Map.singleton c ""
    generarCodigos (Nodo izq der) = Map.union (Map.map ('0' :) (generarCodigos izq)) (Map.map ('1' :) (generarCodigos der))