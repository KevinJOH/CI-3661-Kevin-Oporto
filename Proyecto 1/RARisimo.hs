module RARisimo (frecuencias) where

import Hoffman (Hoffman(..), nuevoHoffman)
import Frecuencia (Frecuencia(..), contar, frecuencia)
import qualified Data.List as List

frecuencias :: String -> [Frecuencia Hoffman]
frecuencias str = map (\c -> Frecuencia (nuevoHoffman c) (countOcurrencias c str)) caracteresUnicos
  where
    caracteresUnicos = List.nub str
    countOcurrencias :: Char -> String -> Int
    countOcurrencias c = length . filter (==c)

ganadores :: [Frecuencia a] -> Maybe (Frecuencia a, Frecuencia a, [Frecuencia a])
ganadores xs
  | length xs < 2 = Nothing
  | otherwise = Just (min1, min2, resto)
  where
    ordenados = List.sortOn frecuencia xs
    min1 = head ordenados
    min2 = ordenados !! 1
    resto = drop 2 ordenados
