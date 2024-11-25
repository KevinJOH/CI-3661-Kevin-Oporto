{-# LANGUAGE InstanceSigs #-}

module Hoffman
  ( Hoffman (..),
    nuevoHoffman,
    fusionHoffman,
    obtenerCaracter,
    arbolIzquierdo,
    arbolDerecho,
    codificacion,
  )
where

import Control.Applicative ((<|>))
import Data.Map qualified as Map
import Text.Read (Lexeme (Ident, Punc), Read (..), ReadPrec, lexP, parens, prec, readPrec, step)

-- Definición del tipo de datos para el árbol de Hoffman.
data Hoffman = Hoja Char | Nodo Hoffman Hoffman
  deriving (Eq)

-- Instancia de la clase Show para el tipo Hoffman, permitiendo la conversión del árbol de Hoffman a una representación en cadena.
instance Show Hoffman where
  show (Hoja a) = "Hoja " ++ show a
  show (Nodo izq der) = "Nodo (" ++ show izq ++ ") (" ++ show der ++ ")"

-- Instancia de la clase Read para el tipo Hoffman, permitiendo la conversión de una cadena a un árbol de Hoffman.
instance Read Hoffman where
  readsPrec :: Int -> String -> [(Hoffman, String)]
  readsPrec _ input = readHoffman input

  readPrec :: Text.Read.ReadPrec Hoffman
  readPrec =
    parens $
      prec
        10
        ( do
            Ident "Hoja" <- lexP
            c <- step readPrec
            return (Hoja c)
        )
        <|> prec
          10
          ( do
              Ident "Nodo" <- lexP
              Punc "(" <- lexP
              izq <- readPrec
              Punc ")" <- lexP
              Punc "(" <- lexP
              der <- readPrec
              Punc ")" <- lexP
              return (Nodo izq der)
          )

-- Función auxiliar para leer un árbol de Hoffman desde una cadena.
readHoffman :: String -> [(Hoffman, String)]
readHoffman input = case lex input of
  [("Hoja", resto)] -> [(Hoja char, resto'') | (char, resto'') <- reads resto]
  [("Nodo", resto)] ->
    [ (Nodo izq der, resto'''')
      | ("(", resto') <- lex resto,
        (izq, resto'') <- readHoffman resto',
        (",", resto''') <- lex resto'',
        (der, resto'''') <- readHoffman resto''',
        (")", resto'''') <- lex resto'''
    ]
  _ -> []

-- Función para crear una nueva hoja de Hoffman a partir de un carácter.
nuevoHoffman :: Char -> Hoffman
nuevoHoffman = Hoja

-- Función para fusionar dos árboles de Hoffman, creando un nuevo nodo.
fusionHoffman :: Hoffman -> Hoffman -> Hoffman
fusionHoffman = Nodo

-- Función para obtener el carácter contenido en una hoja de Hoffman.
obtenerCaracter :: Hoffman -> Char
obtenerCaracter (Hoja a) = a
obtenerCaracter _ = error "No es una hoja"

-- Función para obtener el subárbol izquierdo de un nodo de Hoffman.
arbolIzquierdo :: Hoffman -> Hoffman
arbolIzquierdo (Nodo izq _) = izq
arbolIzquierdo _ = error "No es una rama"

-- Función para obtener el subárbol derecho de un nodo de Hoffman.
arbolDerecho :: Hoffman -> Hoffman
arbolDerecho (Nodo _ der) = der
arbolDerecho _ = error "No es una rama"

-- Función para generar la codificación de Hoffman de los caracteres en el árbol.
codificacion :: Hoffman -> Map.Map Char String
codificacion arbol = construirMapa arbol ""

-- Función auxiliar para construir el mapa de codificaciones de Hoffman.
construirMapa :: Hoffman -> String -> Map.Map Char String
construirMapa (Hoja a) camino = Map.singleton a camino
construirMapa (Nodo izq der) camino =
  Map.union
    (construirMapa izq (camino ++ "0"))
    (construirMapa der (camino ++ "1"))
