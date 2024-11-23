{-# LANGUAGE InstanceSigs #-}
import qualified Data.Map as Map
import Text.Read (Read(..), ReadPrec, Lexeme(Ident, Punc), lexP, parens, prec, readPrec, step)
import Control.Applicative ((<|>))

data Hoffman = Hoja Char | Nodo Hoffman Hoffman
  deriving Eq

instance Show Hoffman where
  show (Hoja a) = "Hoja " ++ show a
  show (Nodo izq der) = "Nodo (" ++ show izq ++ ") (" ++ show der ++ ")"

instance Read Hoffman where
  readsPrec :: Int -> String -> [(Hoffman, String)]
  readsPrec _ input = readHoffman input

  readPrec :: Text.Read.ReadPrec Hoffman
  readPrec = parens $ (prec 10 $ do
                        Ident "Hoja" <- lexP
                        c <- step readPrec
                        return (Hoja c))
                      <|> (prec 10 $ do
                        Ident "Nodo" <- lexP
                        Punc "(" <- lexP
                        izq <- readPrec
                        Punc ")" <- lexP
                        Punc "(" <- lexP
                        der <- readPrec
                        Punc ")" <- lexP
                        return (Nodo izq der))

readHoffman :: String -> [(Hoffman, String)]
readHoffman input = case lex input of
  [("Hoja", resto)] -> [(Hoja char, resto'') | (char, resto'') <- reads resto]
  [("Nodo", resto)] -> 
    [(Nodo izq der, resto'''')
    | ("(", resto') <- lex resto
    , (izq, resto'') <- readHoffman resto'
    , (",", resto''') <- lex resto''
    , (der, resto'''') <- readHoffman resto'''
    , (")", resto'''') <- lex resto''']
  _ -> []

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

