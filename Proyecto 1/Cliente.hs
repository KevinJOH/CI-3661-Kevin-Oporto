module Main (main) where

import Control.Monad (when)
import Data.ByteString qualified as B
import Data.Map qualified as Map
import Hoffman (Hoffman (..), codificacion, fusionHoffman, nuevoHoffman)
import RARisimo (frecuencias, hoffman)
import System.Directory
import System.IO

main :: IO ()
main = do
  putStrLn "Bienvenido al programa de compresión Huffman"
  menu

menu :: IO ()
menu = do
  putStrLn "\nSeleccione una opción:"
  putStrLn "1. Codificar"
  putStrLn "2. Decodificar"
  putStrLn "3. Analizar"
  putStrLn "4. Salir"
  opcion <- getLine
  case opcion of
    "1" -> codificar >> menu
    "2" -> decodificar >> menu
    "3" -> analizar >> menu
    "4" -> putStrLn "Saliendo..."
    _ -> putStrLn "Opción no válida" >> menu

codificar :: IO ()
codificar = do
  putStrLn "Ingrese el path del archivo a codificar:"
  path <- getLine
  existe <- doesFileExist path
  when existe $ do
    contenido <- readFile path
    let arbol = hoffman contenido
    case arbol of
      Nothing -> putStrLn "Error al generar el árbol de Huffman"
      Just arbolH -> do
        let codigos = codificacion arbolH
        let contenidoCodificado = concatMap (codigos Map.!) contenido
        let nuevoPath = path ++ ".raro"
        B.writeFile nuevoPath (B.pack (map (fromIntegral . fromEnum) contenidoCodificado))
        putStrLn $ "Archivo codificado guardado en: " ++ nuevoPath

decodificar :: IO ()
decodificar = do
  putStrLn "Ingrese el path del archivo a decodificar (debe tener extensión .raro):"
  path <- getLine
  existe <- doesFileExist path
  when existe $ do
    contenido <- B.readFile path
    let contenidoDecodificado = map (toEnum . fromIntegral) (B.unpack contenido)
    let nuevoPath = take (length path - 5) path
    writeFile nuevoPath contenidoDecodificado
    putStrLn $ "Archivo decodificado guardado en: " ++ nuevoPath

analizar :: IO ()
analizar = do
  putStrLn "Ingrese el path del archivo a analizar:"
  path <- getLine
  existe <- doesFileExist path
  when existe $ do
    contenido <- readFile path
    let tamOriginal = length contenido
    let arbol = hoffman contenido
    case arbol of
      Nothing -> putStrLn "Error al generar el árbol de Huffman"
      Just arbolH -> do
        let codigos = codificacion arbolH
        let contenidoCodificado = concatMap (codigos Map.!) contenido
        let tamCodificado = length contenidoCodificado `div` 8
        let ganancia = fromIntegral (tamOriginal - tamCodificado) / fromIntegral tamOriginal * 100
        putStrLn $ "Tamaño original: " ++ show tamOriginal ++ " bytes"
        putStrLn $ "Tamaño codificado: " ++ show tamCodificado ++ " bytes"
        putStrLn $ "Ganancia: " ++ show ganancia ++ "%"