{-# LANGUAGE InstanceSigs #-}

-- Definición del tipo de datos Frecuencia que asocia un valor con su frecuencia.
data Frecuencia a = Frecuencia a Int
  deriving Eq

-- Instancia de la clase Show para Frecuencia, permite convertir una Frecuencia a una cadena de caracteres.
instance Show a => Show (Frecuencia a) where
  show :: Frecuencia a -> String
  show (Frecuencia x y) = "Valor: " ++ show x ++ " - Frecuencia: " ++ show y

-- Instancia de la clase Ord para Frecuencia, permite comparar frecuencias basándose en el número de ocurrencias.
instance Eq a => Ord (Frecuencia a) where
  compare (Frecuencia _ x) (Frecuencia _ y) = compare x y

-- Función para iniciar una frecuencia para un valor dado, con una ocurrencia.
iniciarFrecuencia :: Eq a => a -> Frecuencia a
iniciarFrecuencia x = Frecuencia x 1

-- Función para contar el número de ocurrencias de un valor en una lista y devolver una Frecuencia.
contar :: Eq a => a -> [a] -> Frecuencia a
contar x xs = Frecuencia x (length (filter (== x) xs))

-- Función para obtener el valor asociado a una Frecuencia.
valor :: Frecuencia a -> a
valor (Frecuencia x _) = x

-- Función para obtener la frecuencia (el número de ocurrencias) de un valor en una Frecuencia.
frecuencia :: Frecuencia a -> Int
frecuencia (Frecuencia _ x) = x
