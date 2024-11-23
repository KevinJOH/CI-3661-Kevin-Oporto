{-# LANGUAGE InstanceSigs #-}
data Frecuencia a = Frecuencia a Int
  deriving Eq

instance Show a => Show (Frecuencia a) where
  show :: Frecuencia a -> String
  show (Frecuencia x y) = "Valor: " ++ show x ++ " - Frecuencia: " ++ show y

instance Eq a => Ord (Frecuencia a) where
  compare (Frecuencia _ x) (Frecuencia _ y) = compare x y

iniciarFrecuencia :: Eq a => a -> Frecuencia a
iniciarFrecuencia x = Frecuencia x 1

contar :: Eq a => a -> [a] -> Frecuencia a
contar x xs = Frecuencia x (length (filter (== x) xs))

valor :: Frecuencia a -> a
valor (Frecuencia x _) = x

frecuencia :: Frecuencia a -> Int
frecuencia (Frecuencia _ x) = x
