type Conjunto a = a -> Bool

-- a)
miembro :: Conjunto a -> a -> Bool
miembro conj x = conj x

-- b)
vacio :: Conjunto a
vacio _ = False

-- c)
singleton :: (Eq a) => a -> Conjunto a
singleton x = \y -> y == x

-- d)
desdeLista :: (Eq a) => [a] -> Conjunto a
desdeLista xs = \y -> elem y xs

-- e)
complemento :: Conjunto a -> Conjunto a
complemento conj = \x -> not $ conj x

-- f)
union :: Conjunto a -> Conjunto a -> Conjunto a
union conjA conjB = \x -> conjA x || conjB x

-- g)
interseccion :: Conjunto a -> Conjunto a -> Conjunto a
interseccion conjA conjB = \x -> conjA x && conjB x

-- h)
diferencia :: Conjunto a -> Conjunto a -> Conjunto a
diferencia conjA conjB = \x -> conjA x && not (conjB x)

-- i)
transformar :: (b -> a) -> Conjunto a -> Conjunto b
transformar f conjA = \x -> conjA (f x)
