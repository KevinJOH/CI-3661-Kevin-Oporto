data ArbolMB a = Vacio
               | RamaM a (ArbolMB a)
               | RamaB a (ArbolMB a) (ArbolMB a)
  deriving Show

-- c)
plegarArbolMB :: b -> (a -> b -> b) -> (a -> b -> b -> b) -> ArbolMB a -> b
plegarArbolMB transVacio transRamaM transRamaB = plegar
  where
    plegar Vacio = transVacio
    plegar (RamaM x y) = transRamaM x (plegar y)
    plegar (RamaB x y z) = transRamaB x (plegar y) (plegar z)

-- d)
sumarArbolMB :: (Num a) => ArbolMB a -> a
sumarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
  where
    transVacio = 0
    transRamaM x subArbol = x + subArbol
    transRamaB x subArbolIzq subArbolDer = x + subArbolIzq + subArbolDer

{-
 Prueba: 
  sumarArbolMB (RamaB 15 (RamaM 8 (RamaB 4 (RamaM 3 Vacio) Vacio)) (RamaB 2 Vacio Vacio))
 Retorna: 32
-}

-- e)
aplanarArbolMB :: ArbolMB a -> [a] 
aplanarArbolMB = plegarArbolMB transVacio transRamaM transRamaB 
  where 
    transVacio = [] 
    transRamaM x subArbol = x : subArbol 
    transRamaB x subArbolIzq subArbolDer = subArbolIzq ++ [x] ++ subArbolDer

{-
 Prueba:
  aplanarArbolMB (RamaB 15 (RamaM 8 (RamaB 4 (RamaM 3 Vacio) Vacio)) (RamaB 2 Vacio Vacio))
 Retorna: [8, 3, 4, 15, 2]
-}

-- f)
analizarArbolMB :: (Ord a) => ArbolMB a -> Maybe (a, a, Bool)
analizarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
  where
    transVacio = Nothing
    transRamaM x Nothing = Just (x, x, True)
    transRamaM x (Just (subArbolMin, subArbolMax, isOrden)) = 
      Just (min x subArbolMin, max x subArbolMax, isOrden && x <= subArbolMin) 
    transRamaB x Nothing Nothing = Just (x, x, True)
    transRamaB x (Just (subArbolMin, subArbolMax, isOrden)) Nothing = 
      Just (min x subArbolMin, max x subArbolMax, isOrden && x >= subArbolMax)
    transRamaB x Nothing (Just (subArbolMin, subArbolMax, isOrden)) = 
      Just (min x subArbolMin, max x subArbolMax, isOrden && x <= subArbolMin)
    transRamaB x (Just (subArbolMinIzq, subArbolMaxIzq, isOrdenIzq)) (Just (subArbolMinDer, subArbolMaxDer, isOrdenDer)) =
      Just (min x (min subArbolMinIzq subArbolMinDer), max x (max subArbolMaxIzq subArbolMaxDer), isOrdenIzq && isOrdenDer && x <= subArbolMinDer && x >= subArbolMaxIzq)

{-
 Prueba:
  analizarArbolMB (RamaB 15 (RamaM 8 (RamaB 4 (RamaM 3 Vacio) Vacio)) (RamaB 2 Vacio Vacio))
 Retorna: (2,15,False)

 Prueba2:
  analizarArbolMB (RamaB 7 (RamaM 1 (RamaB 3 (RamaM 2 Vacio) Vacio)) (RamaB 8 Vacio Vacio))
 Retorna: (1,8,True)
-}
