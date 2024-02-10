{-
- Lógica Computacional 2024-2
- Profesor: Francisco Hernández Quiroz
- Ayudante: Marco Vladimir Lemus Yáñez
- Ayudante: Naomi Itzel Reyes Granados
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: José Ricardo Desales Santos
- Practica 1: Recordando Haskell. Árboles
- Integrantes: José Rubén Alfaro González
-
-
-}

module Lists where

data List a = Void | Cons a (List a) -- deriving (Show)

instance (Show a) => Show (List a) where
  show Void       = "[]"
  show (Cons a l) = "(" ++ (show a) ++ ":" ++ (show l) ++ ")"

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | myHead. Función que regresa tal vez la cabeza de la lista.
myHead :: List a -> Maybe a
myHead Void = Nothing
myHead (Cons a l) = Just a
-- | myTail. Función que regresa tal vez la cola de la lista.
myTail :: List a -> Maybe (List a)
myTail Void = Nothing
myTail (Cons a l) = Just l

-- | myLast. Función que regresa tal vez el último elemento de la
-- lista.
myLast :: List a -> Maybe a
myLast Void = Nothing
myLast (Cons a Void) = Just a
myLast (Cons a l) = myLast l

-- | myLen. Función que regresa la longitud de la lista.
myLen :: List a -> Int
myLen Void = 0
myLen (Cons a l) = 1 + myLen l

-- | isElem. Función que nos dice si un elemento está en una lista.
isElem :: (Eq a) => List a -> a -> Bool
isElem Void a = False
isElem (Cons a l) x = if a == x then True else isElem l x

-- | myReverse. Función que regresa la reversa de una lista.
myReverse :: List a -> List a
myReverse Void = Void
myReverse (Cons a l) = rhelp (Cons a l) Void

rhelp :: List a -> List a -> List a
rhelp Void rl = rl
rhelp (Cons a l) rl = rhelp l (Cons a rl)
-- | toHaskell. Función que pasa una de nuestras listas a las listas
-- de haskell.
toHaskell :: List a -> [a]
toHaskell Void = []
toHaskell (Cons a l) = [a] ++ toHaskell l 

-- | fromHaskell. Función que pasa una lista de haskell a nuestras
-- listas.
fromHaskell :: [a] -> List a
fromHaskell [] = Void
fromHaskell (x:xs) = (Cons x (fromHaskell xs))

--------------------------------------------------------------------------------
--------                           AUXILIARES                           --------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------

-- Lista que contiene a los primeros cinco elementos.
l1 = (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Void)))))

-- Lista que contiene a los elementos del 6-10.
l2 = (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Void)))))

head1 = myHead l1
-- Regresa: Just 1

head2 = myHead Void
-- Regresa: Nothing

tail1 = myTail l1
-- Regresa: Just (2:(3:(4:(5:[]))))

tail2 = myTail  Void
-- Regresa: Nothing

last1 = myLast l2
-- Regresa: 10

last2 = myLast Void
-- Regresa: Nothing

len1 = myLen l1
-- Regresa: 5

len2 = myLen l1
-- Regresa: 5

len3 = myLen Void
-- Regresa: 0

elem1 = isElem l1 9
-- Regresa: False

elem2 = isElem l2 9
-- Regresa: True

reverse1 = myReverse l1
-- Regresa: (5:(4:(3:(2:(1:[])))))

reverse2 = myReverse l2
-- Regresa: (10:(9:(8:(7:(6:[])))))

toHaskell1 = toHaskell l1
-- Regresa: [1,2,3,4,5]

toHaskell2 = toHaskell l2
-- Regresa: [6,7,8,9,10]

fromHaskell1 = fromHaskell [1,2,3]
-- Regresa: (1:(2:(3:[])))

fromHaskell2 = fromHaskell []
-- Regresa: Void
