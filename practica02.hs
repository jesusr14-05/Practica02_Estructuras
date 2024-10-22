--López Rojas Jesús
--Pineda Morales Roberto Gael
--Ejercicios de la practica 2

-- 1. Longitud de una lista.
-- Función Longitud

longitud :: [a] -> Int
longitud[] = 0
longitud(x : xs) = 1 + longitud(xs)

-- 2. Suma de n números.
-- Función sumaLista

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x : xs) = x + sumaLista(xs)

-- 3. Agregar elemento a una lista.
-- Función agregarElemento

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento lista a bool =
                if bool
                then a:lista
                else lista++[a]

-- 4. Máximo de una lista.
-- Función maximoLista
maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "no puede ingresar una lista que este vacia"
maximoLista [x] = x
maximoLista (x : xs)=
    if x > maximoLista(xs)
        then x 
        else maximoLista(xs)

-- 5. Recuperar un elemento de una lista de acuerdo a su  índice.
-- Función indíce
indice :: [a] -> Int -> a
indice [] index = error "no se puede ingresar una lista vacia"
indice (x:xs) index = if index < 0 || index > longitud(x:xs)-1
    then error "el indice no es valido"
    else if index == 0
        then x
        else indice xs ( index -1)
-- Funcíon divisores

divisores :: Int -> [Int]
divisores n = [x|x <- [1..n], mod n x == 0]

--Función conjunto

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x :conjunto [y|y <- xs, y/=x]

-- Función numerosPares

numerosPares :: [Int] -> [Int]
numerosPares lista = [x|x <- lista, ((mod x 2) == 0)]



