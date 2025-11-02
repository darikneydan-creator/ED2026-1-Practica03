module Practica03 where

-- Tipo de dato Prop
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Sinonimo para los estados
type Estado = [String]

-- Ejercicio 1
variables :: Prop -> [String]
variables (Var x) = [x] --Si es una variable sin mas, entonces devuelve la letra de esa variable
variables (Cons _) = [] --Si es una constante, devuelve una lista vacia porque no hay variables
variables (Not x) = variables x --Si es un not, devuelve la variable que esta siendo negada
variables (And x y) = noDuplicar (variables x ++ variables y) --Si es un and, devuelve ambas variables pero se asegura de no repetir variables (ver comentarios en funcion auxiliar noDuplicar)
variables (Or x y) = noDuplicar (variables x ++ variables y) --Lo mismo con el resto de los casos
variables (Impl x y) = noDuplicar (variables x ++ variables y)
variables (Syss x y) = noDuplicar (variables x ++ variables y)


-- Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Var x) xs = if esElemento x xs then True else False --Si x es elemento de la lista xs (el estado), entonces significa que tiene un valor de True, si no es elemento entonces es False
interpretacion (Cons x) _ = x --Una constante es el valor asi solito
interpretacion (Not x) xs = negacion (interpretacion x xs) --Son funciones normalitas, pero creo que igual habia que implementarlas desde 0 entonces pueden ver funciones auxiliares hasta abajo para ver que hace cada funcion
interpretacion (And x y) xs = conjuncion (interpretacion x xs) (interpretacion y xs)
interpretacion (Or x y) xs = disyuncion (interpretacion x xs) (interpretacion y xs)
interpretacion (Impl x y) xs = disyuncion (negacion (interpretacion x xs)) (interpretacion y xs)
interpretacion (Syss x y) xs = disyuncion (conjuncion (interpretacion x xs) (interpretacion y xs)) (conjuncion (negacion (interpretacion x xs)) (negacion (interpretacion y xs)))

-- Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles x = conjuntoPotencia (variables x) --El conjunto potencia de x son todos sus estados posibles, ns no hay mas

-- Ejercicio 4
modelos :: Prop -> [Estado]
modelos x = modelosAux x (estadosPosibles x) --hice una funcion auxiliar porque no se como hacerlo sin ella, igual y si saben de una forma mas elegante de hacerlo pueden reemplazar esto

modelosAux :: Prop -> [Estado] -> [Estado]
modelosAux _ [] = [] -- Caso base
modelosAux x (y:xs) = if (interpretacion x y) then (y:(modelosAux x xs)) else (modelosAux x xs) --Checa si cada elemento de los estados posibles (que son estados) es valido. Si es valido entonces lo agrega a la lista, si no es valido se sigue con los demas

-- Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes x y = conjuncion (todasValidas x (modelos y)) (todasValidas y (modelos x)) --Si el modelo de x aplica a y, y el modelo de y aplica a x, entonces ambos modelos son iguales :p
                                                                                           --Queria comparar directamente que los modelos fueran iguales, pero como son listas, [[p], [q]] no es igual a [[q], [p]], entonces mejor hice otro tipo de comparacion

-- Ejercicio 6
tautologia :: Prop -> Bool
tautologia x = todasValidas x (estadosPosibles x) --Si todos los estados posibles son validos, entonces es tautologia

-- Ejercicio 7
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica xs x = tautologia (Impl (deslistarProps xs) (x)) --Si (conjuncion de los elementos de la lista implican x) es tautologia, entonces el argumento es valido, i.e. es consecuencia logica

--Funciones auxiliares
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs

negacion :: Bool -> Bool
negacion True = False
negacion False = True

conjuncion :: Bool -> Bool -> Bool
conjuncion True True = True
conjuncion _ False = False
conjuncion False _ = False

disyuncion :: Bool -> Bool -> Bool
disyuncion _ True = True
disyuncion True _ = True
disyuncion False False = False

esElemento :: Eq a => a -> [a] -> Bool --Funcion esElemento devuelve True si x es elemento de lista xs, False si no es elemento
esElemento _ [] = False
esElemento x (y:xs) = if (x == y) then True else (esElemento x xs)

noDuplicar :: Eq a => [a] -> [a] --Funcion noDuplicar checa que el primer elemento de una lista no este ya en la lista. Si sí esta entonces lo saca de la lista, si no está lo deja y sigue con el segundo elemento y así con el resto
noDuplicar [] = []
noDuplicar (x:xs) = if (esElemento x xs) then (noDuplicar xs) else (x:(noDuplicar xs))

todasValidas :: Prop -> [Estado] -> Bool
todasValidas _ [] = False --esto no deberia pasar nunca, pero da un warning si no lo pongo
todasValidas x (y:[]) = interpretacion x y --caso "base", checa si el ultimo estado es valido para x
todasValidas x (y:xs) = if (interpretacion x y) then (todasValidas x xs) else False --si el estado y es valido para x, se sigue con el resto. Si no, entonces no todos los estados dados son validos y se devuelve false

deslistarProps :: [Prop] -> Prop
deslistarProps [] = Cons False --esto no deberia pasar nunca, pero da un warning si no lo pongo
deslistarProps (x:[]) = x --la proposicion final de la lista
deslistarProps (x:xs) = And x (deslistarProps xs) --vuelve una lista de props a un solo prop que va A y B y C y ... y Z