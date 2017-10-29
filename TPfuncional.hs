import Text.Show.Functions

-- Punto 1 
type Ejercicio = Gimnasta -> Gimnasta
type RutinaEjercicio = (Int,Ejercicio)
type Rutina = (Int,[RutinaEjercicio])

data Gimnasta = Gimnasta 
{
    nombre :: String,
    energia :: Int,
    equilibrio :: Int,
    flexibilidad :: Int,
    fuerza :: Int,
    ejercicios :: [Ejercicio]
} deriving (Show)

aumentarEnergia :: Int -> Gimnasta -> Gimnasta
aumentarEnergia cantidad gimnasta = gimnasta {energia = energia gimnasta + cantidad}

aumentarEquilibrio :: Int -> Gimnasta -> Gimnasta
aumentarEquilibrio cantidad gimnasta = gimnasta {equilibrio = equilibrio gimnasta + cantidad}

umentarFlexibilidad :: Int -> Gimnasta -> Gimnasta
aumentarFlexibilidad cantidad gimnasta = gimnasta {flexibilidad = flexibilidad gimnasta + cantidad}

aumentarFuerza :: Int -> Gimnasta -> Gimnasta
aumentarFuerza cantidad gimnasta = gimnasta {fuerza = fuerza gimnasta + cantidad}

medialuna gimnasta = aumentarEquilibrio 5 gimnasta
rolAdelante velocidad gimnasta = aumentarEnergia (div velocidad 2) gimnasta
vertical gimnasta = aumentarFuerza 7 gimnasta
saltoConSoga cantSaltos gimnasta = (aumentarFuerza cantSaltos.aumentarEnergia (-div cantSaltos 2)) gimnasta
saltoMortal altura impulso gimnasta = (aumentarFuerza altura.aumentarFlexibilidad (div impulso 2)) gimnasta

-- Punto 2 
sonia = Gimnasta "sonia" 90 60 40 50 [medialuna,rolAdelante 20,saltoMortal 40 15]
pedro = Gimnasta "pedro" 70 50 50 60 [saltoConSoga 150,vertical,rolAdelante 30]

-- Punto 3
-- Ej A)
aprender :: Ejercicio -> Gimnasta -> Gimnasta
aprender ejercicio gimnasta = gimnasta {ejercicios = (ejercicio:(ejercicios gimnasta))}

ejercitar :: Int -> Ejercicio -> Gimnasta -> Gimnasta
ejercitar cantMinutos ejercicio gimnasta | (div cantMinutos 2) > 0 = ejercitar (cantMinutos -2) ejercicio (ejercicio gimnasta)
                                         | otherwise = aprender ejercicio gimnasta

-- Ej B)
entradaCalor :: Rutina
entradaCalor = (2, [(2 ,rolAdelante 10), (4, medialuna), (1,saltoConSoga 50), (1,saltoMortal 20 15)])

rutinaDiaria :: Rutina
rutinaDiaria = (3, [(1,rolAdelante 20),(1,saltoConSoga 30),(1,vertical),(1,medialuna),(1,saltoConSoga 10)])

-- Ej C)
repetir :: Int -> (Gimnasta -> Gimnasta) -> Gimnasta -> Gimnasta
repetir 0 _ gimnasta = gimnasta
repetir repeticiones funcion gimnasta = repetir (repeticiones-1) funcion (funcion gimnasta)

realizarEjercicioRut :: Gimnasta -> RutinaEjercicio -> Gimnasta
realizarEjercicioRut gimnasta (repeticiones,ejercicio) = repetir repeticiones ejercicio gimnasta

realizarEjerciciosRut :: [RutinaEjercicio] -> Gimnasta -> Gimnasta
realizarEjerciciosRut ejercicios gimnasta = foldl realizarEjercicioRut gimnasta ejercicios

entrenar :: Rutina -> Gimnasta -> Gimnasta
entrenar (repeticiones,ejercicios) gimnasta = repetir repeticiones (realizarEjerciciosRut ejercicios) gimnasta

-- Ej D)
nivelFortaleza :: Gimnasta -> Int
nivelFortaleza (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = energia+fuerza

realizarEjerciciosPers :: Gimnasta -> Gimnasta
realizarEjerciciosPers gimnasta = foldr ($) gimnasta (ejercicios gimnasta) 

tienePotencial :: Int -> Gimnasta -> Bool
tienePotencial n gimnasta = (nivelFortaleza.realizarEjerciciosPers.entrenar rutinaDiaria) gimnasta > n

-- Punto 4
-- A)
nombreMaxGenericoRutina :: (Gimnasta -> Int) -> Rutina -> [Gimnasta] -> String
nombreMaxGenericoRutina criterio_max rutina gimnastas = (nombre.maxSegun criterio_max.entrenarGimnastasRutina rutina) gimnastas

entrenarGimnastasRutina :: Rutina -> [Gimnasta] -> [Gimnasta]
entrenarGimnastasRutina rutina gimnastas = map (entrenar rutina) gimnastas

maxSegun :: Ord b => (Gimnasta -> b) -> [Gimnasta] -> Gimnasta
maxSegun _ [elemento] = elemento
maxSegun funcion (x:y:xs) | funcion x > funcion y = maximoSegun funcion(x:xs)
                          | otherwise = maximoSegun funcion(y:xs)

maxDespuesDeRutina :: [Gimnasta] -> String
maxDespuesDeRutina gimnastas = nombreMaxGenericoRutina fuerza rutinaDiaria gimnastas

-- B)
fortaleza :: Gimnasta -> Int
fortaleza gimnasta = (energia gimnasta) + (fuerza gimnasta)

minEntreDos :: (Gimnasta->Int) -> (Gimnasta->Int) -> Gimnasta -> Int
minEntreDos funcion1 funcion2 gimnasta | (funcion1 gimnasta) > (funcion2 gimnasta) = funcion2 gimnasta
                                       | otherwise = funcion1 gimnasta

maxConMin :: [Gimnasta] -> String
maxConMin gimnastas = nombreMaxGenericoRutina (minimoEntreDos flexibilidad fortaleza) rutinaDiaria gimnastas

-- C)
entrenarEjercicio :: Ejercicio -> Int -> [Gimnasta] -> [Gimnasta]
entrenarEjercicio ejercicio cantMinutos gimnastas = map (ejercitar cantMinutos ejercicio) gimnastas

cantidadEjercicios :: Gimnasta -> Int
cantidadEjercicios gimnasta = (length.ejercicios) gimnasta

nombreMaxGenericoEjercicio :: (Gimnasta -> Int) -> Ejercicio -> Int -> [Gimnasta] -> String
nombreMaxGenericoEjercicio criterio_max ejercicio cantMinutos gimnastas = (nombre.maxSegun criterio_max.entrenarConEjercicio ejercicio cantMinutos) gimnastas

maxDespuesDeEjercicio :: [Gimnasta] -> Int -> Ejercicio -> String
maxDespuesDeEjercicio gimnastas cantMinutos ejercicio = nombreMaxGenericoEjercicio (cantidadEjercicios) ejercicio cantMinutos gimnastas

-- Punto 5
--           Se utilizan Expresiones Lambda y composicion de funciones.
--           El parametro e es comparable, ya que esta definido como Eq, el parametro g es una Expresion Lambda y una lista de los elementos iguales a e

-- Punto 6
--           A_ No se puede implementar ya que el type Ejercicio no es de Eq
--           B_ Si la funcion termina si y solo si se cumple con la condicion, por ejemplo buscar algun socio que tenga 100 de fuerza en la lista de gimnastas
--              h 100 (\n->n fuerza) listaInfinita