import Text.Show.Functions
import Data.List

-- Modelo de datos (Gimnasta y rutina)
type Ejercicio = (Gimnasta -> Gimnasta)

data Gimnasta = Gimnasta {
    nombre :: String,
    energia :: Int,
    equilibrio :: Int,
    flexibilidad :: Int,
    fuerza :: Int,
    habilidad :: [Gimnasta -> Gimnasta]
}

data Rutina = Rutina{
    repeticiones :: Int,
    habilidades :: [Gimnasta -> Gimnasta]
}
-- ejercicios

medialuna :: Ejercicio
medialuna gimnasta = Gimnasta {
    equilibrio = equilibrio gimnasta + 5
}

rolAdelante :: Int -> Ejercicio
rolAdelante gimnasta velocidad = Gimnasta {
    energia = energia gimnasta + (round $ fromIntegral (velocidad / 2))
}

vertical :: Ejercicio
vertical gimnasta = Gimnasta {
    fuerza = fuerza gimnasta + 5
}

saltoConSoga :: Int -> Ejercicio
saltoConSoga gimnasta saltos = Gimnasta {
    fuerza = fuerza gimnasta + (round $ fromIntegral saltos),
    energia = energia gimnasta - (round $ fromIntegral (saltos / 2))
}

saltoMortal :: Int -> Int -> Ejercicio
saltoMortal altura impulso gimnasta = Gimnasta {
    flexibilidad = flexibilidad gimnasta * altura,
    fuerza = fuerza gimnasta + altura
}
-- Sonia y Pedro

sonia = Gimnasta {
    nombre = "Sonia",
    energia = 90,
    equilibrio = 60,
    flexibilidad = 40,
    fuerza = 50,
    habilidad = [medialuna, rolAdelante 20, saltoMortal 50 15]
}

pedro = Gimnasta {
    nombre = "Pedro",
    energia = 70,
    equilibrio = 50,
    flexibilidad = 50,
    fuerza = 60,
    habilidad = [saltoConSoga 150, vertical, rolAdelante 30]
}
-- ejercitar

ejercitar gimnasta ejercicio tiempo {
    |tiempo < 120 = habilidad gimnasta = [ejercicio:habilidad gimnasta]
	|otherwise = ejercicio gimnasta , tiempo = tiempo - 120, ejercitar gimnasta ejercicio (tiempo - 120)
}
-- rutinas

entradaEnCalor = Rutina {
    repeticiones = 2,
    habilidades = [rolAdelante 10, rolAdelante 10, medialuna, medialuna, medialuna, medialuna, saltoConSoga 50, saltoMortal 20 15]
}

rutinaDiaria = Rutina {
    repeticiones = 3,
	habilidades = [rolAdelante 20, saltoConSoga 30, vertical, medialuna, saltoConSoga 10]
}
-- entrenar

entrenar gimnasta rutina{
    |repeticiones = 0
    |otherwise hacerUnEjercicio gimnasta habilidades, repeticiones = repeticiones - 1
}
hacerUnEjercicio gimnasta [ejercicio] {
    | head ejercicio = null
    | otherwise = head(ejercicio) gimnasta, take 1 ejercicio
}
-- potencial

-- maximos y minimos

maxFuerza [gimnasta] = gimnasta 
maxFuerza [x]    = x
maxFuerza (x:xs) = fuerza x max fuerza xs


minFuerza [gimnasta] = gimnasta 
minFuerza [x] = x
minFuerza (x:xs) = fuerza x min fuerza xs

minFlex [gimnasta] = gimnasta 
minFlex [x] = x
minFlex (x:xs) = flex x min flex xs

-- una función h es una subrutina definida que no está enlazada a un identificador.
-- inferencia de tipos y lazy evaluation.

-- 6a No, de agregarlo inteferiria en el punto 4c y el que ya sabe el ejercicio no lo haria.
-- 6b Si, si la condicion de fin es encontrar a un socio en particular.