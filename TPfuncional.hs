import Text.Show.Functions
import Data.List

--Punto 1 Modelizado de gimnasta
data Gimnasta = Gimnasta {
	nombre :: String,
	equilibrio :: Int,
	energia :: Int,
	fuerza :: Int,
	flexibilidad :: Int,
	habilidades :: [Gimnasta -> Gimnasta]
}

--Punto 2 Modelizado de Sonia  y Perdo
sonia = Cliente {
	nombre = "Sonia",
	energia = 90,
	equilibrio = 60,
	flexibilidad = 40,
	fuerza = 50,
	habilidades = [mediaLuna, rolAdelante 20, saltoMortal 15 40]
}

pedro = Cliente {
	nombre = "Pedro",
	energia = 70,
	equilibrio = 50,
	flexibilidad = 50,
	fuerza = 60,
	habilidades = [saltoConSoga 150, vertical, rolAdelante 30]
}

--Punto 3 Funcion ejercitar


-- Habilidades
mediaLuna gimnasta = Gimnasta {
	nombre = nombre cliente,
	equilibrio = (equilibrio cliente) + 5,
	energia = energia cliente,
	fuerza = fuerza cliente,
	flexibilidad = flexibilidad cliente,
}

rolAdelante gimnasta = Gimnasta {
	nombre = nombre cliente,
	equilibrio = equilibrio cliente,
	energia = energia cliente + (velocidad cliente)*2,
	fuerza = fuerza cliente,
	flexibilidad = flexibilidad cliente,
}

vertical gimnasta = Gimnasta {
	nombre = nombre cliente,
	equilibrio = equilibrio cliente,
	energia = energia cliente,
	fuerza = (fuerza cliente) + 7,
	flexibilidad = flexibilidad cliente,
}

saltoConSoga cantidadSaltos gimnasta = Gimnasta {
	nombre = nombre cliente,
	equilibrio = (equilibrio cliente) -  (cantidadSaltos / 2),
	energia = energia cliente,
	fuerza = fuerza cliente,
	flexibilidad = flexibilidad cliente,
}

saltoMortal impulso altura gimnasta = Gimnasta {
	nombre = nombre cliente,
	equilibrio = equilibrio cliente,
	energia = energia cliente,
	fuerza = (fuerza cliente) + altura,
	flexibilidad = (flexibilidad cliente) + (impulso / 2),
}