import Data.List

--1) Modelar las siguientes sustancias
--a. El hidrógeno y el oxígeno
data Elemento = Elemento {
nombre 				  :: String, 
simboloQuimico  :: String, 
numeroAtomico   :: Int,
tipoDeSustancia :: String
} deriving (Show)

data Componente = Componente {
  elemento :: Elemento,
  cantidadElemento :: Int
} deriving (Show)

oxigeno :: Elemento
oxigeno = Elemento "Oxigeno" "O" 8 "No Metal"

hidrogeno :: Elemento
hidrogeno = Elemento "Hidrogeno" "H" 1 "Gas noble"

cloro :: Elemento
cloro = Elemento "Cloro" "Cl" 17 "halogeno"

sodio :: Elemento
sodio = Elemento "Sodio" "Na" 11 "metal"
--b. El agua, sustancia compuesta por 2 hidrógenos y 1 un oxígeno.

data Compuesto = Compuesto [Componente] String String          deriving (Show)

agua :: Compuesto
agua = Compuesto [Componente hidrogeno 2, Componente oxigeno 2] "No metal" "Agua"
sal:: Compuesto
sal = Compuesto [Componente cloro 1 , Componente sodio 1 ] "No metal" "sal"
--type Compuesto = [Componente]

-- 2) 

criterioCalor :: Compuesto -> Bool
criterioCalor (Compuesto compuesto _ _) = (any (compararMetal) compuesto) || (any (compararHalogeno) compuesto)

criterioElectricidad :: Compuesto -> Bool
criterioElectricidad  (Compuesto compuesto _ _) = (any (compararGasNoble) compuesto) || (any (compararMetal) compuesto)

conduceBien :: Compuesto -> Bool
conduceBien compuesto = criterioElectricidad compuesto || criterioCalor compuesto

compararMetal :: Componente -> Bool
compararMetal sustancia =  tipoDeSustancia (elemento sustancia) == "Metal"

compararGasNoble :: Componente -> Bool
compararGasNoble sustancia =  tipoDeSustancia (elemento sustancia) == "Gas Noble"

compararHalogeno :: Componente -> Bool
compararHalogeno sustancia =  tipoDeSustancia (elemento sustancia) == "Halogeno"

-- 3)

nombreDeUnion :: String -> String
nombreDeUnion nombre  
              |last(nombre) /= 'a' && last(nombre) /= 'e' && last(nombre) /= 'i' && last(nombre) /= 'o' && last(nombre) /= 'u' = nombre ++ "uro"
              |last(init(nombre)) == 'a' || last(init(nombre)) == 'e' || last(init(nombre)) == 'i' || last(init(nombre)) == 'o' || last(init(nombre)) == 'u'  = ( init(init(nombre)) ) ++ "uro"
			        |last(nombre) == 'a' || last(nombre) == 'e' || last(nombre) == 'i' || last(nombre) == 'o' || last(nombre) == 'u' =  ( init(nombre) ) ++ "uro"

-- 4)  

combinarNombres :: String -> String -> String 
combinarNombres elemento1 elemento2 = nombreDeUnion elemento1 ++ " de " ++ elemento2

-- 5)
mezclar :: Componente -> Componente -> Compuesto
mezclar (Componente elemento1 cantidad1) (Componente elemento2 cantidad2)   = Compuesto [(Componente elemento1 cantidad1),(Componente elemento2 cantidad2)] "No metal" (combinarNombres (nombre elemento1) (nombre elemento2))   

-- 6)

formula :: Compuesto -> String
formula (Compuesto componentes tipo nombre) = 
