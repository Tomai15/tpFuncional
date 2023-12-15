import Data.List

--1) Modelar las siguientes sustancias
--a. El hidrógeno y el oxígeno

data Componente = Componente Sustancia Int deriving (Show)
data Sustancia= 
              Elemento{
                nombre 				  :: String, 
                simboloQuimico  :: String, 
                numeroAtomico   :: Int,
                tipoDeSustancia :: String

              } 
              |Compuesto{
                componente :: [Componente],
                tipo :: String,
                nombre :: String
              } 
  deriving (Show)

oxigeno :: Sustancia
oxigeno = Elemento "Oxigeno" "O" 8 "No Metal"

{-
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
-}

hidrogeno :: Sustancia
hidrogeno = Elemento "Hidrogeno" "H" 1 "gas noble"

cloro :: Sustancia
cloro = Elemento "Cloro" "Cl" 17 "halogeno"

sodio :: Sustancia
sodio = Elemento "Sodio" "Na" 11 "metal"
--b. El agua, sustancia compuesta por 2 hidrógenos y 1 un oxígeno.


agua :: Sustancia
agua = Compuesto [Componente hidrogeno 2, Componente oxigeno 1] "No metal" "agua"
sal:: Sustancia
sal = Compuesto [Componente cloro 1 , Componente sodio 1] "No metal" "sal"


-- 2) 

conduceBien :: Sustancia -> Bool
conduceBien (Compuesto _ tipo _)  =  (tipo == "gas noble" || tipo == "metal") || (tipo == "halogeno" || tipo == "metal")
conduceBien (Elemento _ _ _ tipo) = (tipo == "gas noble" || tipo == "metal" ) || (tipo == "halogeno" || tipo == "metal")

-- 3)

nombreDeUnion :: String -> String
nombreDeUnion nombre  
              |last(nombre) /= 'a' && last(nombre) /= 'e' && last(nombre) /= 'i' && last(nombre) /= 'o' && last(nombre) /= 'u' = nombre ++ "uro"
              |last(init(nombre)) == 'a' || last(init(nombre)) == 'e' || last(init(nombre)) == 'i' || last(init(nombre)) == 'o' || last(init(nombre)) == 'u'  = ( init(init(nombre)) ) ++ "uro"
			        |last(nombre) == 'a' || last(nombre) == 'e' || last(nombre) == 'i' || last(nombre) == 'o' || last(nombre) == 'u' =  ( init(nombre) ) ++ "uro"

-- 4)  

combinarNombres :: String -> String -> String 
combinarNombres elemento1 elemento2 = (nombreDeUnion elemento1) ++ " de " ++ elemento2
-- 5)


mezclar :: Componente -> Componente -> Sustancia
mezclar (Componente element1 cantidad1) (Componente element2 cantidad2)   = Compuesto [(Componente element1 cantidad1),(Componente element2 cantidad2)] "No metal" (combinarNombres (nombre element1) (nombre element2))
-- 6)

formula :: Sustancia -> String
formula (Elemento _ simboloQuimico _ _) = simboloQuimico
formula (Compuesto lista _ _) = "(" ++ concat (map  buscarSimbolo  lista) ++ ")"

buscarSimbolo :: Componente -> String
buscarSimbolo (Componente elemento cantidad)  
                                            | cantidad> 1 = simboloQuimico elemento ++ show cantidad
                                            | cantidad == 1 = simboloQuimico elemento
