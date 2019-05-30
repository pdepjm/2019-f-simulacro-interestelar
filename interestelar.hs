import Text.Show.Functions

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z


data Planeta = UnPlaneta {
    posicion :: Posicion,
    dilatacionTemporal :: Tiempo -> Tiempo 
} deriving Show

data Astronauta = UnAstronauta {
  edad :: Tiempo,
  planeta :: Planeta
} deriving Show


type Tiempo = Float

--1
x109Z = UnPlaneta {
    posicion = (50,100,70),
    dilatacionTemporal = (*2)
}

laTierra = UnPlaneta {
    posicion = (0,0,0),
    dilatacionTemporal = id
}

--2
anastasia = UnAstronauta {
    edad = 22,
    planeta = laTierra
}

bart = UnAstronauta {
    edad = 11,
    planeta = x109Z
}

--3
distanciaEntreDosPlanetas :: Planeta -> Planeta -> Float
distanciaEntreDosPlanetas planeta1 planeta2 = 
    distancia (posicion planeta1) (posicion planeta2)

distancia :: Posicion -> Posicion -> Float
distancia p1 p2 = 
    sqrt . sum . map (^2) . 
    map diferencia . 
    zip (coordenadas p1) 
    $ coordenadas p2

diferencia (n1, n2) = n1 - n2
coordenadas posicion = 
    map (\funCoord -> funCoord posicion) [coordX, coordY, coordZ]

--4
pasarTiempo :: Tiempo -> Astronauta -> Astronauta
pasarTiempo anios astronauta = 
    envejecer (tiempoTerrestreEnPlaneta astronauta anios) astronauta

tiempoTerrestreEnPlaneta = dilatacionTemporal . planeta

envejecer :: Tiempo -> Astronauta -> Astronauta
envejecer anios astronauta = astronauta {
    edad = (edad astronauta) + anios
}

--5
type Nave = (Planeta, Planeta) -> Tiempo

naveVieja :: Int -> Nave
naveVieja tanquesDeOxigeno =
    tiempoDeViaje (velocidadPorTanques tanquesDeOxigeno)

velocidadPorTanques tanquesDeOxigeno
    |tanquesDeOxigeno <= 6 = 10
    |otherwise = 7

naveFuturista :: Nave
naveFuturista _ = 0

naveX :: Char -> Nave
naveX 'A' = tiempoDeViaje 15
naveX 'B' = tiempoDeViaje 11
naveX 'C' = (*2) . tiempoDeViaje 11

tiempoDeViaje :: Float -> (Planeta, Planeta) -> Tiempo
tiempoDeViaje velocidad (planeta1, planeta2) = 
    (distanciaEntreDosPlanetas planeta1 planeta2) / velocidad


viaje :: Nave -> Planeta -> Astronauta -> Astronauta
viaje nave planetaDestino astronauta = 
    (
        cambiarPlaneta planetaDestino .
        envejecer (nave (planeta astronauta, planetaDestino))
    ) astronauta

cambiarPlaneta :: Planeta -> Astronauta -> Astronauta
cambiarPlaneta nuevoPlaneta astronauta = astronauta {
    planeta = nuevoPlaneta
}

--6
rescatar :: [Astronauta] -> Nave -> Astronauta -> [Astronauta]
rescatar rescatistas nave = 
    volver nave rescatistas . irABuscar rescatistas nave

irABuscar :: [Astronauta] -> Nave -> Astronauta -> [Astronauta]
irABuscar rescatistas nave varade =
    (incorporar varadeVieje . viajarMuchos nave planetaDestino) rescatistas
    where
        varadeVieje = pasarTiempo tiempoDeViaje varade
        planetaDestino = planeta varade
        planetaOrigen = origen rescatistas
        tiempoDeViaje = nave (planetaOrigen, planetaDestino)


volver :: Nave -> [Astronauta] -> [Astronauta] -> [Astronauta]
volver nave rescatistas = viajarMuchos nave (origen rescatistas)

viajarMuchos :: Nave -> Planeta -> [Astronauta] -> [Astronauta]
viajarMuchos nave destino astronautas = map (viaje nave destino) astronautas

incorporar :: Astronauta -> [Astronauta] -> [Astronauta]
incorporar = (:)

origen = planeta . head

--7
puedenSerRescatados :: [Astronauta] -> Nave -> [Astronauta] -> [Astronauta]
puedenSerRescatados rescatistas nave = filter (puedeSerRescatadoPor rescatistas nave)

puedeSerRescatadoPor :: [Astronauta] -> Nave -> Astronauta -> Bool
puedeSerRescatadoPor rescatistas nave = all (not . esViejo) . rescatar rescatistas nave

esViejo :: Astronauta -> Bool
esViejo = (>90) . edad

--8
-- ¿Qué cambios habría que hacer (en qué lugares del código impactaría) si se quisiera agregar un nombre a cada astronauta? ¿Y si quisiera llevar un registro de los planetas en los que estuvo?

-- Para el nombre solamente se tiene que agregar en la definición del data. Gracias a que nunca hacemos patetrn matching del mismo.
-- Para llevar un registro de los planetas, se podría agregar una lista en el astronauta y modificar la primitiva cambiarPlaneta para que además agregue el nuevo planeta a la lista.



-- ¿Qué debería hacer para agregar una nueva nave? ¿Cambiaría algo de las funciones que las usan? ¿Por qué?

-- Se debería agregar una definición para la nave, pudiendo construir el tipo de la misma. No habría que tocar nada más ya que las funciones que las usan son funciones de orden superior que trabajan para cualquier función de dicho tipo.



--9
rescateGrupal :: [Astronauta] -> Nave -> [Astronauta] -> [Astronauta]
rescateGrupal rescatistas nave =
    volver nave rescatistas . rescatarEnSerie rescatistas nave 


rescatarEnSerie :: [Astronauta] -> Nave -> [Astronauta] -> [Astronauta]
rescatarEnSerie rescatistas nave = foldl (flip irABuscar nave) rescatistas 