import Data.Ratio
import Data.Bool
import Data.String

-- estruturas de dados
data Quadtree q = 
    Quadtree 
    { 
        size :: Int,
        root :: !(Node q)
    }
    deriving (Eq, Show)

data Node q = 
    Node 
    { 
        name :: String,
        latitude :: Double,
        longitude :: Double,
        northWest :: !(Node q),
        northEast :: !(Node q),
        southWest :: !(Node q),
        southEast :: !(Node q)
    }
    | Empty
    deriving (Eq, Show)

-- funções de adicionar Nós ao Quadtree

createNode :: String -> Double -> Double -> Node q
createNode city lat long = Node 
    { name = city,
      latitude = lat,
      longitude = long,
      northWest = Empty,
      northEast = Empty,
      southWest = Empty,
      southEast = Empty
    }

createQuadtree :: Int -> Node q -> Quadtree q
createQuadtree num myNode = Quadtree {size = num, root = myNode}

newQuadtree :: String -> Double -> Double -> Quadtree q -- cria uma Quadtree com Nó raiz
newQuadtree city lat long = createQuadtree 1 (createNode city lat long)

newCityNode :: String -> Double -> Double -> Node q -> Node q
newCityNode city lat long myNode
    | latitude myNode >= lat && longitude myNode >= long =
        if northWest myNode == Empty
            then myNode { northWest = createNode city lat long }
            else myNode { northWest = newCityNode city lat long (northWest myNode) }
    | latitude myNode >= lat =
        if northEast myNode == Empty
            then myNode { northEast = createNode city lat long }
            else myNode { northEast = newCityNode city lat long (northEast myNode) }
    | longitude myNode >= long =
        if southWest myNode == Empty
            then myNode { southWest = createNode city lat long }
            else myNode { southWest = newCityNode city lat long (southWest myNode) }
    | otherwise =
        if southEast myNode == Empty
            then myNode { southEast = createNode city lat long }
            else myNode { southEast = newCityNode city lat long (southEast myNode) }

insertNode :: String -> Double -> Double -> Quadtree q -> Quadtree q
insertNode city lat long myQuadtree = 
    createQuadtree ((size myQuadtree)+1) (newCityNode city lat long (root myQuadtree))

-- fórmulas para calcular distâncias entre coordenadas

-- converte graus para radianos

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * pi / 180

-- fórmula Haversine

haversine :: Double -> Double -> Double -> Double -> Double
haversine lat1 lon1 lat2 lon2 = 
    let earthRadius = 6371000 -- raio da Terra em metros
        dlat = degreesToRadians (lat2 - lat1)
        dlon = degreesToRadians (lon2 - lon1)
        a = sin (dlat / 2) ** 2 + cos (degreesToRadians lat1) * cos (degreesToRadians lat2) * sin (dlon / 2) ** 2
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    in earthRadius * c

-- funções para encontrar todas a cidades dentro de uma distância n de uma coordenada

radiusCheck :: Double -> Double -> Double -> Node q -> [String]
radiusCheck lat long n myNode =
    if ((haversine lat long (latitude myNode) (longitude myNode)) <= n)
        then [name myNode]
        else []

parseNode :: Int -> Double -> Double -> Double -> Node q -> [String]
parseNode i lat long n myNode
    | i == 0 = parseQuadtree lat long n (northWest myNode)
    | i == 1 = parseQuadtree lat long n (northEast myNode)
    | i == 2 = parseQuadtree lat long n (southWest myNode)
    | otherwise = parseQuadtree lat long n (southEast myNode)


parseQuadtree :: Double -> Double -> Double -> Node q -> [String]
parseQuadtree lat long n myNode =
    if myNode == Empty
        then []
        else (radiusCheck lat long n myNode) ++ (concatMap (\i -> parseNode i lat long n myNode) [0..3])

findCitiesInRadius :: Double -> Double -> Double -> Quadtree q -> [String]
findCitiesInRadius lat long n myQuadtree = parseQuadtree lat long n (root myQuadtree)

-- funções para encontrar todas a cidades dentro de uma distância n de uma cidade

spreadOut :: String -> Node q -> Node q -> Node q -> Node q -> Node q
spreadOut city northWest southWest northEast southEast=
    let results = filter (/= Empty) [checkName city northWest, checkName city southWest, checkName city northEast, checkName city southEast]
    in if null(results)
          then Empty
          else head results

checkName :: String -> Node q -> Node q
checkName city myNode
    | myNode == Empty = Empty
    | (name myNode) == city = myNode
    | otherwise = spreadOut city (northWest myNode) (southWest myNode) (northEast myNode) (southEast myNode)

findCitiesInRadiusWithNode :: Double -> Node q -> Quadtree q -> [String]
findCitiesInRadiusWithNode n myNode myQuadtree = findCitiesInRadius (latitude myNode) (longitude myNode) n myQuadtree

findCitiesInRadiusOfCity :: String -> Double -> Quadtree q -> [String]
findCitiesInRadiusOfCity city n myQuadtree =
    case checkName city (root myQuadtree) of
        Empty -> ["Cidade não encontrada"]
        node  -> findCitiesInRadiusWithNode n node myQuadtree

theQuadtree = newQuadtree "Campo Mourão" (-24.046329) (-52.37802)
theQuadtree2 = insertNode "Peabiru" (-23.9142123) (-52.3455598) theQuadtree
theQuadtree3 = insertNode "Maringá" (-23.425269) (-51.9382078) theQuadtree2
theQuadtree4 = insertNode "Araruna" (-23.9320308) (-52.4976047) theQuadtree3
theQuadtree5 = insertNode "Engenheiro Beltrão" (-23.7996003) (-52.2592684) theQuadtree4
theQuadtree6 = insertNode "Ponta Grossa" (-25.0891685) (-50.1601812) theQuadtree5
theQuadtree7 = insertNode "Curitiba" (-25.4295963) (-49.2712724) theQuadtree6
theQuadtree8 = insertNode "Londrina" (-23.3112878) (-51.1595023) theQuadtree7
theQuadtree9 = insertNode "Cascavel" (-24.9554996) (-53.4560544) theQuadtree8
theQuadtree10 = insertNode "Foz do Iguaçu" (-25.5304023) (-54.5830692) theQuadtree9
theQuadtree11 = insertNode "Rolândia" (-23.3119901) (-51.3674145) theQuadtree10
theQuadtree12 = insertNode "Arapongas" (-23.4152862) (-51.4293961) theQuadtree11
theQuadtree13 = insertNode "Uberlândia" (-18.9188041) (-48.2767837) theQuadtree12
theQuadtree14 = insertNode "Varginha" (-21.5565914) (-45.4340674) theQuadtree13
theQuadtree15 = insertNode "Ivailândia" (-23.7221801) (-52.1785725) theQuadtree14
theQuadtree16 = insertNode "Blumenau" (-26.9195567) (-49.0658025) theQuadtree15
theQuadtree17 = insertNode "Dois Vizinhos" (-25.7464681) (-53.0554369) theQuadtree16
theQuadtree18 = insertNode "Pato Branco" (-26.2295984) (-52.6712474) theQuadtree17
theQuadtree19 = insertNode "Santa Helena" (-24.8591735) (-54.3328813) theQuadtree18
theQuadtree20 = insertNode "Umuarama" (-23.7621152) (-53.3116192) theQuadtree19

main :: IO ()
main = do (terminalInput theQuadtree20 True)

terminalInput :: Quadtree q -> Bool -> IO ()
terminalInput quadtree continue
    | continue == True = do
        putStrLn "\nEscolha uma função:"
        putStrLn "1. Encontrar todas cidades em distancia n (em metros) de uma Cidade."
        putStrLn "2. Encontrar todas cidades em distancia n (em metros) de uma dada Coordenada."
        putStrLn "3. Adicione nova cidade."
        putStrLn "4. Sair do programa."
        choice <- getLine

        case choice of
            "1" -> do
                putStrLn "Informe o nome da cidade:"
                city <- getLine
                putStrLn "Informe a distância:"
                radiusInput <- getLine
                let radius = read radiusInput :: Double
                let result = findCitiesInRadiusOfCity city radius quadtree
                putStrLn "Cidades Encontradas:"
                mapM_ putStrLn result
                terminalInput quadtree True

            "2" -> do
                putStrLn "Informe a latitude:"
                latInput <- getLine
                putStrLn "Informe a longitude:"
                longInput <- getLine
                putStrLn "Informe a distância:"
                radiusInput <- getLine
                let lat = read latInput :: Double
                let long = read longInput :: Double
                let radius = read radiusInput :: Double
                let result = findCitiesInRadius lat long radius quadtree
                putStrLn "Cidades Encontradas:"
                mapM_ putStrLn result
                terminalInput quadtree True

            "3" -> do
                putStrLn "Informe o nome da cidade:"
                city <- getLine
                putStrLn "Informe a latitude:"
                latInput <- getLine
                putStrLn "Informe a longitude:"
                longInput <- getLine
                let lat = read latInput :: Double
                let long = read longInput :: Double
                let auxQuadtree = insertNode city lat long quadtree
                terminalInput auxQuadtree True

            "4" -> do
                terminalInput quadtree False

            _ -> putStrLn "Escolha a opção 1, 2, 3 ou 4."
    | otherwise = putStrLn "\nFim do programa."