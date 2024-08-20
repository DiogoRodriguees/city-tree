data QuadTree = Empty
              | Node {
                  city :: String,
                  lat :: Double,
                  lon :: Double,
                  no :: QuadTree, -- Noroeste (NO)
                  ne :: QuadTree, -- Nordeste (NE)
                  so :: QuadTree, -- Sudoeste (SO)
                  se :: QuadTree  -- Sudeste (SE)
              } deriving (Show)

-- Função para inserir uma cidade na árvore quádrupla
insertCity :: String -> Double -> Double -> QuadTree -> QuadTree
insertCity name latitude longitude Empty = 
    Node name latitude longitude Empty Empty Empty Empty
insertCity name latitude longitude (Node cityName cityLat cityLon no ne so se)
    | latitude < cityLat && longitude < cityLon = 
        Node cityName cityLat cityLon (insertCity name latitude longitude no) ne so se
    | latitude < cityLat && longitude >= cityLon = 
        Node cityName cityLat cityLon no (insertCity name latitude longitude ne) so se
    | latitude >= cityLat && longitude < cityLon = 
        Node cityName cityLat cityLon no ne (insertCity name latitude longitude so) se
    | otherwise = 
        Node cityName cityLat cityLon no ne so (insertCity name latitude longitude se)

-- Função para calcular a distância entre dois pontos (latitude, longitude)
distance :: (Double, Double) -> (Double, Double) -> Double
distance (lat1, lon1) (lat2, lon2) =
    sqrt ((lat1 - lat2) ** 2 + (lon1 - lon2) ** 2)

-- Função para encontrar cidades dentro de uma distância d de uma localização
citiesWithinDistance :: Double -> (Double, Double) -> QuadTree -> [String]
citiesWithinDistance _ _ Empty = []
citiesWithinDistance d (latitude, longitude) (Node cityName cityLat cityLon no ne so se)
    | distance (latitude, longitude) (cityLat, cityLon) <= d = 
        cityName : (citiesWithinDistance d (latitude, longitude) no ++
                    citiesWithinDistance d (latitude, longitude) ne ++
                    citiesWithinDistance d (latitude, longitude) so ++
                    citiesWithinDistance d (latitude, longitude) se)
    | otherwise = 
        citiesWithinDistance d (latitude, longitude) no ++
        citiesWithinDistance d (latitude, longitude) ne ++
        citiesWithinDistance d (latitude, longitude) so ++
        citiesWithinDistance d (latitude, longitude) se

-- Exemplo de uso
main :: IO ()
main = do
    let cities = [("Louisville", 38.2527, -85.7585),
                  ("Pittsburgh", 40.4406, -79.9959),
                  ("Washington", 38.9072, -77.0369),
                  ("New York", 40.7128, -74.0060)]
    let quadTree = foldr (\(name, lat, lon) qt -> insertCity name lat lon qt) Empty cities
    print quadTree
    let nearbyCities = citiesWithinDistance 5 (38.2527, -85.7585) quadTree
    print nearbyCities
