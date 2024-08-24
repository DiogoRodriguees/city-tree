module ArvoreQuadrupla (
    ArvoreQuadrupla(..),
    inserirCidade,
    buscarCidadesProximas,
    buscarCidadePeloNome
) where

data ArvoreQuadrupla = Empty |
                        Node {
                            city:: String,
                            lat:: Double,
                            lo::Double,
                            no::ArvoreQuadrupla,
                            ne::ArvoreQuadrupla,
                            so::ArvoreQuadrupla,
                            se::ArvoreQuadrupla
                        } deriving (Show)

-- Metodo de inserir cidade
inserirCidade :: String -> Double -> Double -> ArvoreQuadrupla -> ArvoreQuadrupla
inserirCidade name latitude longitude Empty = Node name latitude longitude Empty Empty Empty Empty
inserirCidade name latitude longitude (Node cityName cityLat cityLon no ne so se)
    | latitude < cityLat && longitude < cityLon = Node cityName cityLat cityLon (inserirCidade name latitude longitude no) ne so se
    | latitude < cityLat && longitude >= cityLon = Node cityName cityLat cityLon no (inserirCidade name latitude longitude ne) so se
    | latitude >= cityLat && longitude < cityLon = Node cityName cityLat cityLon no ne (inserirCidade name latitude longitude so) se
    | otherwise = Node cityName cityLat cityLon no ne so (inserirCidade name latitude longitude se)


-- Metodo para calcular distancia entre as ecidades
calcularDistancia :: (Double, Double) -> (Double, Double) -> Double
calcularDistancia (lat1, lon1) (lat2, lon2) = sqrt ((lat1 - lat2) ** 2 + (lon1 - lon2) ** 2)

--  Busca cidades dentro do raio informado
buscarCidadesProximas::Double -> (Double, Double)  -> ArvoreQuadrupla -> [String]
buscarCidadesProximas _ _ Empty = []
buscarCidadesProximas raio (latitude, longitude) (Node nomeCidade lat lon no ne so se)
    | calcularDistancia (latitude, longitude) (lat, lon) <= raio =
        nomeCidade : (
                        buscarCidadesProximas raio (latitude, longitude) no ++ 
                        buscarCidadesProximas raio (latitude, longitude) ne ++
                        buscarCidadesProximas raio (latitude, longitude) so ++ 
                        buscarCidadesProximas raio (latitude, longitude) se
                        )
    | otherwise = 
        buscarCidadesProximas raio (latitude, longitude) no ++
        buscarCidadesProximas raio (latitude, longitude) ne ++
        buscarCidadesProximas raio (latitude, longitude) so ++
        buscarCidadesProximas raio (latitude, longitude) se

-- Dado um nome, retorna a cidade com aquele nome
buscarCidadePeloNome:: String -> ArvoreQuadrupla -> Maybe (Double, Double)
buscarCidadePeloNome _ Empty = Nothing
buscarCidadePeloNome nome (Node nomeCidade lat lon no ne so se)
    | nome == nomeCidade = Just (lat,lon)
    | otherwise = 
        case buscarCidadePeloNome nome no of 
            Just coordenadas -> Just coordenadas
            Nothing ->
                case buscarCidadePeloNome nome ne of
                    Just coordenadas -> Just coordenadas
                    Nothing ->
                        case buscarCidadePeloNome nome so of
                            Just coordenadas -> Just coordenadas
                            Nothing ->
                                case buscarCidadePeloNome nome se of
                                    Just coordenadas -> Just coordenadas
                                    Nothing -> Just (0, 0)
                        

