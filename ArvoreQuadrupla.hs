module ArvoreQuadrupla (
    ArvoreQuadrupla(..),
    inserirCidade,
    buscarCidadesProximas,
    buscarCidadePeloNome,
    imprimirArvore
) where
import Data.List (foldl')

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

imprimirArvore :: ArvoreQuadrupla -> IO ()
imprimirArvore Empty = return ()
imprimirArvore (Node city lat lon no ne so se) = do
    putStrLn $ "Cidade: " ++ city
    putStrLn $ "- NO: " ++ nomeNo no
    putStrLn $ "- NE: " ++ nomeNo ne
    putStrLn $ "- SO: " ++ nomeNo so
    putStrLn $ "- SE: " ++ nomeNo se
    putStrLn ""  -- Linha em branco para separar os nós
    imprimirArvore no  -- Percorre a subárvore Noroeste
    imprimirArvore ne  -- Percorre a subárvore Nordeste
    imprimirArvore so  -- Percorre a subárvore Sudoeste
    imprimirArvore se  -- Percorre a subárvore Sudeste

-- Função auxiliar para obter o nome da cidade ou "Empty" se o nó estiver vazio
nomeNo :: ArvoreQuadrupla -> String
nomeNo Empty = "Empty"
nomeNo (Node city _ _ _ _ _ _) = city

-- Metodo de inserir cidade
inserirCidade :: String -> Double -> Double -> ArvoreQuadrupla -> ArvoreQuadrupla
inserirCidade name latitude longitude Empty = Node name latitude longitude Empty Empty Empty Empty
inserirCidade name latitude longitude (Node cityName cityLat cityLon no ne so se)
    | latitude > cityLat && longitude < cityLon = Node cityName cityLat cityLon (inserirCidade name latitude longitude no) ne so se
    | latitude > cityLat && longitude >= cityLon = Node cityName cityLat cityLon no (inserirCidade name latitude longitude ne) so se
    | latitude <= cityLat && longitude < cityLon = Node cityName cityLat cityLon no ne (inserirCidade name latitude longitude so) se
    | otherwise = Node cityName cityLat cityLon no ne so (inserirCidade name latitude longitude se)


-- Metodo para calcular distancia entre as ecidades
-- calcularDistancia :: (Double, Double) -> (Double, Double) -> Double
-- calcularDistancia (lat1, lon1) (lat2, lon2) = sqrt ((lat1 - lat2) ** 2 + (lon1 - lon2) ** 2)


-- Raio da Terra em quilômetros
raioTerra :: Double
raioTerra = 6371.0

-- Calcula a distância entre dois pontos (lat1, lon1) e (lat2, lon2) em quilômetros
calcularDistancia :: (Double, Double) -> (Double, Double) -> Double
calcularDistancia (lat1, lon1) (lat2, lon2) =
    let
        -- Converte graus para radianos
        rad x = x * pi / 180.0
        lat1' = rad lat1
        lon1' = rad lon1
        lat2' = rad lat2
        lon2' = rad lon2

        -- Diferenças das coordenadas
        dlat = lat2' - lat1'
        dlon = lon2' - lon1'

        -- Fórmula de Haversine
        a = sin(dlat / 2) ** 2 + cos(lat1') * cos(lat2') * sin(dlon / 2) ** 2
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    in
        raioTerra * c

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
                        

