
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

inserirCidade :: String -> Double -> Double -> ArvoreQuadrupla -> ArvoreQuadrupla
inserirCidade name latitude longitude Empty = Node name latitude longitude Empty Empty Empty Empty
inserirCidade name latitude longitude (Node cityName cityLat cityLon no ne so se)
    | latitude < cityLat && longitude < cityLon = Node cityName cityLat cityLon (inserirCidade name latitude longitude no) ne so se
    | latitude < cityLat && longitude >= cityLon = Node cityName cityLat cityLon no (inserirCidade name latitude longitude ne) so se
    | latitude >= cityLat && longitude < cityLon = Node cityName cityLat cityLon no ne (inserirCidade name latitude longitude so) se
    | otherwise = Node cityName cityLat cityLon no ne so (inserirCidade name latitude longitude se)

printArvoreFormatada:: ArvoreQuadrupla -> IO ()
printArvoreFormatada Empty = return ()
printArvoreFormatada (Node city lat lon no ne so se) = do
    putStrLn $ city
    printNodeFormatado "NO" no
    printNodeFormatado "NE" ne
    printNodeFormatado "SO" so
    printNodeFormatado "SE" se

printNodeFormatado:: String -> ArvoreQuadrupla -> IO ()
printNodeFormatado node Empty = do
    return ()

printNodeFormatado node arvoreQuadrupla = do
    case arvoreQuadrupla of
        Node cidade _ _ _ _ _ _ -> do   
            putStrLn $ " " ++ node ++ ": " ++ cidade
            printArvoreFormatada arvoreQuadrupla
        Empty -> do 
            return () 

getChild:: String -> ArvoreQuadrupla -> ArvoreQuadrupla
getChild "NO" (Node _ _ _ no _ _ _) = no
getChild "NE" (Node _ _ _ _ ne _ _) = ne
getChild "SO" (Node _ _ _ _ _ so _) = so
getChild "SE" (Node _ _ _ _ _ _ se) = se
getChild _ Empty = Empty

calcularDistancia :: (Double, Double) -> (Double, Double) -> Double
calcularDistancia (lat1, lon1) (lat2, lon2) = sqrt ((lat1 - lat2) ** 2 + (lon1 - lon2) ** 2)

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

                        


main :: IO ()
main = do
    let cidades = [("Louisville", 38.2527, -85.7585),
                   ("Washington", 38.9072, -77.0369),
                   ("Pittsburgh", 40.4406, -79.9959),
                   ("New York", 40.7128, -74.0060)]

    let arvoreQuadrupla = foldr(\(name, latitude, longitude) node -> inserirCidade name latitude longitude node) Empty cidades
    printArvoreFormatada arvoreQuadrupla
    