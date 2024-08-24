import Control.Monad (mapM_)
import ArvoreQuadrupla


main :: IO ()
main = do
    let cidades = [("Louisville", 38.2527, -85.7585),
                   ("Washington", 38.9072, -77.0369),
                --    ("Pittsburgh", 40.4406, -79.9959),
                   ("New York", 40.7128, -74.0060)]

    let arvoreQuadrupla = foldr(\(name, latitude, longitude) node -> inserirCidade name latitude longitude node) Empty cidades

    -- Menu do sistema
    putStrLn "Escolha uma opção:"
    putStrLn "1. Inserir nova cidade"
    putStrLn "2. Buscar cidades próximas"
    putStrLn "3. Buscar cidade pelo nome"
    putStrLn "4. Imprimir a árvore"
    putStrLn "5. Sair"
    putStrLn "Digite a opção: "
    opcao <- getLine -- leitura do teclado

    -- check option choose
    case opcao of
        "1" -> do
            putStrLn "Digite o nome da cidade: "
            nome <- getLine
            putStrLn "Digite a latitude: "
            latStr <- getLine
            putStrLn "Digite a longitude: "
            lonStr <- getLine
            let latitude = read latStr :: Double
            let longitude = read lonStr :: Double
            let arvoreAtualizada = inserirCidade nome latitude longitude arvoreQuadrupla
            putStrLn "Cidade inserida com sucesso!"

        "2" -> do
            putStrLn "Digite o nome da cidade para buscar proximidade: "
            nomeCidade <- getLine
            putStrLn "Digite a distância (em km): "
            distStr <- getLine
            let distancia = read distStr :: Double
            case buscarCidadePeloNome nomeCidade arvoreQuadrupla of
                Just (lat, lon) -> do
                    let cidadesEncontradas = buscarCidadesProximas distancia (lat, lon) arvoreQuadrupla
                    putStrLn $ "Cidades dentro de " ++ show distancia ++ " km de " ++ nomeCidade ++ ":"
                    mapM_ (\cidade -> putStrLn $ " - " ++ cidade) cidadesEncontradas
                Nothing -> putStrLn "Cidade não encontrada."

        "3" -> do
            putStr "Digite o nome da cidade: "
            nomeCidade <- getLine
            case buscarCidadePeloNome nomeCidade arvoreQuadrupla of
                Just (lat, lon) -> putStrLn $ "Cidade encontrada: " ++ nomeCidade ++ " (" ++ show lat ++ ", " ++ show lon ++ ")"
                Nothing -> putStrLn "Cidade não encontrada."

        "4" -> do
            print arvoreQuadrupla

        _ -> putStrLn "Opção inválida."