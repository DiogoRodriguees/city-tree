module App (run) where

import Control.Monad (forever, when, mapM_)
import ArvoreQuadrupla

mostraArvore:: ArvoreQuadrupla -> IO ()
mostraArvore arvoreQuadrupla = do print arvoreQuadrupla 

executeBuscarPeloNome :: ArvoreQuadrupla -> IO ()
executeBuscarPeloNome arvoreQuadrupla = do
    putStrLn "Digite o nome da cidade: "
    nomeCidade <- getLine
    putStrLn "Digite o raio de distancia da cidade: "
    raio <- getLine

    case buscarCidadePeloNome nomeCidade arvoreQuadrupla of
        Just (lat, lon) -> do
            let distancia = read raio :: Double -- Casting do raio de String para Double
            case buscarCidadesProximas distancia (lat,  lon) arvoreQuadrupla of
                [] -> putStrLn "Cidade não encontrada."
                cidadesEncontradas -> do
                    -- show() mostra a distancia como String
                    putStrLn $ "Cidades dentro de " ++ show distancia ++ " km de " ++ nomeCidade ++ ":"
                    
                    -- Map(callBack, lista) aplica a função entre () para a lista de cidadesEncontradas
                    -- "\" define que sera passado os parametros da função
                    -- "->" indica o inicio da função 
                    mapM_ (\cidade -> putStrLn $ " - " ++ cidade) cidadesEncontradas

        Nothing -> putStrLn "Cidade não encontrada."

executeInserirCidade :: ArvoreQuadrupla -> IO ArvoreQuadrupla
executeInserirCidade arvoreQuadrupla = do
    putStrLn "Digite o nome da cidade: "
    nome <- getLine
    putStrLn "Digite a latitude: "
    latStr <- getLine
    putStrLn "Digite a longitude: "
    lonStr <- getLine

    let latitude = read latStr :: Double -- Casting da latStr de String para Double
    let longitude = read lonStr :: Double -- Casting da lonStr de String para Double
    let arvoreAtualizada = inserirCidade nome latitude longitude arvoreQuadrupla
    
    putStrLn "Cidade inserida com sucesso!"
    -- retornando arvoreAtualizada como IO
    return arvoreAtualizada

executeBuscarCidadesProximas :: ArvoreQuadrupla -> IO ()
executeBuscarCidadesProximas arvoreQuadrupla = do
    -- Lendo os parametros
    putStrLn "Digite a latitude da cidade: "
    latitude <- getLine
    putStrLn "Digite a longitudde da cidade: "
    longitude <- getLine
    putStrLn "Digite a distância (em km): "
    distStr <- getLine

    let distancia = read distStr :: Double -- Casting da distancia de String para Double
    let lat = read latitude::Double -- Casting da latitude de String para Double
    let lon = read longitude::Double -- Casting da longitude de String para Double

    case buscarCidadesProximas distancia (lat, lon) arvoreQuadrupla of
        [] -> putStrLn "Cidade não encontrada."
        cidadesEncontradas -> do
            -- show() mostra a distancia como String
            putStrLn $ "Cidades dentro de " ++ show distancia ++ " km " ++ ":"
            -- Map(callBack, lista) aplica a função entre () para a lista de cidadesEncontradas
            -- "\" define que sera passado os parametros da função
            -- "->" indica o inicio da função 
            mapM_ (\cidade -> putStrLn $ " - " ++ cidade) cidadesEncontradas


-- Função pra para o loop run()
sair :: IO ()
sair = do error "Saindo ..."

mostrarMenu :: IO ()
mostrarMenu = do 
    putStrLn "Escolha uma opção:"
    putStrLn "1. Inserir nova cidade"
    putStrLn "2. Buscar cidades próximas"
    putStrLn "3. Buscar cidade pelo nome"
    putStrLn "4. Imprimir a árvore"
    putStrLn "5. Sair"
    putStrLn "Digite a opção: "

run :: ArvoreQuadrupla -> IO ()
run arvoreQuadrupla = do
    mostrarMenu -- exibindo menu de opções do sistema
    opcao <- getLine -- lendo opção do menu (Teclado)

    case opcao of
        "1" -> do
            arvoreAtualizada <- executeInserirCidade arvoreQuadrupla
            run arvoreAtualizada

        "2" -> do 
            executeBuscarCidadesProximas arvoreQuadrupla
            run arvoreQuadrupla
        
        "3" -> do 
            executeBuscarPeloNome arvoreQuadrupla
            run arvoreQuadrupla

        "4" -> do 
            mostraArvore arvoreQuadrupla
            run arvoreQuadrupla

        "5" -> sair

        _ -> do 
            putStrLn "Opção inválida."
            run arvoreQuadrupla
