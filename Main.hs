import Control.Monad (forever, when, mapM_)
import ArvoreQuadrupla
import System.IO
import App

main :: IO ()
main = do
    -- Curitiba
    --     Latitude: -25.4284
    --     Longitude: -49.2733

    -- Londrina
    --     Latitude: -23.3045
    --     Longitude: -51.1696

    -- Maringa
    --     Latitude: -23.4208
    --     Longitude: -51.9333

    -- Cascavel
    --     Latitude: -24.9555
    --     Longitude: -53.4552

    -- Ponta Grossa
    --     Latitude: -25.0916
    --     Longitude: -50.1666
    
    -- Criando a arvore base da aplicação: inciando ela vazia
    let arvoreQuadrupla = Empty
    run arvoreQuadrupla -- executando o loop da aplicação