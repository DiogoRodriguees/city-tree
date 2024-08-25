import Control.Monad (forever, when, mapM_)
import ArvoreQuadrupla
import App

main :: IO ()
main = do
    -- ("Louisville", 38.2527, -85.7585),
    -- ("Washington", 38.9072, -77.0369),
    -- ("Pittsburgh", 40.4406, -79.9959),
    -- ("New York", 40.7128, -74.0060)
    
    -- Criando a arvore base da aplicação: inciando ela vazia
    let arvoreQuadrupla = Empty
    run arvoreQuadrupla -- executando o loop da aplicação