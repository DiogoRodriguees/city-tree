import Control.Monad (forever, when, mapM_)
import ArvoreQuadrupla
import App

main :: IO ()
main = do
    let cidades = []
    -- ("Louisville", 38.2527, -85.7585),
    -- ("Washington", 38.9072, -77.0369),
    -- ("Pittsburgh", 40.4406, -79.9959),
    -- ("New York", 40.7128, -74.0060)

    let arvoreQuadrupla = foldr(\(name, latitude, longitude) node -> inserirCidade name latitude longitude node) Empty cidades
    run arvoreQuadrupla