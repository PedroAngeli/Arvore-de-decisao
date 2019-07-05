import Utils
import ArvoreDecisao

main = do
       entradaDescricao <- readFile "descricao.txt"
       entradaBase <- readFile "base.txt"
       let caracteristicas = formataCaracteristica (init (formataEntrada entradaDescricao)) 0
       let exemplos = formataEntrada entradaBase
       let d = criaListaDeValores caracteristicas exemplos
       let ig = ig' (d !! 2) exemplos 2
       let e = entropia [["Sol","22","70","NaoVa"],["Sol","22","70","NaoVa"]]
       putStrLn (show e)
       putStrLn (show d)
       putStrLn (show ig)



