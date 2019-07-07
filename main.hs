import Utils
import ArvoreDecisao

main = do
       entradaDescricao <- readFile "descricao.txt"
       entradaBase <- readFile "base.txt"
       let caracteristicas = formataCaracteristica (init (formataEntrada entradaDescricao)) 0
       let exemplos = formataEntrada entradaBase
       let d = criaListaDeValores caracteristicas exemplos
       let ig' = ig (d !! 3) exemplos 3 
       let iv' = iv (d !! 3) exemplos 3 
       putStrLn (show ig')
       putStrLn (show iv')



