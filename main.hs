import Utils
import ArvoreDecisao

main = do
       entradaDescricao <- readFile "descricao.txt"
       entradaBase <- readFile "base.txt"
       let caracteristicas = formataCaracteristica (init (formataEntrada entradaDescricao)) 0
       let exemplos = formataEntrada entradaBase
       let d = criaListaDeValores caracteristicas exemplos
       let melhor = melhorTeste caracteristicas exemplos d
       putStrLn (show melhor)
      



