import Utils
import ArvoreDecisao

main = do
       entradaDescricao <- readFile "descricao.txt"
       entradaBase <- readFile "base.txt"
       let caracteristicas = formataCaracteristica (init (formataEntrada entradaDescricao)) 0
       let exemplos = formataEntrada entradaBase
       --let d = criaListaDeValores caracteristicas exemplos
       --let melhor = calculaIgr caracteristicas exemplos d
       let arv = arvoreDecisao exemplos caracteristicas (maioria exemplos) []
       let a = imprimeArvore arv 0
       putStrLn (show a)
      



