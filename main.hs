import Utils
import ArvoreDecisao

main = do
       entradaDescricao <- readFile "descricao.txt"
       entradaBase <- readFile "base.txt"
       entradaCaso <- readFile "caso.txt"
       let caso = head (formataEntrada entradaCaso)
       let caracteristicas = formataCaracteristica (init (formataEntrada entradaDescricao)) 0
       let exemplos = formataEntrada entradaBase
       let arv = arvoreDecisao exemplos caracteristicas (maioria exemplos) []
       let a = imprime' (filhosDoNo arv) arv "" ""
       let resp = avaliaCaso arv caracteristicas caso
       resultado <- writeFile "result.txt" (resp)
       arvore <- writeFile "arvore.txt" (a)
       return ()
      



