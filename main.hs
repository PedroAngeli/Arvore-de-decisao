import Data.List

main = do
       entradaDescricao <- readFile "descricao.txt"
       entradaBase <- readFile "base.txt"
       let caracteristicas = formataEntrada entradaDescricao
       let exemplos = formataEntrada entradaBase
       putStrLn "ola"

-- Recebe a entrada e formata em uma lista de listas, onde cada lista 
-- é uma linha e cada elemento dessa lista é uma string que foi separada
-- por espaço.
formataEntrada entrada = map words (lines entrada)

-- Recebe uma base de exemplos (lista de lista de exemplos) 
-- e retorna uma lista ordenada de classes presentes na base.
separaClasses exemplos = sort [last x | x<-exemplos]


-- Função auxiliar de "maioria"
maioria' [] count ant maisCont maior = if count > maior then ant
                                       else maisCont
maioria' (x:xs) count ant maisCont maior = if x == ant then maioria' xs (count+1) x maisCont maior
                                           else if count > maior then maioria' xs 1 x ant count
                                           else maioria' xs 1 x maisCont maior

-- Recebe uma base de exemplos (lista de lista de exemplos) e
-- retorna uma string que representa a classe que mais aparece
maioria exemplos = maioria' classes 1 [] (head classes) 1
                   where classes = separaClasses exemplos