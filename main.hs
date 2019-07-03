import Data.List

data Arv = Folha String String | No String String [Arv] deriving Show

main = do
       entradaDescricao <- readFile "descricao.txt"
       entradaBase <- readFile "base.txt"
       let caracteristicas = formataEntrada entradaDescricao
       let exemplos = formataEntrada entradaBase
       let classes = sort (separaClasses exemplos)
       let p = porcentagens (group classes) (fromIntegral (length classes))
       putStrLn (show (group classes))
       putStrLn (show p)

-- Recebe a entrada e formata em uma lista de listas, onde cada lista 
-- é uma linha e cada elemento dessa lista é uma string que foi separada
-- por espaço.
formataEntrada entrada = map words (lines entrada)

-- Recebe uma base de exemplos (lista de lista de exemplos) 
-- e retorna uma lista de classes presentes na base.
separaClasses exemplos = [last x | x<-exemplos]


-- Função auxiliar de "maioria"
maioria' [] count ant maisCont maior = if count > maior then ant
                                       else maisCont
maioria' (x:xs) count ant maisCont maior = if x == ant then maioria' xs (count+1) x maisCont maior
                                           else if count > maior then maioria' xs 1 x ant count
                                           else maioria' xs 1 x maisCont maior

-- Recebe uma base de exemplos (lista de lista de exemplos) e
-- retorna uma string que representa a classe que mais aparece
maioria exemplos = maioria' classes 1 [] (head classes) 1
                   where classes = sort (separaClasses exemplos)


-- Função auxiliar de mesmaClassficacao
mesmaClassificacao' [] _ = True
mesmaClassificacao' (x:xs) ant | x == ant = mesmaClassificacao' xs x
                               | otherwise = False


-- Recebe uma base de exemplos (lista de lista de exemplos) e
-- retorna um booleano indicando se as classes são iguais para todos 
-- os exemplos
mesmaClassificacao exemplos = mesmaClassificacao' classes (head classes)
                              where classes = separaClasses exemplos

-- Recebe uma lista de lista de classes e retona
-- uma lista de porcentagens de cada classe
porcentagens [] _ = []
porcentagens (x:xs) tam = (fromIntegral(length x)/tam):(porcentagens xs tam)
                   
-- Recebe uma lista de porcentagens de uma base de exemplos
-- e retorna a entropia dessa base
entropia xs = (*(-1)) (sum [x*(log x/log 2) | x<-xs]) 

-- melhorTeste caracteristicas exemplos = 
--                                         where classes = sort (separaClasses exemplos)
--                                               porcentagens' = porcentagens (group classes) (fromIntegral (length classes))
--                                               entropiaDaBase = entropia porcentagens'


