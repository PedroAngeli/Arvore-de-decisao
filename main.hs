import Data.List

data Arv = Folha String String | No String String [Arv] deriving Show

main = do
       entradaDescricao <- readFile "descricao.txt"
       entradaBase <- readFile "base.txt"
       let caracteristicas = formataEntrada entradaDescricao
       let exemplos = formataEntrada entradaBase
       let classes = sort (separaClasses exemplos)
       let p = porcentagens classes 1 (head classes) (fromIntegral (length classes))
       putStrLn (show classes)
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


porcentagens [] count _ tam = [count/tam]
porcentagens (x:xs) count ant tam | x == ant = porcentagens xs (count+1) x tam
                                  | otherwise = [count/tam] ++ (porcentagens xs 1 x tam)
                              

-- melhorTeste caracteristicas exemplos = 
--                                         where classes = sort (separaClasses exemplos)
--                                               porcentagens' = porcentagens classes 1 (head classes) (fromIntegral (length classes))


