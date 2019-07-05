module Utils where 

import Data.List
import Data.Function
import Text.Read

-- Recebe uma lista de lista de caracteristicas e retorna
-- uma lista de lista de tupla cujo fst é a caracteristica
-- e o snd é o indice da coluna correspondente
formataCaracteristica [] _ = []
formataCaracteristica (x:xs) idx = (x,idx):formataCaracteristica xs (idx+1) 

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
                   
-- Recebe uma base de exemplos
-- e retorna a entropia dessa base
entropia exemplos = (*(-1)) (sum [x*(log x/log 2) | x<-xs]) 
                    where classes = sort (separaClasses exemplos)
                          xs = porcentagens (group classes) (fromIntegral (length classes))


-- Recebe um indice e a lista de exemplos e 
-- retorna uma tupla do valor da caracteristica e
-- sua respectiva classe
listaDeTuplas idx exemplos = [(x !! idx,last x) | x<-exemplos]


-- Função auxiliar de discretizar
discretizar' [] _ = []
discretizar' (x:xs) ant = (show ((n1 + n2)/2)):discretizar' xs x
                          where n1 = read (fst (last ant)) :: Double
                                n2 = read (fst (head x)) :: Double

-- Recebe um indice que representa o indice da caracteristica
-- na lista de caracteristicas e recebe a base de exemplos e retorna
-- uma lista de valores discretizados da caracteristica
discretizar idx exemplos = (listaDiscretetizada) ++ [(last listaDiscretetizada)] 
                           where listaDeTuplas' = sort (listaDeTuplas idx exemplos)
                                 tuplaAgrupada = groupBy ((==) `on` snd) listaDeTuplas'
                                 listaDiscretetizada = discretizar' (tail tuplaAgrupada) (head tuplaAgrupada)

-- Recebe a lista de lista de caracteristicas e a lista de exemplos
-- e retorna uma lista de lista com os valores dessas caracteristicas
criaListaDeValores [] _ = []
criaListaDeValores (x:xs) exemplos | (length (fst x)) == 1 = (discretizar (snd x) exemplos):(criaListaDeValores xs exemplos)
                                       | otherwise = (tail (fst x)):(criaListaDeValores xs exemplos)



--entropia' xs = (*(-1)) (sum [x*(log x/log 2) | x<-xs])

avaliaIG' (Just x) = x

avaliaIG x val idx tamLista | talvez == Nothing = x == valor
                            | tamLista == 0 = valorConvertido > xConvertido
                            | otherwise =  valorConvertido <= xConvertido 
                            where valor = (val !! idx)
                                  talvez = (readMaybe x) :: Maybe Double
                                  xConvertido = avaliaIG' talvez
                                  valorConvertido = read valor :: Double
                                  

-- Recebe uma lista de valores ["Sol","Chuva","Nublado"],
-- uma base de exemplos e o indice da caracteristica, ou seja
-- sua coluna correspondente
ig' [] _ _ = []
ig' (x:xs) exemplos idx = ((tamListaGerada/tamBaseDeExemplos) * (entropia listaGerada)):(ig' xs exemplos idx)
                       where listaGerada = [val | val<-exemplos, (avaliaIG x val idx (length xs))]
                             tamListaGerada = fromIntegral(length listaGerada) 
                             tamBaseDeExemplos = fromIntegral(length exemplos)


                            