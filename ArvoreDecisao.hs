module ArvoreDecisao where
    
    
import Utils
import Data.List
import Data.Function
import Text.Read


data Arv = Folha String String | No String String [Arv] deriving Show

-- Recebe duas strings e retorna um No com uma lista
-- de filhos vazia
criaNo pergunta resposta = No pergunta resposta []


arvoreDecisao exemplos caracteristicas maisComum resposta 
                                                          | exemplos == [] = Folha maisComum resposta 
                                                          | (mesmaClassificacao exemplos) == True = Folha (last (head exemplos)) resposta
                                                          | caracteristicas == [] = Folha (maioria exemplos) resposta
                                                          | otherwise = montaArvore valoresMelhor idxMelhor exemplos melhor node flag "-Infinity" caracteristicas
                                                          where valores = criaListaDeValores caracteristicas exemplos
                                                                melhor = melhorTeste caracteristicas exemplos valores
                                                                idxMelhor = achaIdxMelhor melhor caracteristicas
                                                                valoresMelhor = achaValoresMelhor melhor caracteristicas exemplos
                                                                node = criaNo melhor resposta
                                                                flag = (ehNumerica (head valoresMelhor))


montaArvore [] _ _ _ (No p r ys) _ _ _ = No p r ys                        
montaArvore (x:xs) idxMelhor exemplos melhor (No p r ys) flag ant caracteristicas =  montaArvore xs idxMelhor exemplos melhor (No p r (ys ++ [filho])) flag x caracteristicas
                                                                                    where exemplosi = separaExemplos exemplos idxMelhor x (length xs) flag ant
                                                                                          caracteristicasSemMelhor = tiraMelhor melhor caracteristicas
                                                                                          maioria' = maioria exemplos
                                                                                          filho = arvoreDecisao exemplosi caracteristicasSemMelhor maioria' x
                                                                  
achaValoresMelhor _ [] _ = []
achaValoresMelhor melhor (x:xs) exemplos | melhor == (head (fst x)) = if length (fst x) == 1 then head(criaListaDeValores [x] exemplos) else (tail (fst x))
                                         | otherwise = achaValoresMelhor melhor xs exemplos

-- Recebe uma string e a lista de caracteristicas e retorna
-- o indice dessa string na lista
achaIdxMelhor _ [] = -1
achaIdxMelhor melhor (x:xs) | melhor == (head (fst x)) = snd x
                            | otherwise = achaIdxMelhor melhor xs


-- Recebe um valor e retorna se é numérico ou não
ehNumerica x | xConvertido == Nothing = False
             | otherwise = True
             where xConvertido = (readMaybe x) :: Maybe Double 


-- Recebe uma base de exemplos e informações adicionais
-- para tratar caracteristicas numericas e retorna
-- uma lista de exemplos que correspondem ao valor vi
separaExemplos [] _ _ _ _ _ = []
separaExemplos (x:xs) idxMelhor vi tam flag ant | flag == False = if valor == vi then x:(separaExemplos xs idxMelhor vi tam flag ant) else separaExemplos xs idxMelhor vi tam flag ant
                                                | tam == 0 = if valorConvertido > viConvertido then x:(separaExemplos xs idxMelhor vi tam flag ant) else separaExemplos xs idxMelhor vi tam flag ant
                                                | otherwise = if (valorConvertido > antConvertido) && (valorConvertido <= viConvertido) then x:(separaExemplos xs idxMelhor vi tam flag ant) else separaExemplos xs idxMelhor vi tam flag ant
                                                where valor = (x !! idxMelhor)
                                                      valorConvertido = converteMaybe ((readMaybe valor) :: Maybe Double)
                                                      antConvertido = converteMaybe ((readMaybe ant) :: Maybe Double)
                                                      viConvertido = converteMaybe ((readMaybe vi) :: Maybe Double)



tiraMelhor melhor (x:xs) | melhor == head(fst x) = xs
                         | otherwise = x:(tiraMelhor melhor xs)

valorDoNo (Folha p r) = r
valorDoNo (No p r xs) = r


