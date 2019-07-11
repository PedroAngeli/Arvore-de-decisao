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
--Recebe um no interno ou folha e retorna
--seu valor "resposta"
respostaDoNo (Folha p r) = r
respostaDoNo (No p r xs) = r

perguntaDoNo (Folha p r) = p
perguntaDoNo (No p r xs) = p

valorDoNo (Folha p r) = r
valorDoNo (No p r xs) = r

filhosDoNo (No p r xs) = xs

avaliaCaso (Folha p r) _ _ = p
avaliaCaso (No p r xs) caracteristicas caso = avaliaCaso filho caracteristicas caso
                                              where idx = achaIndicePergunta p caracteristicas
                                                    valorCaso = (caso !! idx)
                                                    flag = ehNumerica valorCaso
                                                    filho = filhoCorrespondente valorCaso xs flag "-Infinity"

achaIndicePergunta pergunta (x:xs) | pergunta == head (fst x) = snd x
                                   | otherwise = achaIndicePergunta pergunta xs


filhoCorrespondente valorCaso (x:xs) flag ant | flag == False = if (valorDoNo x) == valorCaso then x else filhoCorrespondente valorCaso xs flag ant
                                              | (length xs) > 0 = if (valorCasoConvertido > antConvertido) && (valorCasoConvertido <= valorNoConvertido) then x else filhoCorrespondente valorCaso xs flag (valorDoNo x)
                                              | otherwise = x
                                              where valorCasoConvertido = converteMaybe ((readMaybe valorCaso) :: Maybe Double)
                                                    antConvertido = converteMaybe ((readMaybe ant) :: Maybe Double)
                                                    valorNoConvertido = converteMaybe ((readMaybe (valorDoNo x)) :: Maybe Double)


                                        

printaSeNao esp = esp ++ "senao\n"
printaFimSe esp = esp ++ "fim-se\n"

imprime (Folha p r) esp _ _ _ _= esp ++ "retorne " ++ p ++ "\n"
imprime (No p r xs) esp pergunta resposta ys ant | ehNumerica resposta == False = esp ++ "se " ++ pergunta ++ " = " ++ resposta ++ " entao\n"
                                              | otherwise = if length ys == 0 then esp ++ "se " ++ pergunta ++ " > " ++ resposta ++ " entao\n" else if ehNumerica ant == False then esp ++ "se " ++ pergunta ++ " <= " ++ resposta ++ " entao\n" else esp ++ "se " ++ ant ++ " < " ++ pergunta ++ " <= " ++ resposta ++ " entao\n"

imprimeNo (Folha p r) esp _= imprime (Folha p r) esp p r p r
imprimeNo (No p r xs) esp ant = imprime' xs (No p r xs) esp ant

imprime' [] _ _ _ = ""
imprime' (x:xs) (No p r ys) esp ant = imprime (No p r ys) esp p (respostaDoNo x) xs ant ++ imprimeNo x (esp ++ "   ") ant ++ if length xs == 0 then printaFimSe esp else printaSeNao esp ++ imprime' xs (No p r ys) (esp ++ "   ") (respostaDoNo x) ++ printaFimSe esp

