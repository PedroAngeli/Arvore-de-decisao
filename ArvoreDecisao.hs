module ArvoreDecisao where


data Arv = Folha String String | No String String [Arv] deriving Show