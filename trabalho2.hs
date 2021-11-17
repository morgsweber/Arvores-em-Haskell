-- Trabalho 2
-- 98703-02 - Programação Funcional - Turma 010 - 2021/2 - Prof. Diego Vrague Noble
-- Nomes: Arthur Luz, Daniela Rigoli e Morgana Weber

-- Considere a seguinte definição de tipo para árvores binárias:
 
data Arvore = Folha Int | No Arvore Arvore deriving (Show)

-- 1) Defina exemplos para

t1 :: Arvore
t1 = Folha 2
--
t2 :: Arvore
t2 = No (Folha 3) (Folha 1)

t3 :: Arvore
t3 = No (Folha 4) (No (Folha 1)(Folha 4))

-- que tenham, respectivamente, uma, duas e três folhas.

-- 2) Defina as funções

folhas :: Arvore -> [Int]
folhas (Folha n) = [n]
folhas (No l r)  = folhas l ++ folhas r

tamanho :: Arvore -> Int
tamanho (Folha n) = 1
tamanho (No l r)  = tamanho l + tamanho r

-- A função folhas retorna uma lista contendo os valores inteiros que estão nas folhas da árvore.
-- A função tamanho devolve a quantidade de folhas de uma árvore.

-- 3) Uma árvore binária é balanceada se todo o nó tem a propriedade ter as subárvores da esquerda e da direita com o mesmo tamanho. As folhas são trivialmente balenceadas.

-- nao balanceada p testar
t4 :: Arvore
t4 = No (Folha 9) (No (Folha 1) (No (Folha 3) (Folha 2)))

-- Defina a função

balanceada :: Arvore -> Bool
balanceada (Folha n) = True
balanceada (No l r)  = abs( tamanho l - tamanho r) <= 1 && balanceada l && balanceada r

-- que decide se uma árvore é balanceada ou não.

-- 4) Defina a função particiona :: [a] -> ([a],[a]) que divide uma lista de tamanho par
-- em duas partes de mesmo tamanho que devem estar dentro de uma tupla.

particiona :: [a] -> ([a],[a])
particiona xs = splitAt (length xs `div` 2 ) xs

-- 5) Através da função particiona, defina a função

--balancear :: [Int] -> Arvore
balancear :: [Int] -> Arvore
balancear [x] = Folha x
balancear xs  = No (balancear ys) (balancear zs)
              where (ys,zs) = particiona xs

-- que converte uma lista não-vazia com tamanho potência de 2 em uma árvore cujas folhas contém os elementos da lista.