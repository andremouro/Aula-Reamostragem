#Aula 1

#Começamos abrindo o diretório em que iremos trabalhar
#A função setwd() abre um diretório (pasta). Então copiamos o caminho para a pasta
#que queremos trabalhar, e colamos dentro da função. É preciso colocar o caminho entre aspas
#pois não é um comando conhecido pelo programa.

setwd('')

#Depois de abrirmos a pasta que queremos trabalhar, é preciso abrir o arquivo.
#No caso, queremos abrir o arquivo "Aranha_Amostra1.csv".
#Para abrir um arquivo, usamos a função read.csv()

read.csv(
"Aranha_Amostra1.csv", #Primeiro, colocamos o nome do arquivo
header = T, #Agora indicamos que nessa planilha, as colunas possuem título
sep = ';' #Por fim, indicamos qual foi o separador de colunas utilizado. No caso, foi o ';'.
)

#A planilha foi aberta, no entanto não podemos trabalhar com ela. Para podermos trabalhar com a planilha
#precisamos atribuí-la a um objeto. Vamos criar um objeto chamado "dados", e nele vamos colocar a planilha
#Para criar um objeto, usamos o símbolo " <- ". 

dados <- read.csv("Aranha_Amostra1.csv", header = T, sep = ';')

#Agora, toda vez que escrevermos dados, o programa irá trazer a nossa planilha

dados

#Vamos explorar um pouco estes dados.Para isso, podemos usar a função "str()"

str(dados)

#Vemos que "dados" é um data.frame (planilha). Possui 20 observações para 2 variáveis.
#As variáveis são os nomes das colunas: ind e tamanho_espinhos. Vemos que a coluna ind
#possui valores inteiros (int); enquanto que o tamanho_espinhos possui valores numéricos (num).

#Vamos sumarizar estes dados.
#Para isso começamos calculando a média do tamanho dos espinhos das nossas amostras.
#Vamos começar calculando na mão. 

#Primeiro, precisamos selecionar a coluna tamanho_espinhos de dentro da nossa planilha.
#Para fazer isso, podemos utilizar o sinal "$".

dados$tamanho_espinhos

#Com esse sinal, indicamos que queremos selecionar de dentro do data.frame dados, a coluna tamanho_espinhos.
#Para calcular precisamos somar todos os valores e dividí-los pelo número de observações.
#Para realizar a soma, podemos usar a função "sum()"

sum(dados$tamanho_espinhos)

#A soma de todas as observações é de 4.77. Vamos dividir por 20.

sum(dados$tamanho_espinhos)/20

#A média do tamanho de espinhos da nossa amostra é de 0.24. Vamos criar um objeto com esse valor

med1 <- sum(dados$tamanho_espinhos)/20
med1

#Ao invés de calcular na mão. Podemos usar a função "mean()".

med2 <- mean(dados$tamanho_espinhos)
med2

#Um outro parâmetro descritivo é a variância e o desvio padrão.
#Vamos começar calculando a variância.
#Vamos começar calculando a soma do tamanho dos espinhos - média

dados$tamanho_espinhos - med2

(dados$tamanho_espinhos - med2)^2

sum((dados$tamanho_espinhos - med2)^2)

sum((dados$tamanho_espinhos - med2)^2)/19

var1 <- sum((dados$tamanho_espinhos - med2)^2)/19
var1

#O desvio padrão é a raiz quadrada da variância

desvpad1 <- var1^(1/2)
desvpad1

desvpad2 <- sqrt(var1)
desvpad2

#Ou podemos usar a função sd()

desvpad3 <- sd(dados$tamanho_espinhos)
desvpad3

?sd
#Agora vamos plotar esses dados em um boxplot, para termos uma visualização da média e da distribuição dos dados
#Para plotar um boxplot, podemos usar a função "boxplot()".

boxplot(
dados$tamanho_espinhos, #Primeiro colocamos o nome da coluna que queremos plotar
xlab = 'Dados', #Podemos adicionar o nome do eixo x
ylab = 'Tamanho dos espinhos (mm)' #E o nome do eixo y
)


##########################################
##########################################
##########################################
#Agora vamos abrir uma outra planilha
#Vamos abrir a planilha "Aranhas_Amostra2.csv"

#Abrimos o diretório, caso ainda não esteja aberto
setwd('C:/Users/andre/Desktop/Documentos/Projetos/Dout/Proj-WoodDecomposition/PROJECT/ESCRITO/APRESENTAÇÃO/AulaDecomposi/AulaPrática')

#E vamos abrir a planilha "Aranhas_Amostra2.csv"

read.csv(
"Aranha_Amostra2.csv", #Primeiro, colocamos o nome do arquivo
header = T, #Agora indicamos que nessa planilha, as colunas possuem título
sep = ';' #Por fim, indicamos qual foi o separador de colunas utilizado. No caso, foi o ';'.
)

#Faltou atribuir a um objeto. Vamos criar o objeto 'dados2'

dados2 <- read.csv(
"Aranha_Amostra2.csv", #Primeiro, colocamos o nome do arquivo
header = T, #Agora indicamos que nessa planilha, as colunas possuem título
sep = ';' #Por fim, indicamos qual foi o separador de colunas utilizado. No caso, foi o ';'.
)

dados2

#Vamos ver os dados com a função str()

str(dados2)

#Vemos que esta planilha possui uma nova coluna, a coluna 'amostra'
#Por essa coluna, conseguimos distinguir se a observação foi feita na amostra 1 ou na amostra 2

#Agora vamos calcular a média, variância e desvio padrão para cada uma das amostras

#Para fazer isso, precisamos selecionar apenas os dados de uma das amostras.
#Podemos usar a função subset()

subset(
dados2, #Primeiro, dizemos qual o data.frame que queremos dividir
subset = dados2$amostra == 'amostra1' #Em seguida, damos o teste lógico para a divisão
#No caso, queremos selecionar apenas as linhas em que na coluna 'amostra' esteja presente 'amostra1'
)

#Faltou atribuir a um objeto

dado.am1 <- subset(
dados2, #Primeiro, dizemos qual o data.frame que queremos dividir
subset = dados2$amostra == 'amostra1' #Em seguida, damos o teste lógico para a divisão
#No caso, queremos selecionar apenas as linhas em que na coluna 'amostra' esteja presente 'amostra1'
)

dado.am1


#Agora façam isso criando um objeto apenas com as observações da amostra2 e calculem a média, variância e desv pad para essa outra amostra.

##############################
##############################

dado.am2 <- subset(
dados2, 
subset = dados2$amostra == 'amostra2'
)

dado.am2

med.am2 <- mean(dado.am2$tamanho_espinhos)
med.am2

var.am2 <- sum((dado.am2$tamanho_espinhos - med.am2)^2)/19
var.am2

desvpad1 <- sqrt(var.am2)
desvpad1

desvpad2 <- sd(dado.am2$tamanho_espinhos)

#Agora vamos plotar os resultados, usando a função boxplot.

boxplot(
tamanho_espinhos ~ amostra #colocamos aqui a função Tamanho dos espinhos em relação ao tipo de amostra
, data = dados2 #E aqui, adicionamos o data.frame de onde serão retirados os dados
)


########################################
########################################
#######Análise de reamostragem##########
########################################
########################################

#Vamos usar o pacote 'Rsampling' para a análise
#Vamos carregar o pacote usando a função library()
library(Rsampling)

#Agora vamos fazer uma reamostragem dos dados, apenas da amostra 1
#Para fazer a reamostragem e tirar uma planilha com os dados, usamos a função
#within_columns() do pacote

within_columns(
dado.am1, #Começamos identificando o data.frame com o qual iremos trabalhar
cols = 3, #Agora dizemos qual a coluna que terá os dados embaralhados. No caso, é a coluna 3
replace = T, #Indicamos que queremos que haja reposição dos dados
FUN = base::sample #Usamos esse argumento que é padrão também.
)

#Faltou atribuir a um objeto
dado.r.am1 <- within_columns(
dado.am1, 
cols = 3, 
replace = T, 
FUN = base::sample 
)

#Vamos comparar as duas planilhas
dado.am1
dado.r.am1

#Agora vamos calcular a média, variância e desvpad desta nossa nova planilha.

med.r <- mean(dado.r.am1$tamanho_espinhos)
med.r

var.r <- var(dado.r.am1$tamanho_espinhos)
var.r

var.r.2 <- sum((dado.r.am1$tamanho_espinhos - med.r)^2)/19
var.r.2

desvpad.r <- sd(dado.r.am1$tamanho_espinhos)
desvpad.r

desvpad.r.2 <- sqrt(var.r.2)
desvpad.r.2

#Agora, vamos fazer muitas simulações, e ver o quanto o valor de média
#varia entre as simulações

#Para isso, precisamos criar primeiro uma função

stat <- function(dataframe){
media <- mean(dataframe$tamanho_espinhos)
}

#E depois usamos a função Rsampling

data.r <-
Rsampling(
type = 'within_columns', #Dizemos o tipo de embaralhamento que queremos (dentro das colunas)
data = dado.am1, #Indicamos oconjunto de dados (data.frame)
statistics = stat, #Qual a estatística que será usada (a função que acabamos de criar)
ntrials = 1000, #O número de embaralhamento
replace = T, #Indicamos que deve haver a reposição de dados
cols = 3 #E indicamos qual coluna será embaralhada
)

#Vamos ver os dados
data.r
#Cada valor representa a média calculada para o conjunto de dados sorteados.

#Vamos plotar agora a frequência dessas médias, usando a função hist()
hist(data.r)

#Com a frequência, nós podemos calcular qual a probabilidade de tirarmos um valor de média ao acaso!
#Para isso, calculamos quantas a proporção de aleatorizações que tiveram um valor maior que a média que desejamos saber

sum(data.r >= stat(dado.am1))/1000

sum(data.r >= 0.3)/1000
