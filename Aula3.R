#Começamos abrindo o diretório em que iremos trabalhar

setwd('')

#Vamos trabalhar com os dados de abundância de formigueiros.
#Para isso precisamos abrir a planilha "TaxaDecomp"
#Vamos abrir a planilha e atribuí-la ao objeto 'dados'

dados <- 
read.csv(
"TaxaDecomp.csv", #Primeiro, colocamos o nome do arquivo
header = T, #Agora indicamos que nessa planilha, as colunas possuem título
sep = ';' #Por fim, indicamos qual foi o separador de colunas utilizado. No caso, foi o ';'.
)

#Vamos explorar um pouco estes dados.Para isso, podemos usar a função "str()"

str(dados)

#Vemos que 'dados' é um data.frame. Possui 3 colunas (n_id, temp e taxa_decomp
#O n_id refere-se ao número da parcela em que foram feitas as observações
#A temp é a temperatura média em que as medidas de decomposição foram feitas
#taxa_decomp é a taxa de decomposição da matéria orgânica.

#Neste caso, não há a necessidade de tirarmos os parâmetros descritivos
#O que vamos testar aqui é uma relação entre as duas variáveis (temp e taxa de decomp)
#Para entender isso melhor, vamos plotar os dados
#Para isso, vamos usar a função plot()

#Para plotarmos, primeiro é necessário saber o que queremos testar
#Queremos testar se há uma relação entre as duas variáveis.
#Com o aumento da temperatura, esperamos que aumente a taxa de decomposição
#Temperaturas muito baixas inibem a atividade dos decompositores, e por isso a decomposição seria lenta

#Então, neste caso, a variável preditora é: TEMPERATURA
#E a variável resposta é: A TAXA DE DECOMPOSIÇÃO

#Então vamos plotar

plot(
taxa_decomp ~ temp, #Primeiro, colocamos a fórmula. Variável Resposta ~ Variável Preditora
data = dados)

#Aparentemente existe uma relação entre as duas variáveis. Uma relação positiva
#Vamos ajustar um modelo linear, para que se estime o coeficiente angular e o intercepto
#Para ajustar um modelo linear, usamos a função lm()

lm(
taxa_decomp ~ temp,#Primeiro, colocamos a fórmula. Variável Resposta ~ Variável Preditora
data = dados)

#Temos o valor do intercepto e o valor do coeficiente angular
#Vamos criar um objeto com esse modelo

mod <- lm(taxa_decomp ~ temp, data = dados)

#Vamos acessar os coeficientes usando o operador $
mod$coefficients

mod$coefficients[1] #Intercepto
mod$coefficients[2] #Coeficiente angular

#Agora podemos até incluir o modelo estimado em nosso gráfico
#Para isso, podemos usar a função abline()
abline(mod)

#Para nos certificarmos que a relação é significativa, precisamos nos certificar
#que o coeficiente angular que encontramos é pouco provável de ser obtido pelo acaso
#Ou seja, que o coeficiente angular que obtemos é significativamente diferente de 0

#Para isso, podemos utilizar o método da reamostragem

#Vamos usar o pacote 'Rsampling' para a análise
#Vamos carregar o pacote usando a função library()
library(Rsampling)

#Para fazer a reamostragem e tirar uma planilha com os dados, usamos a função
#within_columns() do pacote

dados.r <- within_columns(
dados, #Começamos identificando o data.frame com o qual iremos trabalhar
cols = 3, #Agora dizemos qual a coluna que terá os dados embaralhados. No caso, é a coluna 2
replace = F, #Indicamos que não queremos que haja reposição dos dados
FUN = base::sample #Usamos esse argumento que é padrão também.
)

dados.r

#Vamos plotar esses dados

plot(taxa_decomp ~ temp, data = dados.r)

#Os dados ficaram muito diferentes.
#Vamos ajustar um modelo linear e ver se houve diferença nos parâmetros estimados

mod.r <- lm(taxa_decomp ~ temp, data = dados.r)
mod.r

#Os dados ficaram muito diferentes
mod.r$coefficients[1] #intercepto
mod.r$coefficients[2] #coeficiente angular

#Vamos plotar o modelo
abline(mod.r)




#Agora, vamos fazer muitas simulações, e ver o quanto o valor de média
#varia entre as simulações

#Para isso, precisamos criar primeiro uma função
#Nessa função, queremos automatizar a estimativa do coeficiente angular

stat <- function(dataframe){
mod <- lm(taxa_decomp ~ temp, data = dataframe)
coef <- mod$coefficients[2]
}

#E depois usamos a função Rsampling

data.r <-
Rsampling(
type = 'within_columns', #Dizemos o tipo de embaralhamento que queremos (dentro das colunas)
data = dados, #Indicamos oconjunto de dados (data.frame)
statistics = stat, #Qual a estatística que será usada (a função que acabamos de criar)
ntrials = 1000, #O número de embaralhamento
replace = F, #Indicamos que deve haver a reposição de dados
cols = 3 #E indicamos qual coluna será embaralhada
)

#Vamos ver os dados
data.r
#Cada valor representa a média calculada para o conjunto de dados sorteados.

#Vamos plotar agora a frequência dessas médias, usando a função hist()
hist(data.r)

#Com a frequência, nós podemos calcular qual a probabilidade de tirarmos um valor de média ao acaso!
#Para isso, calculamos quantas a proporção de aleatorizações que tiveram um valor maior que a média que desejamos saber

sum(data.r >= stat(dados))/1000


