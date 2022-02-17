#Começamos abrindo o diretório em que iremos trabalhar

setwd('C:/Users/andre/Desktop/Documentos/Projetos/Dout/Proj-WoodDecomposition/PROJECT/ESCRITO/APRESENTAÇÃO/AulaDecomposi/AulaPrática')

#Vamos trabalhar com os dados de abundância de formigueiros.
#Para isso precisamos abrir a planilha "DadoFormig"
#Vamos abrir a planilha e atribuí-la ao objeto 'dados'

dados <- 
read.csv(
"DadoFormig.csv", #Primeiro, colocamos o nome do arquivo
header = T, #Agora indicamos que nessa planilha, as colunas possuem título
sep = ';' #Por fim, indicamos qual foi o separador de colunas utilizado. No caso, foi o ';'.
)

#Vamos explorar um pouco estes dados.Para isso, podemos usar a função "str()"

str(dados)

#Vemos que 'dados' é um data.frame. Possui 3 colunas (n_id, habitat e n_formig)
#O n_id refere-se ao número da parcela em que foram feitas as observações
#O habitat diz respeito à estrutura da vegetação em que as observações foram feitas
#n_formig é o número de formigueiros encontrados dentro da parcela.

#Vamos extrair os parâmetros descritivos (média, variância e desvio padrão)
#Vamos fazer isso para o campo e para a floresta

#Começamos criando um novo data.frame apenas com as observações feitas no 'campo'.

#Podemos usar a função subset()

dados.c <-
subset(
dados, #Primeiro, dizemos qual o data.frame que queremos dividir
subset = dados$habitat == 'Campo' #Em seguida, damos o teste lógico para a divisão
#No caso, queremos selecionar apenas as linhas em que na coluna 'habitat' esteja presente 'Campo'
)

dados.c

#Queremos calcular a média do número de formigueiros por parcelas no Campo
med.c <- mean(dados.c$n_formig)
med.c

var.c <- var(dados.c$n_formig)
var.c

desv.c <- sd(dados.c$n_formig)
desv.c

#Agora vamos fazer para as parcelas da Floresta

dados.f <-
subset(
dados, #Primeiro, dizemos qual o data.frame que queremos dividir
subset = dados$habitat == 'Floresta' #Em seguida, damos o teste lógico para a divisão
#No caso, queremos selecionar apenas as linhas em que na coluna 'habitat' esteja presente 'Floresta'
)

dados.f

#Queremos calcular a média do número de formigueiros por parcelas no Campo
med.f <- mean(dados.f$n_formig)
med.f

var.f <- var(dados.f$n_formig)
var.f

desv.f <- sd(dados.f$n_formig)
desv.f

#Agora que tiramos os parâmetros descritivos das amostras, vamos plotar os resultados
#Para isso, precisamos saber o que queremos testar

#O problema proposto indica que o pesquisador deseja saber se a presença de formigueiros está relacionada com a estrutura da comunidade vegetal.
#Então ele quer saber se há diferença na densidade de formigueiros entre Campo e Floresta
#Neste caso, a variável preditora é: O HABITAT
#E a variável resposta é: O Nº de FORMIGUEIROS

#Plotamos um boxplot (já que a variável preditora é categórica)

boxplot(
n_formig ~ habitat, #Primeiro colocamos a fórmula Variável Resposta ~ Variável Preditora
data = dados)

#Vemos que há diferença, mas precisamos testar se ela é significativa, ou se poderia ser fruto do acaso.
#Para isso, usaremos o método de Re-amostragem

#Primeiro, precisamos definir a estatística de interesse (o teste)
#Queremos testar a diferença entre as médias de formigueiros no campo e na floresta
#Vamos calcular essa diferença nos dados que coletamos
#Para calcular cada média, podemos usa a função tapply()

tapply(
dados$n_formig,#Primeiro, adicionamos a coluna com os valores de interesse. No caso, é o nº de formigueiros
 INDEX= dados$habitat, #depois, usamos o índice que será usado para separar as obserações. Nossas observações estão separadas pelo habitat, então selecionamos essa coluna.
 FUN = mean #Por fim, adicionamos a função que queremos realizar. No caso, a média.
)

#Obtemos dois valores. Um valor de média para o campo, e o outro para a floresta.
#Vamos colocar esses resultados em um objeto chamado 'meds'

meds <- tapply(
dados$n_formig,
 INDEX= dados$habitat, 
 FUN = mean 
)
meds

#Para selecionar um dado específico neste objeto, podemos usar o operador []
meds[1]#Seleciona o primeiro dado
meds[2]#Seleciona o segundo dado

#Agora vamos calcular a diferença entre as médias

dif.obs <- meds[1] - meds[2]
dif.obs

#Precisamos nos certificar que essa diferença não foi obtida pelo acaso.
#Vamos fazer a aleatorização dos dados, e calcular a diferença entre as médias dos dados aleatorizados

#Vamos usar o pacote 'Rsampling' para a análise
#Vamos carregar o pacote usando a função library()
library(Rsampling)

#Para fazer a reamostragem e tirar uma planilha com os dados, usamos a função
#within_columns() do pacote

dados.r <- within_columns(
dados, #Começamos identificando o data.frame com o qual iremos trabalhar
cols = 2, #Agora dizemos qual a coluna que terá os dados embaralhados. No caso, é a coluna 2
replace = F, #Indicamos que não queremos que haja reposição dos dados
FUN = base::sample #Usamos esse argumento que é padrão também.
)

meds.r <- tapply(
dados.r$n_formig,
 INDEX= dados.r$habitat, 
 FUN = mean 
)
meds.r

dif.r <- meds.r[1] - meds.r[2]
dif.r

#Agora, vamos fazer muitas simulações, e ver o quanto o valor de média
#varia entre as simulações

#Para isso, precisamos criar primeiro uma função
#Nessa função, automatizamos o calculo da diferença das médias.

stat <- function(dataframe){
meds <- tapply(dataframe$n_formig, INDEX = dataframe$habitat, FUN = mean)
dif <- meds[1] - meds[2]
}

#E depois usamos a função Rsampling

data.r <-
Rsampling(
type = 'within_columns', #Dizemos o tipo de embaralhamento que queremos (dentro das colunas)
data = dados, #Indicamos oconjunto de dados (data.frame)
statistics = stat, #Qual a estatística que será usada (a função que acabamos de criar)
ntrials = 1000, #O número de embaralhamento
replace = F, #Indicamos que deve haver a reposição de dados
cols = 2 #E indicamos qual coluna será embaralhada
)

#Vamos ver os dados
data.r
#Cada valor representa a média calculada para o conjunto de dados sorteados.

#Vamos plotar agora a frequência dessas médias, usando a função hist()
hist(data.r)

#Com a frequência, nós podemos calcular qual a probabilidade de tirarmos um valor de média ao acaso!
#Para isso, calculamos quantas a proporção de aleatorizações que tiveram um valor maior que a média que desejamos saber

sum(data.r >= stat(dados))/1000

sum(data.r >= 0.3)/1000
