# Script tirado de 
# Parte 1: https://educationalresearchtechniques.com/2017/03/06/logistic-regression-in-r-2/
# Parte 2: https://educationalresearchtechniques.com/2017/03/08/validating-a-logistic-model-in-r/
# OBS: Fiz modificações no script original que tinha nome de variaveis parecidos mas com diferenças em poucas letras, e a ordem de apresentação dos dados usados para treino e validação estavam confusas. 

# Vamos criar um código que prever o sexo do individuo quando se tem informação sobre uma serie doutras variáveis. Depois vamos testar a acurácia do modelo por validação cruzada. 

# Limpar memoria do R
rm(list=ls())

# Carregar pacotes
library(MASS);library(bestglm);library(reshape2);library(corrplot)

# Carregar bando de dados
data(survey)
?MASS::survey # Descrição do conjunto de dados utilizado: respostas de 237 estudantes de Estatística I da Universidade de Adelaide a várias perguntas.


# Primeiro vamos remover do banco de dados todas as variaveis categoricas. Isso e necessario porque a função utilizada para a cross-validation não aceita fatores. 
survey$Clap<-NULL
survey$W.Hnd<-NULL
survey$Fold<-NULL
survey$Exer<-NULL
survey$Smoke<-NULL
survey$M.I<-NULL
survey<-na.omit(survey) # Esse codigo apaga todas as linhas que tem ao menos um NA. É uma função util para quem quer um banco de dados completo

# Após realizar essa transformação dos dados precisamos checar a colinearidade. Para isso, vamos usar a função corrplot.mixed do pacote corrplot
pc<-cor(survey[,2:5]) # faz uma matriz que calcula a correlacao entre as colunas 2:5
corrplot.mixed(pc)    # Plot das correações observadas



set.seed(123) # O autor do tutorial também definiu o seed, mas não explicou porque isso é necessário. De qualquer forma, eu repeti o procedimento. 
ind<-sample(2,nrow(survey),replace=T,prob = c(0.7,0.3)) # Cada valor representa se a linha vai ser sorteada ou não. 
train<-survey[ind==1,] # dados de treinamento - usados para estimar os parametros  
test<-survey[ind==2,] # dados de validação - usados para validar o modelo


# Criar o modelo desejado (nesse caso um modelo logístico)
fit<-glm(Sex~.,family=binomial,train)
summary(fit)
# odds ratios
exp(coef(fit))
# Note que o modelo indica que a Height é a única variável que explica o sexo do indivíduo. Se você pedir o odds ratios do modelo, vai ter a acesso a informação de que a mudança em 1 unidade da variável independente (altura) aumenta a probabilidade de probabilidade de ser homem em 1.23 odds (não entendi como interpretar esse valor)

######################################
#####    VALIDAÇÃO DO MODELO!    #####
######################################
# Vamos usar os  dados o conjunto de dados teste (objeto test) para validar o modelo

# Primeiro vamos criar uma coluna que representa a aplicação do nosso modelo (criado anteriormente) para prever se aquele dado é de um macho ou de uma fêmea.
test$prob<-predict(fit,newdata = test, type = 'response') 

# Depois você cria uma outra coluna, que vai representar categoricamente as probabilidades apresentadas anteriormente. Se a probabilidade calculada para ser macho for de >0.5, a célula vai definir o dado como macho, se for <0.5 vai definir como fêmea. 
test$predict<-rep('Female',46)
test$predict[test$prob>0.5]<-"Male"

# Por fim, testamos a eficácia do modelo calculando frequência com que o modelo conseguiu acertar o sexo do indivíduo. Mais a adiante vamos criar uma estrutura que vai refazer esse procedimento varias vezes, para estimar a precisão do nosso modelo.  
mean(test$predict==test$Sex)

# Uma maneira visual de enxergar isso é fazer uma tabela que mostrando quantas vezes uma fêmea foi corretamente identificada como fêmea, quantas vezes a fêmea foi erroneamente identificada, quantas vezes um macho foi corretamente identificado como macho, quantas vezes um macho foi erroneamente identificado. 
table(test$predict,test$Sex)

# Agora por curiosidade, vamos repetir o mesmo procedimento com os dados usados na criação do modelo
train$prob<-predict(fit, type = 'response')
train$predict<-rep('Female',123)
train$predict[train$prob>0.5]<-"Male"
mean(train$predict==train$Sex)
table(train$predict,train$Sex)

# Veja que curiosamente nosso modelo foi melhor para prever os dados do conjunto de dados de teste, que do proprio conjunto de dados que deu origem ao modelo. 

######################################
#####  K-fold cross validation   #####
######################################
# K-fold cross validation - modelo logístico
# Agora vamos iniciar a validação cruzada k-fold, para testar o quão bom nosso modelo é para prever o sexo com base num conjunto de variáveis. A validação cruzada k-fold não funciona para variáveis respostas categóricas. Vamos transformar os dados, de forma que machos sejam codificados como 1 e fêmeas como 0. Ou seja, é a variável resposta é 1=SIM ou 0=NÃO para o sexo masculino. 
# Nesse passo, vamos criar um conjunto de dados chamado "my.cv" com uma coluna chamada "y" para receber o codigo do sexo (0 ou 1), e vamos remover as colunas:
#  .	Sex: representa categoricamente o sexo, e são dados que vieram do banco de dados original
#  .	Prob: representa a probabilidade do indivíduo ser do sexo masculino, e são dados que vieram da aplicação do modelo para prever o sexo
#  .	Predict: representa categoricamente o sexo, e são dados que vieram da aplicação do modelo para prever o sexo
train$y<-rep(0,123)
train$y[train$Sex=="Male"]=1
my.cv<-train[,-c(1,7,8)]


# O codigo para o K-fold analysis é:
bestglm(Xy=my.cv,IC="CV",CVArgs = list(Method="HTF",K=10,REP=1),family = binomial)

# Esse resultado confirma o que deu anteriormente, que apenas a variável Height prevê o sexo do indivíduo. 

# O tutorial continua com mais informacoes de como fazer a validação, mas faltou referencias teoricas, e não senti confiança nas proximas etapas. 
# Acredito que até aqui temos uma boa nocao do que se trata

