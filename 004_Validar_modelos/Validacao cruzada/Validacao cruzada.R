# Script tirado de 
# Parte 1: https://educationalresearchtechniques.com/2017/03/06/logistic-regression-in-r-2/
# Parte 2: https://educationalresearchtechniques.com/2017/03/08/validating-a-logistic-model-in-r/
# OBS: Fiz modifica��es no script original que tinha nome de variaveis parecidos mas com diferen�as em poucas letras, e a ordem de apresenta��o dos dados usados para treino e valida��o estavam confusas. 

# Vamos criar um c�digo que prever o sexo do individuo quando se tem informa��o sobre uma serie doutras vari�veis. Depois vamos testar a acur�cia do modelo por valida��o cruzada. 

# Limpar memoria do R
rm(list=ls())

# Carregar pacotes
library(MASS);library(bestglm);library(reshape2);library(corrplot)

# Carregar bando de dados
data(survey)
?MASS::survey # Descri��o do conjunto de dados utilizado: respostas de 237 estudantes de Estat�stica I da Universidade de Adelaide a v�rias perguntas.


# Primeiro vamos remover do banco de dados todas as variaveis categoricas. Isso e necessario porque a fun��o utilizada para a cross-validation n�o aceita fatores. 
survey$Clap<-NULL
survey$W.Hnd<-NULL
survey$Fold<-NULL
survey$Exer<-NULL
survey$Smoke<-NULL
survey$M.I<-NULL
survey<-na.omit(survey) # Esse codigo apaga todas as linhas que tem ao menos um NA. � uma fun��o util para quem quer um banco de dados completo

# Ap�s realizar essa transforma��o dos dados precisamos checar a colinearidade. Para isso, vamos usar a fun��o corrplot.mixed do pacote corrplot
pc<-cor(survey[,2:5]) # faz uma matriz que calcula a correlacao entre as colunas 2:5
corrplot.mixed(pc)    # Plot das correa��es observadas



set.seed(123) # O autor do tutorial tamb�m definiu o seed, mas n�o explicou porque isso � necess�rio. De qualquer forma, eu repeti o procedimento. 
ind<-sample(2,nrow(survey),replace=T,prob = c(0.7,0.3)) # Cada valor representa se a linha vai ser sorteada ou n�o. 
train<-survey[ind==1,] # dados de treinamento - usados para estimar os parametros  
test<-survey[ind==2,] # dados de valida��o - usados para validar o modelo


# Criar o modelo desejado (nesse caso um modelo log�stico)
fit<-glm(Sex~.,family=binomial,train)
summary(fit)
# odds ratios
exp(coef(fit))
# Note que o modelo indica que a Height � a �nica vari�vel que explica o sexo do indiv�duo. Se voc� pedir o odds ratios do modelo, vai ter a acesso a informa��o de que a mudan�a em 1 unidade da vari�vel independente (altura) aumenta a probabilidade de probabilidade de ser homem em 1.23 odds (n�o entendi como interpretar esse valor)

######################################
#####    VALIDA��O DO MODELO!    #####
######################################
# Vamos usar os  dados o conjunto de dados teste (objeto test) para validar o modelo

# Primeiro vamos criar uma coluna que representa a aplica��o do nosso modelo (criado anteriormente) para prever se aquele dado � de um macho ou de uma f�mea.
test$prob<-predict(fit,newdata = test, type = 'response') 

# Depois voc� cria uma outra coluna, que vai representar categoricamente as probabilidades apresentadas anteriormente. Se a probabilidade calculada para ser macho for de >0.5, a c�lula vai definir o dado como macho, se for <0.5 vai definir como f�mea. 
test$predict<-rep('Female',46)
test$predict[test$prob>0.5]<-"Male"

# Por fim, testamos a efic�cia do modelo calculando frequ�ncia com que o modelo conseguiu acertar o sexo do indiv�duo. Mais a adiante vamos criar uma estrutura que vai refazer esse procedimento varias vezes, para estimar a precis�o do nosso modelo.  
mean(test$predict==test$Sex)

# Uma maneira visual de enxergar isso � fazer uma tabela que mostrando quantas vezes uma f�mea foi corretamente identificada como f�mea, quantas vezes a f�mea foi erroneamente identificada, quantas vezes um macho foi corretamente identificado como macho, quantas vezes um macho foi erroneamente identificado. 
table(test$predict,test$Sex)

# Agora por curiosidade, vamos repetir o mesmo procedimento com os dados usados na cria��o do modelo
train$prob<-predict(fit, type = 'response')
train$predict<-rep('Female',123)
train$predict[train$prob>0.5]<-"Male"
mean(train$predict==train$Sex)
table(train$predict,train$Sex)

# Veja que curiosamente nosso modelo foi melhor para prever os dados do conjunto de dados de teste, que do proprio conjunto de dados que deu origem ao modelo. 

######################################
#####  K-fold cross validation   #####
######################################
# K-fold cross validation - modelo log�stico
# Agora vamos iniciar a valida��o cruzada k-fold, para testar o qu�o bom nosso modelo � para prever o sexo com base num conjunto de vari�veis. A valida��o cruzada k-fold n�o funciona para vari�veis respostas categ�ricas. Vamos transformar os dados, de forma que machos sejam codificados como 1 e f�meas como 0. Ou seja, � a vari�vel resposta � 1=SIM ou 0=N�O para o sexo masculino. 
# Nesse passo, vamos criar um conjunto de dados chamado "my.cv" com uma coluna chamada "y" para receber o codigo do sexo (0 ou 1), e vamos remover as colunas:
#  .	Sex: representa categoricamente o sexo, e s�o dados que vieram do banco de dados original
#  .	Prob: representa a probabilidade do indiv�duo ser do sexo masculino, e s�o dados que vieram da aplica��o do modelo para prever o sexo
#  .	Predict: representa categoricamente o sexo, e s�o dados que vieram da aplica��o do modelo para prever o sexo
train$y<-rep(0,123)
train$y[train$Sex=="Male"]=1
my.cv<-train[,-c(1,7,8)]


# O codigo para o K-fold analysis �:
bestglm(Xy=my.cv,IC="CV",CVArgs = list(Method="HTF",K=10,REP=1),family = binomial)

# Esse resultado confirma o que deu anteriormente, que apenas a vari�vel Height prev� o sexo do indiv�duo. 

# O tutorial continua com mais informacoes de como fazer a valida��o, mas faltou referencias teoricas, e n�o senti confian�a nas proximas etapas. 
# Acredito que at� aqui temos uma boa nocao do que se trata

