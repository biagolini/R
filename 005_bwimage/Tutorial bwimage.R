# Limpar a mem�ria do R
rm(list=ls())
gc()

# Definir diret�rio de trabalho
# Op��o 1 - passar o path na m�o
setwd("D:/AmbienteDesenvolvimento/Tutorial bwimage/")
# Op��o 2 - usar uma tela de navega��o
setwd(choose.dir())

# Instalar os pacotes necess�rios - Se j� tiver instalado pode pular essa parte
install.packages("bwimage")
install.packages("openxlsx")

# Carregar pacotes necess�rios
library(bwimage)
library(openxlsx)


# Carregar uma lista com o nome das figuras
lista_arquivos<- read.xlsx("Lista_figuras.xlsx", sheet = 1,colNames = FALSE)

# Criar um objeto com a lista de nome de arquivos
lista_imagens<-as.character(lista_arquivos[,1])

# Converter as imagens em matrizes binarias. Essa parte pode demorar, tenha paci�ncia.  
lista_matizes<-threshold_image_list(lista_imagens, filetype ="jpg",compress_method ="frame_fixed", target_width=1000, target_height=1000)

# Criar matrizes em branco para enviar o resultado da an�lise de dados
matriz_resposta<-matrix(NA,ncol=2,nrow=length(lista_imagens))
colnames(matriz_resposta)<-c("DV","Agregacao")

#  Loop para an�lise das figuras em cada conjunto de imagens. Essa parte vai demorar ainda mais que a de antes, aqui o hardware do seu computador ser� levado ao m�ximo que o R consegue usar. Minha experi�ncia com R, diz que normalmente o gargalo est� na velocidade da sua mem�ria Ram. Por isso, �s vezes um computador mais novo com uma mem�ria mais r�pida, pode rodar a an�lise mais r�pida que em um computador antigo, mes-mo que o antigo tenha um processador potente.   
for(i in 1:length(lista_imagens)){
  matriz_resposta[i,1]<-denseness_total(lista_matizes[[i]])
  matriz_resposta[i,2]<-aggregation_index(lista_matizes[[i]])[1]
}
# Exportar resultados 
resultado_completo<-as.data.frame(matriz_resposta)
resultado_completo$Identificacao<-lista_arquivos[,1]
write.xlsx(resultado_completo, file = "Resultado completo.xlsx", overwrite=TRUE)

# Aqui j� temos uma tabela com o resultado de DV e agrega��o de todas as imagens, agora vamos aproveitar e resumir os dados.
n_figuras<-5 # numero de figuras por ninho
matriz_media5<-matrix(NA,ncol=3,nrow=length(matriz_resposta[,1])/n_figuras)
colnames(matriz_media5)<-c("Identifica��o do ponto","DV","Agregacao")

for( i in 1: length(matriz_media5[,1])){
  matriz_media5[i,2:3]<-apply(matriz_resposta[((5*i)-4):(5*i),],2 ,mean)
  matriz_media5[i,1]<-lista_arquivos[,1][(5*i)-4]
}

# Exportar resultados resumidos
matriz_media5<-data.frame(matriz_media5)
write.xlsx(matriz_media5, file = "Resultado resumido.xlsx", overwrite=TRUE)


#### NOTA
# Por limita��es do R, se sua lista de imagens for grande, o R n�o vai conseguir lhe dar com as listas de matrizes (porque existe um tamanho m�ximo de arquivo que o R consegue trabalhar, e sua lista com dados de todas as figuras pode ultrapassar esse limite).
# Isso aconteceu comigo, nas an�lises da minha tese (os artigos que indiquei no in�cio do tutorial s�o cap�tulos da tese). Para solucionar esse problema, eu dividi a lista de arquivos em grupos de 300 imagens. Dependendo dos valores que voc� escolher em "target_width" e "target_height", voc� pode mudar o tamanho dos grupos (quanto maior o target, maior o tamanho da matriz de 1s e 0s, portanto menos imagens podem ser colocadas em cada grupo. Neste novo cen�rio, as analises ficariam assim:
# Particionar as figuras em grupos de at� 300s imagens.
lista_imagens1<-as.character(lista_arquivos[1:300,1])
lista_imagens2<-as.character(lista_arquivos[301:600,1])
lista_imagens3<-as.character(lista_arquivos[601:900,1])
lista_imagens4<-as.character(lista_arquivos[901:1120,1])

# Converter as imagens em matrizes binarias
lista_matizes1<-threshold_image_list(lista_imagens1, filetype ="jpg",compress_method ="frame_fixed", target_width=1000, target_height=1000)
lista_matizes2<-threshold_image_list(lista_imagens2, filetype ="jpg",compress_method ="frame_fixed", target_width=1000, target_height=1000)
lista_matizes3<-threshold_image_list(lista_imagens3, filetype ="jpg",compress_method ="frame_fixed", target_width=1000, target_height=1000)
lista_matizes4<-threshold_image_list(lista_imagens4, filetype ="jpg",compress_method ="frame_fixed", target_width=1000, target_height=1000)

# Criar matrizes em branco para o resultado da an�lise de dados
matriz_resposta1<-matrix(NA,ncol=2,nrow=length(lista_imagens1))
matriz_resposta2<-matrix(NA,ncol=2,nrow=length(lista_imagens2))
matriz_resposta3<-matrix(NA,ncol=2,nrow=length(lista_imagens3))
matriz_resposta4<-matrix(NA,ncol=2,nrow=length(lista_imagens4))
colnames(matriz_resposta4)<-colnames(matriz_resposta3)<-colnames(matriz_resposta2)<-colnames(matriz_resposta1)<-c("DV","Agregacao")
rownames(matriz_resposta1)<-lista_imagens1
rownames(matriz_resposta2)<-lista_imagens2
rownames(matriz_resposta3)<-lista_imagens3
rownames(matriz_resposta4)<-lista_imagens4

# Loop para an�lise das figuras em cada conjunto de imagens
for(i in 1:length(lista_matizes1)){
  matriz_resposta1[i,1]<-denseness_total(lista_matizes1[[i]])
  matriz_resposta1[i,2]<-aggregation_index(lista_matizes1[[i]])[1]
}

for(i in 1:length(lista_matizes2)){
  matriz_resposta2[i,1]<-denseness_total(lista_matizes2[[i]])
  matriz_resposta2[i,2]<-aggregation_index(lista_matizes2[[i]])[1]
}

for(i in 1:length(lista_matizes3)){
  matriz_resposta3[i,1]<-denseness_total(lista_matizes3[[i]])
  matriz_resposta3[i,2]<-aggregation_index(lista_matizes3[[i]])[1]
}

for(i in 1:length(lista_matizes4)){
  matriz_resposta4[i,1]<-denseness_total(lista_matizes4[[i]])
  matriz_resposta4[i,2]<-aggregation_index(lista_matizes4[[i]])[1]
}

resultado_final<-rbind(matriz_resposta1,matriz_resposta2,matriz_resposta3,matriz_resposta4)
