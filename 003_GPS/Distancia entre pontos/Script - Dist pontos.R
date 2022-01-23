setwd("D:/Profissional/Bioestatistica no R/GPS/Distancia entre pontos")
rm(list=ls())

library(geosphere)

temp_2018<- read.table(file = "DADOS_2018.txt", header = TRUE, row.names = 1)
temp_2019<- read.table(file = "DADOS_2019.txt", header = TRUE, row.names = 1)
dados<-rbind(temp_2018,temp_2019)


distancias<-distm(dados, fun = distHaversine)

write.table(distancias,   file = "RESPOSTA_Tudo junto.csv" , sep = ",", col.names = NA, qmethod = "double")



################## Calcular quem está fora do raio de x metros
tabela_para_analise<-temp_2019
raio<-50
matriz_resposta<-matrix(NA,ncol=2,nrow=length(tabela_para_analise[,1]))
colnames(matriz_resposta)<-c("Ninho","Dentro_raio_50")
matriz_resposta[,1]<-rownames(tabela_para_analise)


pb <- txtProgressBar(min = 0, max = length(tabela_para_analise[,1]), style = 3)
for(i in 1:length(tabela_para_analise[,1])){
  for(k in 1:length(tabela_para_analise[,1])){  
    if(!i==k){  
    if( distm(rbind(tabela_para_analise[i,],tabela_para_analise[k,]), fun = distHaversine)[2,1]<raio){
      matriz_resposta[i,2]<-paste0(matriz_resposta[i,2]," ; ",rownames(tabela_para_analise[k,]))
    }}}
  setTxtProgressBar(pb, i)
  }


write.table(matriz_resposta,   file = "RESPOSTA_Maiores_2019.csv" , sep = ",", col.names = NA, qmethod = "double")


