setwd("D:/Nuvem/Dropbox/Artigos em trabalho/2020 Fogo vs display/Dados/Distancia borda/Calc distancia ate a borda")
rm(list=ls())

library(geosphere)
## Introduzir poligonos das areas
line_fogo     <- read.table(file = "Area_fogo.txt", header = TRUE)
line_geo      <- read.table(file = "Area_GEO_GoogleEarth.txt", header = TRUE)


## Introduzir dados de coordenadas dos displays
# Todos
Display_GPS<- read.table(file = "Pontos_Display.txt", header = TRUE)
# Separar dados da geo
dados_geo<-subset(Display_GPS, Display_GPS$Area=="geo")
pnts_geo<-dados_geo[,3:4]


#### Calcular distancia a borda da area fogo
d_fogo = dist2Line(pnts_geo, line_fogo)
d_fogo<-cbind(d_fogo,dados_geo[,1])
colnames(d_fogo)<-c("Distancia","Lon","Lat","ID_Displays")
write.table(d_fogo,   file = "RESPOSTA_Distancias Borda Fogo.csv" , sep = ",", col.names = NA, qmethod = "double")


# Fogo
plot( makeLine(line_geo), type='l')
points(line_fogo, type='l',col="red")
points(pnts_geo, col='blue', pch=20)