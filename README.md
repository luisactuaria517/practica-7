# practica-7
# metodo ingenuo

# descomposicion vlasica
# se utilisa la funcion descompose
# otro tipo de descomposicion de la estacionariedad
# y tendencia es la descomposicion por loess la cual esta
# basada en un algoritmo que permite descomponer las series de tiempo en 
# 3 elementos: estacionariedad, tendencia y aleatoriedad, uno de las 
# ventajas de esta descomposicion es el calculo de la estacionariedad 
# se obtienen velores de estacionariedad no vonstante, en contraste con el 
# metodo clasico de descomposicion

tar2<-read.csv("C:\\Users\\luisa\\OneDrive\\Documentos\\r series de tiempo\\tarea2.csv")
tar2ts<-ts(tar2,frequency = 1, start = 2005)
tar2ts
tar2
class(tar2)
des<-ts(tar2,frequency = 4, start = 2005)
class(des)
desco<-decompose(des)
tend1<-desco$trend
plot(tend1,col="blue",main="tendencia desocupacion", ylab="tasas",xlab="años")
lines(tend2,col="red")

####### DESCOMPOSICION TENDENCIA Estacional por LOESS
# se utilisa la funcion stl()

des1<-ts(tar2[1:48,1],frequency = 4, start = 2005)
desco2<-stl(des1,s.window = "periodic",robust = T)
names(desco2)
desco2
desco$trend
desco2$weights
tend2<-desco2$time.series[,2]
tend2
esta2<-desco2$time.series[,1]
esta2


pibm<-read.csv("C:\\Users\\luisa\\OneDrive\\Documentos\\r series de tiempo\\pib.csv")
pibd<-ts(pibm[1:36,1],frequency = 4, start = 2007)
de<-stl(pibd,s.window = "periodic",robust = T)
names(de)
de
de$weights
tend3<-de$time.series[,2]
tend3
esta3<-de$time.series[,1]
esta3

class(pibd)
dis<-decompose(pibd)
tend4<-dis$trend

plot(tend4,col="blue",main="tendencia desocupacion", ylab="tasas",xlab="años")
lines(tend3,col="red")
install.packages("fpp")
require(fpp)
ajus<-seasadj(desco2)# ajusta estqacionariedad 
##### Pronostico metodo ingenuo
inge1<-naive(ajus, h=4)
plot(inge1)
inge1
