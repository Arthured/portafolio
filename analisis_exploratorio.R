#Tome una conjunto de datos con informacion sobre steam
#Analizare algunas compañias su ranking de juegos, su tiempo jugado en total de todos los juegos

library(tidyverse)
df1<-read.csv("C://Users//artur//OneDrive//Documentos//uni//7mo//Terminal//Base_de_datos//game_data_all.csv")
activision<-subset(df1,publisher=="Activision")
View(activision)
ranking<-activision$rating
#5 numeros y la media
summary(ranking)
#histograma
hist(ranking,xlab="porcentaje",main="Ranking de activision",col=rainbow(10))
n<-c(1:length(ranking))
#Dispersion de las calificaciones a los juegos
ggplot(activision,
       aes(y=rating,x=n,col="blue"))+
       geom_point()
#boxplot de la calificacion de los juegos
ggplot(activision,
       aes(y=rating))+
       geom_boxplot()  
ran_ecdf<-ecdf(ranking)
plot(ran_ecdf, verticals=TRUE, do.points=FALSE, main="distribucion empirica de las calificaciones",
    xlab="calificacion del juego", ylab=" funcion de distribución empirica",
    col="black")
#intervalo de confianza al 95%
alpha <-0.05
en <- sqrt(log(2/alpha)/(2*n))
L_DKW <- pmax(ran_ecdf(ranking)-en,0)
U_DKW <- pmin(ran_ecdf(ranking)+en,1)
points(sort(ranking), L_DKW[order(ranking)], "l", col="red")
points(sort(ranking), U_DKW[order(ranking)], "l", col="red")
lines(c(.5,.5), c(0,1))