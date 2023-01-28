install.packages("devtools")  # instala paquete devtools
devtools::install_github("dgonxalex80/paqueteMET") # descarga de paqueteMET

library(paqueteMET)     # activar paqueteMET
library(dplyr)
library(tidyverse)

data("CarreraLuz22")    # cargar la dataset

install.packages("remotes")  
install.packages("flexdashboard")
remotes::install_github("rstudio/bslib")  # descarga paquete que contiene formato   

dim(CarreraLuz22)[1]

head(CarreraLuz22)

table(CarreraLuz22$categoria)

table(CarreraLuz22$nacionalidad)

sexs = prop.table(table(CarreraLuz22$sex))*100
round(sexs[1],1)



hist(CarreraLuz22$edad)


library(plyr)
mu <- ddply(CarreraLuz22, "sex", summarise, grp.mean=mean(edad))

ggplot(CarreraLuz22, aes(x=edad, fill=sex, color=sex)) +
  theme_classic() + geom_histogram(position="identity", alpha = 0.5)+
  labs(x="Edad", 
       y="Frecuencia") + 
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")




ggplot(data = CarreraLuz22) +
  geom_density(aes(x=timerun/60,fill=factor(sex)),bins=10, position = "identity",alpha = 0.5) +
  theme_classic()
  

ggplot(data = CarreraLuz22) + geom_boxplot(aes(x=factor(sex),
                                           y=timerun/60,
                                           fill=factor(sex)),
                                           outlier.color = "red",
                                           outlier.shape = 16,
                                           outlier.size = 1) + coord_flip() + theme_classic() +
  geom_jitter(aes(x = sex, y = timerun/60), 
              size = 2,
              alpha = 0.05,
              width = 0.1)+
  labs(x="Genero", 
       y="Tiempo (min)",
       color=NULL)+scale_fill_manual(values=c("cornflowerblue", "salmon")) +
  theme(legend.position="none")


table(CarreraLuz22$origen)


CarreraLuz22$origen <- replace(CarreraLuz22$origen,CarreraLuz22$origen== "Bogotá", "Bogota")
CarreraLuz22$origen <- replace(CarreraLuz22$origen,CarreraLuz22$origen== "Bogota D.c", "Bogota")

CarreraLuz22$origen <- replace(CarreraLuz22$origen, "Cali Valle", "Cali")
CarreraLuz22$origen <- replace(CarreraLuz22$origen, "Florida Va", "Florida")



ft2=table(CarreraLuz22$origen)%>% 
  as.data.frame()
colnames(ft2)=c("Ciudad", "Frecuencia absoluta")
ft2=flextable(ft2)

set_flextable_defaults(
  font.size = 15, font.family = "Helvetica",
  font.color = "#333333",
  table.layout = "fixed",
  border.color = "gray",
  padding.top = 3, padding.bottom = 3,
  padding.left = 4, padding.right = 4)

ft2 <- ft2 %>%
  width(j = 1,  width = 5) %>%
  width(j = 2, width = 3)
ft2

ft2



ft2=table(CarreraLuz22$origen)%>% 
  as.data.frame()
colnames(ft2)=c("Ciudad", "Frecuencia absoluta")


ft2

ft2 %>%
  DT::datatable(escape = True, options = list(paging=FALSE))


####3 hombre

dataH = subset(CarreraLuz22, CarreraLuz22$sex=="Hombre")


ggplot(dataH, aes(y=timerun/60 , x=categoria))+
  geom_point() + theme_classic() + scale_fill_manual(values=c("salmon", "cornflowerblue")) +
  labs(x="Categoria de los participantes", 
       y="Tiempo (min)",
       color=NULL)




ggplot(data = dataH) + geom_boxplot(aes(x=factor(categoria),
                                               y=timerun/60,
                                               fill=factor(categoria)),
                                           outlier.color = "red",
                                           outlier.shape = 16,
                                           outlier.size = 1,
                                           alpha=0.5) + theme_classic() + #coord_flip() +
  geom_jitter(aes(x = categoria, y = timerun/60), 
              size = 2,
              alpha = 0.05,
              width = 0.1)+
  labs(x="Categoria de los competidores", 
       y="Tiempo (min)",
       color=NULL)+#scale_fill_manual(values=c("salmon", "cornflowerblue")) +
  theme(legend.position="none")




muH <- ddply(dataH, "categoria", summarise, grp.mean=mean(timerun/60))

ggplot(dataH, aes(x=timerun/60, fill=categoria, color=categoria)) +
  theme_classic() + geom_histogram(position="identity", alpha = 0.5)+
  labs(x="Tiempo (min)", 
       y="Frecuencia") 



+ 
  geom_vline(data=mu, aes(xintercept=grp.mean, color=categoria),
             linetype="dashed")








##############################
ggplot(CarreraLuz22, aes(y=edad , x=timerun/60, col=factor(sex)))+
  geom_point() + theme_classic() + scale_fill_manual(values=c("salmon", "cornflowerblue")) +
  labs(x="Tiempo (min)", 
       y="Edad",
       color=NULL)




ft1=table(CarreraLuz22$categoria)%>% 
  as.data.frame()
colnames(ft1)=c("Categoría", "Frecuencia absoluta")
ft1=flextable(ft1)

set_flextable_defaults(
  font.size = 12, font.family = "Helvetica",
  font.color = "#333333",
  table.layout = "fixed",
  border.color = "gray",
  padding.top = 3, padding.bottom = 3,
  padding.left = 4, padding.right = 4)

ft1

ft1 <- ft1 %>%
  width(j = 1,  width = 50) %>%
  width(j = 2, width = 3)
ft1















ggplot(data = CarreraLuz22) + geom_boxplot(aes(x=factor(sex), y=edad)) + coord_flip()




ggplot(CarreraLuz22, aes(edad, timerun/60)) + geom_point(aes(colour = factor(sex)))


ggplot(CarreraLuz22) +
  geom_point(aes(x = edad, y = timerun/60, col = sex))



conteo <- table(CarreraLuz22$sex,CarreraLuz22$categoria)

barplot(conteo, main="Número de atletas por categoria y sexo",  
        xlab="Categorías",
        col=c("#0d3b66","#f4d35e"),
        legend = rownames(conteo),
        las=1, ylim = c(0,1200), 
        names.arg=c("Juvenil","Abierta","Veteranos A","Veteranos B","Veteranos C"))



dataFc4=subset(CarreraLuz22, (CarreraLuz22$sex=="Mujer" & CarreraLuz22$categoria=="4. Veteranos B")) # dataset de mujeres
x=dataFc4$timerun/60
hist(x, xlim=c(40,100), las=1,
     main = "Distribución de los tiempos mujeres Veteranas B",
     xlab="tiempo (min)",
     ylab = "frecuencia",
     col ="#ee964b")



par(mar = c(5, 4, 4, 1)+0.5)  # margenes de la gráfica
plot(density(x), lwd=3, col="#0d3b66",
     main = "Distribución del tiempo - mujeres categoría Veteranas B",
     xlab="tiempo (min)",
     ylab = "densidad", las=1,
     cex.lab=1,  # tamaño de etiqueta ejes
     cex.axis=.8, # tamaño escalas de los ejes 
     cex.main=1, # tamaño del titulo
     cex.sub=1)    # tamaño del subtitulo)


boxplot(CarreraLuz22$timerun/60~CarreraLuz22$sex,
        main = "Distribución del tiempo carrera La Luz 2022",
        ylab="tiempo (min)",
        xlab = "sexo", las=1,
        col=c("#f4d35e","#ee964b"))



y=ts(inflacion2022$inflacion, star=c(1993,1), end=c(2022,12), frequency=12)
plot(y, type="l",
     main="Inflación Colombia  ene-1993 a dic-2022",
     ylab="inflación acumulada anual (%)",
     xlab = "meses", las=1,
     col="#ee964b", 
     lwd = 4)



