datacenso = read.csv('C:/Users/kevin/OneDrive/Documentos/2. Repo/1. MIIC/fpgrowth/PERSONA_BDP.csv', sep=',')
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

subs <- subset(datacenso, NIVGRADO >=51)

#arbol <- rpart(var_a_predecir ~ var_1+var_2+var_3, data=datacenso, method = "class")

arbol <- rpart(PCP30_2D ~ PCP6+PCP7+NIVGRADO+ANEDUCA, data=datacenso, method = "class")

rpart.plot(arbol,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Depto. de nacimiento",
           cex = 0.45)  

arbol2 <- rpart(PCP35_A ~ PCP6+PCP7+NIVGRADO+ANEDUCA, data=subs, method = "class")
rpart.plot(arbol2,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "#hijos",
           cex = 0.45)  

arbol3 <- rpart(PCP35_A ~ PCP6+PCP7+NIVGRADO+MUNICIPIO, data=datacenso, method = "class")

rpart.plot(arbol3,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "#hijos general",
           cex = 0.45)  

new_data <- data.frame(
  PCP6=c(2),
  PCP7=c(35),
  NIVGRADO=c(51),
  ANEDUCA=c(25)
)

res <- predict(arbol2, new_data, type ="class")
print(res)

arbol4 <- rpart(NIVGRADO~ PCP6+PCP7+PCP12+PCP13, data=datacenso, method = "class")
rpart.plot(arbol4,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Grado academico partiendo de pueblos y lenguas",
           cex = 0.45) 

subs <- subset(datacenso, NIVGRADO >=71)


arbol5 <- rpart(PCP7~ PCP6+NIVGRADO, data=datacenso, method = "class")

rpart.plot(arbol5,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Grado academico partiendo de pueblos y lenguas",
           cex = 0.45) 

arbol6 <- rpart(PCP7~ PCP6+NIVGRADO, data=subs, method = "class")

rpart.plot(arbol6,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Grado academico partiendo de pueblos y lenguas",
           cex = 0.45) 

arbol7 <- rpart(PCP34~ PCP7+PCP6+NIVGRADO, data=datacenso, method = "class")
rpart.plot(arbol7,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Grado academico partiendo de pueblos y lenguas",
           cex = 0.45) 

#CENSO DE VIVIENDA
vivienda = read.csv('C:/Users/kevin/OneDrive/Documentos/2. Repo/1. MIIC/arbol/VIVIENDA_BDP.csv', sep=',')

subv <- subset(vivienda, MUNICIPIO ==101)

arbol8 <- rpart(PCV1~ ZONA, data=vivienda, method = "class")


rpart.plot(arbol8,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo de vivienda nacional",
           cex = 0.45) 

arbol9 <- rpart(DEPARTAMENTO~ PCV1+PCV2+PCV3+PCV4+PCV5, data=vivienda, method = "class")

rpart.plot(arbol9,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 


subv2 <- subset(vivienda, DEPARTAMENTO ==8)

arbol9 <- rpart(MUNICIPIO~ PCV1+PCV2+PCV3+PCV4+PCV5, data=subv2, method = "class")

rpart.plot(arbol9,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 

subv3 <- subset(vivienda, DEPARTAMENTO ==9)

arbol10 <- rpart(MUNICIPIO~ PCV1+PCV2+PCV3+PCV4+PCV5, data=subv3, method = "class")

rpart.plot(arbol10,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 


subv4 <- subset(vivienda, DEPARTAMENTO ==3)

arbol11 <- rpart(MUNICIPIO~ PCV1+PCV2+PCV3+PCV4+PCV5, data=subv4, method = "class")

rpart.plot(arbol11,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 

arbol12 <- rpart(PCV1~ MUNICIPIO+PCV2+PCV3+PCV4+PCV5, data=vivienda, method = "class")

rpart.plot(arbol12,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 

subv5 <- subset(vivienda, DEPARTAMENTO ==1)

arbol13 <- rpart(PCV1~ ZONA+PCV2+PCV3+PCV4+PCV5, data=subv5, method = "class")

rpart.plot(arbol13,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 


subv5 <- subset(vivienda, MUNICIPIO ==101)

arbol13 <- rpart(PCV1~ ZONA+PCV2+PCV3+PCV4+PCV5, data=subv5, method = "class")

rpart.plot(arbol13,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 

arbol15 <- rpart(ZONA~ PCV1+PCV2+PCV3+PCV4+PCV5, data=subv5, method = "class")

rpart.plot(arbol15,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 

subv6 <- subset(vivienda, MUNICIPIO ==301)

arbol14 <- rpart(PCV1~ ZONA+PCV2+PCV3+PCV4+PCV5, data=subv6, method = "class")

rpart.plot(arbol14,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 


subv6 <- subset(vivienda, DEPARTAMENTO ==5)

arbol14 <- rpart(PCV1~ PCV2+PCV3+PCV4+PCV5, data=subv6, method = "class")

rpart.plot(arbol14,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45)


subv6 <- subset(vivienda, DEPARTAMENTO ==9)

arbol14 <- rpart(PCV1~ PCV2+PCV3+PCV4+PCV5, data=subv6, method = "class")

rpart.plot(arbol14,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 

subv6 <- subset(vivienda, DEPARTAMENTO ==2)

arbol14 <- rpart(PCV1~ PCV2+PCV3+PCV4+PCV5, data=subv6, method = "class")

rpart.plot(arbol14,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 

subv6 <- subset(vivienda, DEPARTAMENTO ==18)

arbol14 <- rpart(PCV1~ PCV2+PCV3+PCV4+PCV5, data=subv6, method = "class")

rpart.plot(arbol14,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 

subv6 <- subset(vivienda, DEPARTAMENTO ==16)

arbol14 <- rpart(PCV1~ PCV2+PCV3+PCV4+PCV5, data=subv6, method = "class")

rpart.plot(arbol14,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 

subv6 <- subset(vivienda, MUNICIPIO ==1601)

arbol14 <- rpart(PCV1~ PCV2+PCV3+PCV4+PCV5, data=subv6, method = "class")

rpart.plot(arbol14,
           type = 2,
           extra = 0,  
           under = TRUE,  
           fallen.leaves = TRUE,  
           box.palette = "BuGn",  
           main = "Tipo dada su construcción",
           cex = 0.45) 

new_data2 <- data.frame(
  PCV2=c(9),
  PCV3=c(8),
  PCV4=c(2),
  PCV5=c(4)
)

res <- predict(arbol14, new_data2, type ="class")
print(res)
