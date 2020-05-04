#install.packages("readxl")
#install.packages("dplyr")
#install.packages("nlme")
#install.packages("plm")
#install.packages("stargazer")
#install.packages("coeftest")
#install.packages("cbind")
#install.packages("lme4")
#install.packages("DataExplorer")
#install.packages("plyr")
#install.packages("gplots")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("gplots")
#install.packages("car")
#install.packages("apsrtable")
#install.packages("caret")
#install.packages("summarytools")
library("summarytools")
library("DataExplorer")
library("readxl")
library("dplyr")
library("nlme")
library("plm")
library("stargazer")
library("lme4")
library("tidyverse")
library("dplyr")
library("gplots")
library("car")
library("apsrtable")
library("rgl")
library("plm")
library("caret")
library("mgcv")

############



Data <- read_excel("Base-de-Datos.xlsx", sheet = "BD")
glimpse(Data)



#Creación de Variables
Data_p <- Data
Data_p$utilidad <- Data_p$Ingresos - Data_p$Costo_ventas
Data_p$utilidadPer <- Data_p$utilidad / Data_p$Ingresos







#Tratando Missing Values
plot_missing(Data_p)






Missing_values_per_NIT <- Data_p %>% 
  group_by(NIT) %>% 
  summarise(count = sum(is.na(Ingresos))) %>%
  filter(count == 1) %>%
  select(NIT)







Missing_values_per_NIT <- Missing_values_per_NIT[['NIT']]







Datap_final <- Data_p[ ! Data_p$NIT %in% Missing_values_per_NIT, ]







#Validadmos nuevamente el % de Missing values
plot_missing(Datap_final)







#Eliminamos empresas que tienen utilidades negativas
Utilidad_Negativa <- Datap_final %>% 
  group_by(NIT) %>% 
  summarise(count = sum(utilidadPer <0)) %>%
  filter(count > 0) %>%
  select(NIT)






#Crecion de la base de datos final
Utilidad_Negativa <- Utilidad_Negativa[['NIT']]
Datap_final <- Datap_final[ ! Datap_final$NIT %in% Utilidad_Negativa, ]






#Analisis de la correlacion entre las variables
Data_filtered <- Datap_final[ ,!(colnames(Datap_final) %in% c("Year", "NIT", "Costo_ventas", "Ingresos", "utilidad"))]
#summary(Data_filtered)



summarytools::descr(Data_filtered, transpose = TRUE)
plot_histogram(Data_filtered)





#Graficas de las variables






plotmeans(utilidadPer ~ NIT, main="Heterogeineity across NIT", data=Datap_final)
plotmeans(utilidadPer ~ Year, main="Heterogeineity across Years", data=Datap_final)
plotmeans(utilidadPer ~ PIB, main="Heterogeineity across NIT", data=Datap_final)
plotmeans(utilidadPer ~ Inflacion, main="Heterogeineity across NIT", data=Datap_final)
p <-ggplot(data=Datap_final, aes(x = Year, y = utilidadPer, group = NIT))
p + geom_line()




plot(Data_filtered)
plot_correlation(Data_filtered, type = 'continuous','Review.Date')

#Modelo OLS 
ols <-lm(utilidadPer ~  PIB+Inflacion, data=Datap_final)
summary(ols)
yhat0 <- ols$fitted
print(summary(ols))


#Fixed model con las dos variables consideradas (PIB, Inflacion)
fixed.dum1 <-lmer(utilidadPer~ PIB +Inflacion+ (1|NIT) , data=Datap_final)
summary(fixed.dum1)
coef(fixed.dum1)
yhat1 <- fitted(fixed.dum1)

scatter3d(yhat1 ~ PIB +Inflacion , data=Datap_final,groups=factor(Datap_final$NIT),surface.col = 1:18, boxplots=FALSE, smooth=TRUE, reg.line=FALSE)


#Mixed effect model con las dos variables considerdas(PIB+Inflacion) permitiendo que los coeficientes puedan variar por cada empresa
mixed <- lmer(utilidadPer ~ PIB +Inflacion + (1 + PIB + Inflacion | NIT), data = Datap_final, REML = FALSE)
summary(mixed)
coef(mixed)
yhat2<-fitted(mixed)


#Graficas de los punto predictos por los 3 modelos comparados con los valores reales
plot(Datap_final$utilidadPer,yhat0,col = "green")
plot(Datap_final$utilidadPer,yhat1,col = "blue")
plot(Datap_final$utilidadPer,yhat2,col = "red")

#Calculo del los errores por cada modelo
y0 <- sum((Datap_final$utilidadPer-yhat0)^2)
y1 <- sum((Datap_final$utilidadPer-yhat1)^2)
y2 <- sum((Datap_final$utilidadPer-yhat2)^2)

#Resultados de los errores de cada modelo
print(y0)
print(y1)
print(y2)


