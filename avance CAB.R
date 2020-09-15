library(readr)
library(dplyr)

install.packages("gt")
library(gt)
library(reshape2)
library(tidyr)
library(MASS)
library(dplyr)

install.packages("e1071")
library(e1071)

install.packages("writexl")
library("writexl")

pkg<-list("rvest","tidyverse")
lapply(pkg, require, character.only=T)
rm(pkg)


###hacer el proceso de webscraping para cargar la tabla de NYc año 2020###
url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=NEW+YORK&year=2020"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex5 <- html_table(tmp[[1]])
n5=dim(ibex5)[1]
year = rep(2020,n5)
ibex5 <- cbind(ibex5,year)
class(ibex5)  
glimpse(ibex5)


Base_trabajo<-ibex5
class(Base_trabajo)  
glimpse(Base_trabajo)


Base_trabajo$`BASE SALARY` <- gsub(","," ",Base_trabajo$`BASE SALARY`)
Base_trabajo$`BASE SALARY` <- gsub(" ","",Base_trabajo$`BASE SALARY`)
Base_trabajo$`BASE SALARY` <- as.numeric(Base_trabajo$`BASE SALARY`)
Base_trabajo$aplicaciones<-1
glimpse(Base_trabajo)

mode(Base_trabajo) # Indica como se encuentra almacenada la informaci?n en este elemento
str(Base_trabajo) # Informaci?n de la estructura de un grupo de datos 
names(Base_trabajo)
Base_trabajo$jahr<-Base_trabajo$year
Base_trabajo$year <- NULL
names(Base_trabajo)
dim(Base_trabajo)

?group_by

is.na(Base_trabajo$`BASE SALARY`)
which(is.na(Base_trabajo$`BASE SALARY`))
?complete.cases
complete.cases(Base_trabajo$`BASE SALARY`)
sum(complete.cases(Base_trabajo$`BASE SALARY`))

summary(Base_trabajo)
summary(datosny)



###CON LA BASE lista la colapsamos por compañia para hacer el analísis de EB###

Base_trabajo_collapsed<-Base_trabajo %>% group_by(EMPLOYER) %>% summarise(
  mean_wage = mean(`BASE SALARY`), sd_wage = sd(`BASE SALARY`),aplicaciones = sum(aplicaciones))

Base_trabajo_collapsed$sd_wage[is.na(Base_trabajo_collapsed$sd_wage)] <- 0


write_xlsx(Base_trabajo_collapsed,"C:/Users/VSD0301/OneDrive - Universidad de los Andes/Big Data ML Applied Economics/Taller1/punto2/Base_trabajo_collapsed.xlsx")




complete.cases(Base_trabajo_collapsed$mean_wage)
sum(complete.cases(Base_trabajo_collapsed$mean_wage))

complete.cases(Base_trabajo_collapsed$sd_wage)
sum(complete.cases(Base_trabajo_collapsed$sd_wage))

Base_trabajo_collapsed$sd_wage <- as.numeric(Base_trabajo_collapsed$sd_wage)
sapply(Base_trabajo_collapsed, mean, na.rm=TRUE)
summary(Base_trabajo_collapsed)
xhat<-summary(mydata)


###se crean las variables que serviran para calcular la posterior mean###

Base_trabajo_collapsed$sigma_squared1<-Base_trabajo_collapsed$sd_wage*Base_trabajo_collapsed$sd_wage/Base_trabajo_collapsed$aplicaciones

#se asume una segunga varianza de X q es igual al promedio de sd_wage elevado al cuadrado y dividido entre el promedio de aplicaciones de todas las empresas
Base_trabajo_collapsed$sigma_squared2<-9608*9608/4.5

Base_trabajo_collapsed$Xbar<-105119

Base_trabajo_collapsed$ximinusXbar<-Base_trabajo_collapsed$mean_wage-Base_trabajo_collapsed$Xbar

Base_trabajo_collapsed$ximinusXbar_sqrd<-Base_trabajo_collapsed$ximinusXbar*Base_trabajo_collapsed$ximinusXbar

Base_trabajo_collapsed$N<-4858-3

Base_trabajo_collapsed$a<-sum(Base_trabajo_collapsed$ximinusXbar_sqrd)


summary(Base_trabajo_collapsed)

Wage_EB<- Base_trabajo_collapsed %>%
  mutate(eb_estimate1 = ((N*sigma_squared1*Xbar/a)+(mean_wage)-(mean_wage*N*sigma_squared1/a)), eb_estimate2 = ((N*sigma_squared2*Xbar/a)+(mean_wage)-(mean_wage*N*sigma_squared2/a)), c1=(N*sigma_squared1/a), c2=(N*sigma_squared2/a))

summary(Wage_EB)

head(Wage_EB %>% arrange(eb_estimate1))



write_xlsx(Wage_EB,"C:/Users/VSD0301/OneDrive - Universidad de los Andes/Big Data ML Applied Economics/Taller1/punto2/Wage_EB.xlsx")

#####Se grafican los resultados para el análisis. El primer resultado eb_estimate1
#### se calculó con base en la varianza sigma_squared1

#########Gráfica con eb_estimate1##################


# Create plotting space and before scores
plot(x = rep(1, length(Wage_EB$mean_wage)), 
     y = Wage_EB$mean_wage, 
     xlim = c(.5, 3.5), 
     ylim = c(500, 630000),
     ylab = "Wage", 
     xlab = "Estimation",
     main = "Predicting Wage", 
     xaxt = "n")

# Add after scores
points(x = rep(2, length(Wage_EB$eb_estimate1)), y = Wage_EB$eb_estimate1)

points(x = rep(3, length(Wage_EB$Xbar)), y = Wage_EB$Xbar)


# Add connections with segments()
segments(x0 = rep(1, length(Wage_EB$mean_wage)), 
         y0 = Wage_EB$mean_wage, 
         x1 = rep(2, length(Wage_EB$eb_estimate1)), 
         y1 = Wage_EB$eb_estimate1,
         col = gray(0, .5))

segments(x0 = rep(2, length(Wage_EB$eb_estimate1)), 
         y0 = Wage_EB$eb_estimate1,
         x1 = rep(3, length(Wage_EB$Xbar)), 
         y1 = Wage_EB$Xbar,
         col = gray(0, .5))


# Add labels
mtext(text = c("MLE", "EB1", "Observed Mean"), 
      side = 1, at = c(1, 2, 3), line = 1)




###El primer resultado eb_estimate2
#### se calculó con base en la varianza sigma_squared2

#########Gráfica con eb_estimate2##################


# Create plotting space and before scores
plot(x = rep(1, length(Wage_EB$mean_wage)), 
     y = Wage_EB$mean_wage, 
     xlim = c(.5, 3.5), 
     ylim = c(500, 630000),
     ylab = "Wage", 
     xlab = "Estimation",
     main = "Predicting Wage (EB2)", 
     xaxt = "n")

# Add after scores
points(x = rep(2, length(Wage_EB$eb_estimate2)), y = Wage_EB$eb_estimate2)

points(x = rep(3, length(Wage_EB$Xbar)), y = Wage_EB$Xbar)


# Add connections with segments()
segments(x0 = rep(1, length(Wage_EB$mean_wage)), 
         y0 = Wage_EB$mean_wage, 
         x1 = rep(2, length(Wage_EB$eb_estimate2)), 
         y1 = Wage_EB$eb_estimate2,
         x2 = rep(3, length(Wage_EB$Xbar)), 
         y2 = Wage_EB$Xbar,
         col = gray(0, .5))

segments(x0 = rep(2, length(Wage_EB$eb_estimate2)), 
         y0 = Wage_EB$eb_estimate2,
         x1 = rep(3, length(Wage_EB$Xbar)), 
         y1 = Wage_EB$Xbar,
         col = gray(0, 0.5))


# Add labels
mtext(text = c("MLE", "EB2", "Observed Mean"), 
      side = 1, at = c(1, 2, 3), line = 1)

git add -A
git commit -m "Escribir comentario"
git push
















##papelera de reciclaje##
subset1<-Base_trabajo[Base_trabajo$LOCATION == 'NEW YORK, NY',]
subset2<-Base_trabajo[Base_trabajo$LOCATION == 'NEW YORK, NY'& Base_trabajo$year == 2020,]
glimpse(subset)
names(subset)


subset_collapsed<-subset %>% filter(year == max(year))

Base_trabajo %>%
  group_by(LOCATION) %>%
  summarize(mean(BASE SALARY))

grades %>%
  group_by(school, classof) %>%
  summarize(mean_sat = mean(sat_score))
##################################################################
