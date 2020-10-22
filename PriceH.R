library(readr)
library(dplyr)
library(gt)
library(reshape2)
library(tidyr)
library(MASS)
library(e1071)
library(VIM)
library(stargazer)
library(caret)
library(texreg)

setwd("C:/BIGDATA/Taller1/Talle1_1/problem-set-1-luque-beltran-bocanegra")

View(house1)
attach(house1)
names(house1)

#estadisticas descriptivas

options(scipen = 999)
descriptivas1<- house1 %>% summarise(rooms,bedrooms,bathrooms,surface_total,surface_covered,price)%>% 
  summarise_each(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE),max(.,na.rm = TRUE),
                      min(.,na.rm = TRUE),median(.,na.rm = TRUE),sum(is.na(.),na.rm = TRUE))) %>%
  t %>% as.data.frame %>% add_rownames %>%
  separate(rowname, into = c("feature", "fun"), sep = "_")%>%gt()

descriptivas1

distinct (house1, property_type)
distinct (house1, l3)



#eliminación de valores 0 en precio total 133/239343

house1<-filter (house1, house1$price!=0)

#cambio de USD a COP
house1$price<-ifelse (house1$currency=="USD", house1$price*3600,house1$price)

#inputación lotes
house1$rooms<-ifelse (house1$property_type=="Lote" & is.na(house1$rooms), 0,house1$rooms)
house1$bedrooms<-ifelse (house1$property_type=="Lote" & is.na(house1$bedrooms), 0,house1$bedrooms)
house1$bathrooms<-ifelse (house1$property_type=="Lote" & is.na(house1$bathrooms), 0,house1$bathrooms)
house1$surface_covered<-ifelse (house1$property_type=="Lote" & is.na(house1$surface_covered), 0,house1$surface_covered)
house1$surface_total<-ifelse (house1$property_type=="Oficina" & is.na(house1$surface_total), 0,house1$surface_total)
house1$surface_total<-ifelse (house1$property_type=="Apartamento" & is.na(house1$surface_total), 0,house1$surface_total)
house1$surface_total<-ifelse (house1$property_type=="PH" & is.na(house1$surface_total), 0,house1$surface_total)


#inputación hotdeck
h2 <- hotdeck(house1,variable=c("rooms","bedrooms","bathrooms","surface_total","surface_covered"),
                  domain_var=c("l3","price","property_type"),impNA = TRUE)

#inputación de la medias para los valores faltantes

h2<- h2 %>% group_by(l3,property_type) %>% mutate(rooms=ifelse(is.na(rooms),ceiling(mean(!is.na(rooms))),rooms))
h2<- h2 %>% group_by(l3,property_type) %>% mutate(bedrooms=ifelse(is.na(bedrooms),ceiling(mean(!is.na(bedrooms))),bedrooms))
h2<- h2 %>% group_by(l3,property_type) %>% mutate(bathrooms=ifelse(is.na(bathrooms),ceiling(mean(!is.na(bathrooms))),bathrooms))


#eliminación de nulos que no se pudieron imputar
h2 <- h2[!is.na(h2$surface_total),]
h2 <- h2[!is.na(h2$surface_covered),]

View (h2)
sum(is.na(h2$rooms))
sum(is.na(h2$bathrooms))
sum(is.na(h2$bedrooms))
sum(is.na(h2$surface_covered))
sum(is.na(h2$surface_total))

descriptivas2<- h2 %>% summarise(rooms,bedrooms,bathrooms,surface_total,surface_covered,price)%>% 
  summarise_each(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE),max(.,na.rm = TRUE),
                      min(.,na.rm = TRUE),median(.,na.rm = TRUE),sum(is.na(.),na.rm = TRUE))) %>%
  t %>% as.data.frame %>% add_rownames %>%
  separate(rowname, into = c("feature", "fun"), sep = "_")%>%gt()

descriptivas2
h2$lnprice<-log(h2$price)

#nuevas variables
h2$homicidios<-ifelse(h2$l2=="Cundinamarca", 1032,0)
h2$hurto<-ifelse(h2$l2=="Cundinamarca", 126687,0)
h2$arealic<-ifelse(h2$l2=="Cundinamarca", 3256328,0)
h2$nuevasconst<-ifelse(h2$l2=="Cundinamarca", 41975,0)

h2$homicidios<-ifelse(h2$l3=="Barranquilla", 272,h2$homicidios)
h2$hurto<-ifelse(h2$l3=="Barranquilla", 10786,h2$hurto)
h2$arealic<-ifelse(h2$l3=="Barranquilla", 403488,h2$arealic)
h2$nuevasconst<-ifelse(h2$l3=="Barranquilla", 5669,h2$nuevasconst)

h2$homicidios<-ifelse(h2$l3=="Cali", 1113,h2$homicidios)
h2$hurto<-ifelse(h2$l3=="Cali", 20291,h2$hurto)
h2$arealic<-ifelse(h2$l3=="Cali", 947697,h2$arealic)
h2$nuevasconst<-ifelse(h2$l3=="Cali", 9284,h2$nuevasconst)

h2$homicidios<-ifelse(h2$l2=="Antioquia", 579,h2$homicidios)
h2$hurto<-ifelse(h2$l2=="Antioquia", 26848,h2$hurto)
h2$arealic<-ifelse(h2$l2=="Antioquia", 840858,h2$arealic)
h2$nuevasconst<-ifelse(h2$l2=="Antioquia", 7154,h2$nuevasconst)

#punto 2.3

set.seed(10101)

indic<-sample(1:nrow(h2),floor(.1*nrow(h2)))

entrenamiento <- h2[indic,]
prueba <- h2[-indic,]

View(entrenamiento)
entrenamiento$lnprice<-log(entrenamiento$price)

#regresión sobre entrenamiento
m1<-lm(lnprice~1,data=entrenamiento)
stargazer(m1,type="text")

#predicción sobre prueba
#yhat<-predict(m2,prueba)
prueba$yhat<-predict(m1,prueba)
View(prueba)

#Calculation of the prediction error
prueba<- prueba %>% mutate(err=(lnprice-yhat)^2)
sqrt(mean(prueba$err))

#modelos 2
m2<-lm(lnprice~bathrooms+rooms+factor(property_type),data=entrenamiento)
stargazer(m2,type="text")
prueba$yhat_2<-predict(m2,prueba)
prueba<- prueba %>% mutate(err2=(lnprice-yhat_2)^2)
sqrt(mean(prueba$err2))

#model 3
m3<-lm(lnprice~bathrooms+rooms+bedrooms+factor(property_type)+factor(l3),data=entrenamiento)
stargazer(m3,type="text")
prueba$yhat_3<-predict(m3,prueba)
prueba<- prueba %>% mutate(err3=(lnprice-yhat_3)^2)
sqrt(mean(prueba$err3))

#model 4

prueba$lnprice<-log(prueba$price)
m4<-lm(lnprice~bathrooms+rooms+bedrooms+factor(property_type)+factor(l3)+surface_total+surface_covered,data=entrenamiento)
stargazer(m4,type="text")
prueba$yhat_4<-predict(m4,prueba)
prueba<- prueba %>% mutate(err4=(lnprice-yhat_4)^2)
sqrt(mean(prueba$err4))

#model 5
m5<-lm(lnprice~rooms+(bathrooms*bedrooms)+factor(property_type)+factor(l3)+lat+lon,data=entrenamiento)
stargazer(m5,type="text")
prueba$yhat_5<-predict(m5,prueba)
prueba<- prueba %>% mutate(err5=(lnprice-yhat_5)^2)
sqrt(mean(prueba$err5))

#model 6
m6<-lm(lnprice~rooms+(bathrooms*bedrooms)+factor(property_type)+factor(l3)+lat+lon+(surface_total)^2,data=entrenamiento)
stargazer(m6,type="text")
prueba$yhat_6<-predict(m6,prueba)
prueba<- prueba %>% mutate(err6=(lnprice-yhat_6)^2)
sqrt(mean(prueba$err6))

#model 7
m7<-lm(lnprice~rooms+factor(l3)+(bathrooms*bedrooms)+factor(property_type)+factor(l3)+lat+lon+(lat*lon)+surface_covered+surface_total,data=entrenamiento)
stargazer(m7,type="text")
prueba$yhat_7<-predict(m7,prueba)
prueba<- prueba %>% mutate(err7=(lnprice-yhat_7)^2)
sqrt(mean(prueba$err7))

#model 7.1
m71<-lm(lnprice~rooms+factor(l3)+(bathrooms*bedrooms)+factor(property_type)+factor(l3)+lat+lon+(lat*lon)+poly(surface_covered,3)+surface_total,data=entrenamiento)
stargazer(m71,type="text")
prueba$yhat_71<-predict(m71,prueba)
prueba<- prueba %>% mutate(err71=(lnprice-yhat_7)^2)
sqrt(mean(prueba$err71))

texreg::htmlreg(list(m2,m3,m4,m5,m6,m7,m71),file='models.html')

stargazer(m2,m3,m4,m5,m6,m7,m71, header=FALSE,
          type = "text",
          title="Tabla 1. Comparación de modelos",
          digits=2, single.row=FALSE,
          intercept.bottom=FALSE)

#modelo 8 nuevas variables
m8<-lm(lnprice~rooms+(bathrooms*bedrooms)+factor(property_type)+surface_total+surface_covered+(lat*lon)+hurto+arealic,data=entrenamiento)
stargazer(m8,type="text")
entrenamiento$yhat_8<-predict(m8,entrenamiento)
entrenamiento<- entrenamiento %>% mutate(err8=(lnprice-yhat_8)^2)
sqrt(mean(entrenamiento$err8))


sqrt(mean(prueba$err))
sqrt(mean(prueba$err2))
sqrt(mean(prueba$err3))
sqrt(mean(prueba$err4))
sqrt(mean(prueba$err5))
sqrt(mean(prueba$err6))
sqrt(mean(prueba$err7))
sqrt(mean(prueba$err71))
sqrt(mean(prueba$err8))


#nuevas variables

#LOOCV
error_LOOCV <- c()
for(i in 1:dim(entrenamiento)[1]){
  modelo  <- lm(lnprice~rooms+(bathrooms*bedrooms)+factor(property_type)+surface_total+surface_covered+(lat*lon)+hurto+arealic,data=entrenamiento[-i,])
    error_LOOCV[i] <- entrenamiento$lnprice[i]-predict(modelo,entrenamiento[i,])
  print(i)
}
mean(error_LOOCV*error_LOOCV)
h <- hatvalues(m2)

options(scipen=999)
medidas <- data.frame(LOOCV=error_LOOCV^2,leverage=as.numeric(h))
medidas <- data.frame(LOOCV=sqrt(mean(prueba$err2)),leverage=as.numeric(h),error_modelo_co = (entrenamiento$lnprice-predict(m2,entrenamiento)))
medidas$algo <- medidas$error_modelo_co/(1-medidas$leverage)
mean((medidas$algo^2))

View(medidas)

medidas$algo <- prueba$err/(1-h)
a<-mean((prueba$err/(1-h))^2)


#########QR
h2$property_type_Casa <- ifelse(h2$property_type=="Casa",1,0)
h2$property_type_Lote <- ifelse(h2$property_type=="Lote",1,0)
h2$property_type_Otro <- ifelse(h2$property_type=="Otro",1,0)
h2$property_type_Apto <- ifelse(h2$property_type=="Apartamento",1,0)
h2$property_type_LC   <- ifelse(h2$property_type=="Local comercial",1,0)
h2$property_type_Ofic <- ifelse(h2$property_type=="Oficina",1,0)
h2$property_type_Finc <- ifelse(h2$property_type=="Finca",1,0)
h2$property_type_Parq <- ifelse(h2$property_type=="Parqueadero",1,0)
h2$property_type_Depo <- ifelse(h2$property_type=="Dep\xf3sito",1,0)
h2$property_type_PH   <- ifelse(h2$property_type=="PH",1,0)
h2$bathbed<-h2$bathrooms*h2$bedrooms
h2$latlon<-h2$lat*h2$lon
h2$inter<-1

X <- h2[!is.na(h2$rooms)& !is.na(h2$bedrooms),c("inter","property_type_Casa",
                                                "property_type_Lote",
"property_type_Otro","property_type_Apto","property_type_LC","rooms",
"property_type_Ofic","property_type_Parq","bathbed","surface_total",
"surface_covered","latlon","hurto","arealic")]

X <- as.matrix(X)

y <- h2$lnprice[!is.na(h2$rooms)& !is.na(h2$bedrooms)]

Xq <- solve(qr(X,LAPACK=TRUE),y)

View(Xq)

modelof <- lm(lnprice~property_type_Casa+property_type_Lote+property_type_Otro+
                    property_type_Apto+property_type_LC+rooms+property_type_Ofic+
property_type_Parq+bathbed+surface_total+surface_covered+latlon+hurto+
arealic,data=h2)

summary(modelof)

p1 <- h2[h2$l2=="Cundinamarca",c("inter","property_type_Casa",
                                                "property_type_Lote",
                                                "property_type_Otro","property_type_Apto","property_type_LC","rooms",
                                                "property_type_Ofic","property_type_Parq","bathbed","surface_total",
                                                "surface_covered","latlon","hurto","arealic")]
p1<- as.matrix(p1)
y <- h2$lnprice[h2$l2=="Cundinamarca"]
Xq <- solve(qr(p1,LAPACK=TRUE),y)
View(Xq)

p2 <- h2[h2$l3=="Barranquilla",c("inter","property_type_Casa",
                                  "property_type_Lote",
                                  "property_type_Otro","property_type_Apto","property_type_LC","rooms",
                                  "property_type_Ofic","property_type_Parq","bathbed","surface_total",
                                  "surface_covered","latlon","hurto","arealic")]
X <- as.matrix(p2)
y <- h2$lnprice[h2$l3=="Barranquilla"]
Xq <- solve(qr(X,LAPACK=TRUE),y)
View(Xq)

p3 <- h2[h2$l2=="Antioquia",c("inter","property_type_Casa",
                                 "property_type_Lote",
                                 "property_type_Otro","property_type_Apto","property_type_LC","rooms",
                                 "property_type_Ofic","property_type_Parq","bathbed","surface_total",
                                 "surface_covered","latlon","hurto","arealic")]
X <- as.matrix(p3)
y <- h2$lnprice[h2$l2=="Antioquia"]
Xq <- solve(qr(X,LAPACK=TRUE),y)
View(Xq)

p4 <- h2[h2$l3=="Cali",c("inter","property_type_Casa",
                                "property_type_Lote",
                                "property_type_Otro","property_type_Apto","property_type_LC","rooms",
                                "property_type_Ofic","property_type_Parq","bathbed","surface_total",
                                "surface_covered","latlon","hurto","arealic")]
X <- as.matrix(p4)
y <- h2$lnprice[h2$l3=="Cali"]
Xq <- solve(qr(X,LAPACK=TRUE),y)
View(Xq)

######Punto 2.2

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



