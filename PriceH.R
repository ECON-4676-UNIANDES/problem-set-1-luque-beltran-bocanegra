library(readr)
library(dplyr)
library(gt)
library(reshape2)
library(tidyr)
library(MASS)
library(e1071)
library(VIM)
library(stargazer)

setwd("C:/BIGDATA/Taller1/Talle1_1/problem-set-1-luque-beltran-bocanegra")
house1 <- read_csv("house.csv")
View(house1)
attach(house1)
#descriptivas<-house1 %>% select(rooms,bedrooms,bathrooms,surface_total,surface_covered,price)%>% 
#  summarise_all(promedio=~mean(.,na.rm = TRUE),deviacion=~sd(.,na.rm = TRUE),maximo=~max(.,na.rm = TRUE),
#                              minimo=~min(.,na.rm = TRUE),mediana=~median(.,na.rm = TRUE))%>%gt()

options(scipen = 999)
descriptivas1<- house1 %>% summarise(rooms,bedrooms,bathrooms,surface_total,surface_covered,price)%>% 
  summarise_each(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE),max(.,na.rm = TRUE),
                      min(.,na.rm = TRUE),median(.,na.rm = TRUE),sum(is.na(.),na.rm = TRUE))) %>%
  t %>% as.data.frame %>% add_rownames %>%
  separate(rowname, into = c("feature", "fun"), sep = "_")%>%gt()

descriptivas1

#house1$price_cat <- ifelse(house1$price<=253000000,"C1",
#                           ifelse(house1$price>253000000 & house1$price<=420000000,"C2",
#                                  ifelse(house1$price>420000000 & house1$price<=794000000,"C3",
#                                         ifelse(house1$price > 794000000,"C4",NA))))

distinct (house1, property_type)

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



#eliminación de valores 0 en precio total 133/239343

house1<-filter (house1, house1$price!=0)

descriptivas1<- house1 %>% summarise(rooms,bedrooms,bathrooms,surface_total,surface_covered,price)%>% 
  summarise_each(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE),max(.,na.rm = TRUE),
                      min(.,na.rm = TRUE),median(.,na.rm = TRUE),sum(is.na(.),na.rm = TRUE))) %>%
  t %>% as.data.frame %>% add_rownames %>%
  separate(rowname, into = c("feature", "fun"), sep = "_")%>%gt()

descriptivas1

#inputación rooms
h2 <- hotdeck(house1,variable=c("rooms","bedrooms","bathrooms","surface_total","surface_covered"),
                  domain_var=c("l3","price","property_type"),impNA = TRUE)
View(h2)
sum(is.na(h2$rooms))
sum(is.na(h2$bathrooms))
sum(is.na(h2$bedrooms))
sum(is.na(h2$surface_covered))
sum(is.na(h2$surface_total))


#punto 2.3

set.seed(10101)

indic<-sample(1:nrow(h2),floor(.7*nrow(h2)))

entrenamiento <- h2[indic,]
prueba <- h2[-indic,]

#regresión sobre entrenamiento
m1<-lm(price~1,data=entrenamiento)
stargazer(m1,type="text")

#predicción sobre prueba
#yhat<-predict(m2,prueba)
prueba$yhat<-predict(m1,prueba)
View(prueba)

#Calculation of the prediction error
prueba<- prueba %>% mutate(err=(price-yhat)^2)
sqrt(mean(prueba$err))

#modelos 2
m2<-lm(price~rooms,data=entrenamiento)
stargazer(m2,type="text")
prueba$yhat_2<-predict(m2,prueba)
prueba<- prueba %>% mutate(err2=(price-yhat_2)^2)
sqrt(mean(prueba$err2))

#model 3
m3<-lm(price~bathrooms+rooms+factor(property_type),data=entrenamiento)
stargazer(m3,type="text")
prueba$yhat_3<-predict(m3,prueba)
tespruebat<- prueba %>% mutate(err3=(lnprice-yhat_3)^2)
sqrt(mean(prueba$err3))

#model 4
m4<-lm(price~bathrooms+rooms+bedrooms+factor(property_type),data=entrenamiento)
stargazer(m4,type="text")
prueba$yhat_4<-predict(m4,prueba)
prueba<- prueba %>% mutate(err4(lnprice-yhat_4)^2)
sqrt(mean(prueba$err4))

#model 5
m5<-lm(price~bathrooms+rooms+bedrooms+factor(property_type)+factor(l3),data=entrenamiento)
stargazer(m5,type="text")
prueba$yhat_5<-predict(m5,prueba)
prueba<- prueba %>% mutate(err5(lnprice-yhat_5)^2)
sqrt(mean(prueba$err5))

#model 6
entrenamiento$lnprice<-log(price)
m6<-lm(lnprice~bathrooms+rooms+bedrooms+factor(property_type)+factor(l3),data=entrenamiento)
stargazer(m6,type="text")
prueba$yhat_6<-predict(m6,prueba)
prueba<- prueba %>% mutate(err6(lnprice-yhat_6)^2)
sqrt(mean(prueba$err6))

#model 7
entrenamiento$lnprice<-log(price)
m7<-lm(lnprice~bathrooms+rooms+bedrooms+factor(property_type)+factor(l3)+surface_total+surface_covered,data=entrenamiento)
stargazer(m7,type="text")
prueba$yhat_7<-predict(m7,prueba)
prueba<- prueba %>% mutate(err7(lnprice-yhat_7)^2)
sqrt(mean(prueba$err7))


sqrt(mean(prueba$err))
sqrt(mean(prueba$err2))
sqrt(mean(prueba$err3))
sqrt(mean(prueba$err4))
sqrt(mean(prueba$err5))
sqrt(mean(prueba$err6))
sqrt(mean(prueba$err7))



error_LOOCV <- c()
for(i in 1:dim(house2)[1]){
  modelo  <- lm(price~rooms+bathrooms+factor(property_type)+surface_total+surface_covered,data=house2[-i,])
  error_LOOCV[i] <- mean(residuals(modelo))  
}
mean(error_LOOCV)
h <- hatvalues(modelo)




