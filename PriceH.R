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

#sapply(h2, mode)
#transform(h2, surface_covered = as.numeric(surface_covered))
#h2<- h2 %>% mutate(p_preciom_t=ifelse(is.na(surface_covered),(h2$price*.5)/h2$surface_covered,0))#estima precio por metro cuadrado de total
#h2$p_preciom_<-0
#mutate(h2,is.na(h2$surface_covered),p_preciom_t=h2$precio*.5/h2$surface_covered,0)#estima precio por metro cuadrado de total
#h2$p_preciom_t<-ifelse(is.na(h2$surface_covered),(h2$precio*.5)/h2$surface_covered,is.na(h2$p_preciom_t))

#h2$p_preciom_t<-ifelse(!is.na(h2$surface_covered),(h2$price*0.5)/h2$surface_covered,NA)#estima precio por metro cuadrado de edificación
#h2$p_preciom_t<- h2 %>% group_by(l3,l4,property_type) %>% mutate(p_preciom_t=ifelse(is.na(p_preciom_t),mean(!is.na(p_preciom_t)),p_preciom_t))
#h2<- h2 %>% group_by(l3,l4,property_type) %>% mutate(surface_covered=ifelse(is.na(surface_covered),mean(!is.na(p_preciom_t),surface_covered)))#estima metro cuadrado de construcción

#h2$p_preciom_t1<-ifelse(!is.na(h2$surface_total),(h2$price*0.5)/h2$surface_total,NA)#estima precio por metro cuadrado de total
#h2$p_preciom_t1<- h2 %>% group_by(l3,l4,property_type) %>% mutate(p_preciom_t1=ifelse(is.na(surface_covered),mean(p_preciom_t1),p_preciom_t1))
#h2<- h2 %>% group_by(l3,l4,property_type) %>% mutate(surface_total=ifelse(is.na(surface_total),!is.na(price)/!is.na(p_preciom_t1),surface_total))#estima metro cuadrado de total

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

indic<-sample(1:nrow(h2),floor(.01*nrow(h2)))

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


#modelo 8 nuevas variables
m8<-lm(lnprice~rooms+(bathrooms*bedrooms)+factor(property_type)+lat+lon+(lat*lon)+surface_total+hurto+arealic,data=entrenamiento)
stargazer(m8,type="text")
prueba$yhat_8<-predict(m8,prueba)
prueba<- prueba %>% mutate(err8=(lnprice-yhat_8)^2)
sqrt(mean(prueba$err8))


sqrt(mean(prueba$err))
sqrt(mean(prueba$err2))
sqrt(mean(prueba$err3))
sqrt(mean(prueba$err4))
sqrt(mean(prueba$err5))
sqrt(mean(prueba$err6))
sqrt(mean(prueba$err7))
sqrt(mean(prueba$err8))


#nuevas variables

#LOOCV
error_LOOCV <- c()
for(i in 1:dim(entrenamiento)[1]){
  modelo  <- lm(lnprice~rooms+(bathrooms*bedrooms)+factor(property_type)+lat+lon+(lat*lon)+surface_total+hurto+arealic,data=entrenamiento[-i,])
    error_LOOCV[i] <- entrenamiento$lnprice[i]-predict(modelo,entrenamiento[i,])
  print(i)
}
mean(error_LOOCV*error_LOOCV)
h <- hatvalues(m8)

options(scipen=999)
#medidas <- data.frame(LOOCV=error_LOOCV^2,leverage=as.numeric(h))

medidas <- data.frame(LOOCV=error_LOOCV,leverage=as.numeric(h),error_modelo_co = (entrenamiento$lnprice-predict(m8,entrenamiento)))
medidas$algo <- medidas$error_modelo_co/(1-medidas$leverage)

View(medidas)


# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model <- train(lnprice~rooms+(bathrooms*bedrooms)+factor(property_type)+lat+lon+(lat*lon)+surface_total+hurto+arealic, data = h2, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)






