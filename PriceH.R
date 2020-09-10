library(readr)
library(dplyr)
library(gt)
library(reshape2)
library(tidyr)
library(MASS)
library(e1071)

setwd("C:/BIGDATA/Taller1/Talle1_1/problem-set-1-luque-beltran-bocanegra")
house1 <- read_csv("house.csv")
attach(house_prices_ps1)
#descriptivas<-house1 %>% select(rooms,bedrooms,bathrooms,surface_total,surface_covered,price)%>% 
#  summarise_all(promedio=~mean(.,na.rm = TRUE),deviacion=~sd(.,na.rm = TRUE),maximo=~max(.,na.rm = TRUE),
#                              minimo=~min(.,na.rm = TRUE),mediana=~median(.,na.rm = TRUE))%>%gt()

options(scipen = 999)
descriptivas<-house1 %>% select(rooms,bedrooms,bathrooms,surface_total,surface_covered,price)%>% 
summarise_each(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE),max(.,na.rm = TRUE),
                    min(.,na.rm = TRUE),median(.,na.rm = TRUE),sum(is.na(.),na.rm = TRUE))) %>%
  t %>% 
  as.data.frame %>% 
  add_rownames %>%
  separate(rowname, into = c("feature", "fun"), sep = "_")%>%gt()

descriptivas

#inputación
train <- house1[which(!is.na(house1$rooms)),c("rooms","property_type","price","l3")]
train$rooms <- factor(train$rooms)
train$price <- ifelse(train$price%in%c(0:10000000),"C1",ifelse(train$price>10000000,"C2",NA))
train$price <- factor(train$price)

train2 <- house1[which(is.na(house1$rooms)),c("X1","rooms","property_type","price","l3")]
train2$rooms <- factor(train2$rooms)
train2$price <- ifelse(train2$price%in%c(0:10000000),"C1",ifelse(train2$price>10000000,"C2",NA))
train2$price <- factor(train2$price)                      
                      

nb_1 <- naiveBayes(rooms ~ factor(l3)+price+factor(property_type) , data = train)

train2$rooms<-predict(nb_1, train2[,-1], type="class")

house1 <- left_join(x=house1,y=train2[,c("X1","rooms")],by=c("X1"))

house1$rooms <- ifelse(!is.na(house1$rooms.x),house1$rooms.x,house1$rooms.y)

table(house1$rooms, useNA = "ifany")
sssssss





set.seed(10101)


muestra <- sample(house1,dim(house1)[1]*0.3)

entrenamiento <- house1[sample,]


lm(price~1,data=house1)


modelo <- lm(prim....)


residuals(modelo)


modelo1 <- lm price~I(rooms^2)+I(bedrooms*rooms),data=house1)


modelo2 <- lm(log(price)~rooms,data=house1)


for( i in 1:dim(house1)[1]){}


