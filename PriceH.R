library(readr)
library(dplyr)
library(gt)
library(reshape2)
library(tidyr)
library(MASS)
library(e1071)

setwd("C:/BIGDATA/Taller1/Talle1_1/problem-set-1-luque-beltran-bocanegra")
house1 <- read_csv("house.csv")
View(house1)
attach(house1)
#descriptivas<-house1 %>% select(rooms,bedrooms,bathrooms,surface_total,surface_covered,price)%>% 
#  summarise_all(promedio=~mean(.,na.rm = TRUE),deviacion=~sd(.,na.rm = TRUE),maximo=~max(.,na.rm = TRUE),
#                              minimo=~min(.,na.rm = TRUE),mediana=~median(.,na.rm = TRUE))%>%gt()

options(scipen = 999)
descriptivas<- house1 %>% summarise(rooms,bedrooms,bathrooms,surface_total,surface_covered,price)%>% 
  summarise_each(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE),max(.,na.rm = TRUE),
                      min(.,na.rm = TRUE),median(.,na.rm = TRUE),sum(is.na(.),na.rm = TRUE))) %>%
  t %>% as.data.frame %>% add_rownames %>%
  separate(rowname, into = c("feature", "fun"), sep = "_")%>%gt()

descriptivas

house1$price_cat <- ifelse(house1$price<=253000000,"C1",
                           ifelse(house1$price>253000000 & house1$price<=420000000,"C2",
                                  ifelse(house1$price>420000000 & house1$price<=794000000,"C3",
                                         ifelse(house1$price > 794000000,"C4",NA))))

distinct (house1, operation_type)

#cambio de USD a COP
house1$price<-ifelse (house1$currency=="USD", house1$price*3600,house1$price)

#inputación lotes


#inputación rooms
train <- house1[which(!is.na(house1$rooms)),c("rooms","property_type","price","l3")]
train$rooms <- factor(train$rooms)
train$price <- ifelse(train$price%in%c(0:420000000),"C1",ifelse(train$price>420000000,"C2",NA))
train$price <- factor(train$price)

train2 <- house1[which(is.na(house1$rooms)),c("X1","rooms","property_type","price","l3")]
train2$rooms <- factor(train2$rooms)
train$price <- ifelse(train$price%in%c(0:420000000),"C1",ifelse(train$price>420000000,"C2",NA))
train2$price <- factor(train2$price)                      
                      

nb_1 <- naiveBayes(rooms ~ factor(l3)+price+factor(property_type) , data = train)#inputación teorema de bayes

train2$rooms<-predict(nb_1, train2[,-1], type="class")

house1 <- left_join(x=house1,y=train2[,c("X1","rooms")],by=c("X1"))

house1$rooms <- ifelse(!is.na(house1$rooms.x),house1$rooms.x,house1$rooms.y)

table(house1$rooms, useNA = "ifany")


#inputación bedrooms
trainB <- house1[which(!is.na(house1$bedrooms)),c("bedrooms","property_type","price","l3")]
trainB$bedrooms <- factor(trainB$bedrooms)
trainB$price <- ifelse(trainB$price%in%c(0:420000000),"C1",ifelse(trainB$price>420000000,"C2",NA))
trainB$price <- factor(trainB$price)

trainB2 <- house1[which(is.na(house1$bedrooms)),c("X1","bedrooms","property_type","price","l3")]
trainB2$bedrooms <- factor(trainB2$bedrooms)
trainB2$price <- ifelse(trainB2$price%in%c(0:420000000),"C1",ifelse(trainB2$price>420000000,"C2",NA))
trainB2$price <- factor(trainB2$price)                      

nb_2 <- naiveBayes(bedrooms ~ factor(l3)+price+factor(property_type) , data = trainB)#inputación teorema de bayes
trainB2$bedrooms<-predict(nb_1, trainB2[,-1], type="class")
house1 <- left_join(x=house1,y=trainB2[,c("X1","bedrooms")],by=c("X1"))
house1$bedrooms <- ifelse(!is.na(house1$bedrooms.x),house1$bedrooms.x,house1$bedrooms.y)
table(house1$bedrooms, useNA = "ifany")

#inputación bathrooms
trainBA <- house1[which(!is.na(house1$bathrooms)),c("bathrooms","property_type","price","l3")]
trainBA$ba <- factor(trainBA$bathrooms)
trainBA$price <- ifelse(trainBA$price%in%c(0:420000000),"C1",ifelse(trainBA$price>420000000,"C2",NA))
trainBA$price <- factor(trainBA$price)

trainBA2 <- house1[which(is.na(house1$bathrooms)),c("X1","bathrooms","property_type","price","l3")]
trainBA2$bathrooms <- factor(trainBA2$bathrooms)
trainBA2$price <- ifelse(trainBA2$price%in%c(0:420000000),"C1",ifelse(trainBA2$price>420000000,"C2",NA))
trainBA2$price <- factor(trainBA2$price)                      

nb_3 <- naiveBayes(bathrooms ~ factor(l3)+price+factor(property_type) , data = trainBA)#inputación teorema de bayes
trainBA2$bathrooms<-predict(nb_1, trainBA2[,-1], type="class")
house1 <- left_join(x=house1,y=trainBA2[,c("X1","bathrooms")],by=c("X1"))
house1$bathrooms <- ifelse(!is.na(house1$bathrooms.x),house1$bathrooms.x,house1$bathrooms.y)
table(house1$bathrooms, useNA = "ifany")

#inputación surface_total
trainST <- house1[which(!is.na(house1$surface_total)),c("surface_total","property_type","price","l3")]
trainST$surface_total <- factor(trainST$surface_total)
trainST$price <- ifelse(trainST$price%in%c(0:420000000),"C1",ifelse(trainST$price>420000000,"C2",NA))
trainST$price <- factor(trainST$price)

trainST2 <- house1[which(is.na(house1$surface_total)),c("X1","surface_total","property_type","price","l3")]
trainST2$surface_total <- factor(trainST2$surface_total)
trainST2$price <- ifelse(trainST2$price%in%c(0:420000000),"C1",ifelse(trainST2$price>420000000,"C2",NA))
trainST2$price <- factor(trainST2$price)                      

nb_4 <- naiveBayes(surface_total ~ factor(l3)+price+factor(property_type) , data = trainST)#inputación teorema de bayes
trainST2$surface_total<-predict(nb_1, trainST2[,-1], type="class")
house1 <- left_join(x=house1,y=trainST2[,c("X1","surface_total")],by=c("X1"))
house1$bathrooms <- ifelse(!is.na(house1$bathrooms.x),house1$surface_total.x,house1$surface_total.y)
table(house1$surface_total, useNA = "ifany")

#inputación surface_covered
trainSC <- house1[which(!is.na(house1$surface_covered)),c("surface_covered","property_type","price","l3")]
trainSC$surface_covered <- factor(trainSC$surface_covered)
trainSC$price <- ifelse(trainSC$price%in%c(0:420000000),"C1",ifelse(trainSC$price>420000000,"C2",NA))
trainSC$price <- factor(trainSC$price)

trainSC2 <- house1[which(is.na(house1$surface_covered)),c("X1","surface_covered","property_type","price","l3")]
trainSC2$surface_covered <- factor(trainSC2$surface_covered)
trainSC2$price <- ifelse(trainSC2$price%in%c(0:420000000),"C1",ifelse(trainSC2$price>420000000,"C2",NA))
trainSC2$price <- factor(trainSC2$price)                      

nb_5 <- naiveBayes(surface_covered ~ factor(l3)+price+factor(property_type) , data = trainSC)#inputación teorema de bayes
trainST2$surface_total<-predict(nb_1, trainSC2[,-1], type="class")
house1 <- left_join(x=house1,y=trainST2[,c("X1","surface_covered")],by=c("X1"))
house1$bathrooms <- ifelse(!is.na(house1$surface_covered.x),house1$surface_covered.x,house1$surface_covered.y)
table(house1$surface_covered, useNA = "ifany")


set.seed(10101)


muestra <- sample(house1,dim(house1)[1]*0.3)

entrenamiento <- house1[sample,]


lm(price~1,data=house1)


modelo <- lm(prim....)


residuals(modelo)


modelo1 <- lm price~I(rooms^2)+I(bedrooms*rooms),data=house1)


modelo2 <- lm(log(price)~rooms,data=house1)


for( i in 1:dim(house1)[1]){}


