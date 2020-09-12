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


#delay
Sys.sleep(40)
web_page<-read_html("https://h1bdata.info/index.php?em=&job=&city=NEW+YORK&year=2016")

#With xpath
table1_2016NYC<- web_page %>% 
  html_node(xpath="//*[@id="myTable"]") %>% 
  html_table()


#css selector
table1<- web_page %>% 
  html_node('table.wikitable:nth-child(15) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1)') %>% 
  html_table()



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


Base_trabajo_collapsed<-Base_trabajo %>% group_by(EMPLOYER) %>% summarise(
  mean_wage = mean(`BASE SALARY`), sd_wage = sd(`BASE SALARY`),aplicaciones = sum(aplicaciones))

Base_trabajo_collapsed$sd_wage[is.na(Base_trabajo_collapsed$sd_wage)] <- 0


write_xlsx(Base_trabajo_collapsed,"C:/Users/VSD0301/OneDrive - Universidad de los Andes/Big Data ML Applied Economics/Taller1/punto2/Base_trabajo_collapsed.xlsx")




git add -A
git commit -m "Escribir comentario"
git push

