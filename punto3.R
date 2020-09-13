
load("D:/Desktop/problem-set-1-luque-beltran-bocanegra/Bayes.RData")

library(rvest)
library(tidyr) 
library(readxl)
library(Hmisc)
library(descr)
library(ggplot2)
library(datasets) 
library(tidyverse) 

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=NEW+YORK&year=2016"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex <- html_table(tmp[[1]])
n=dim(ibex)[1]
year = rep(2016,n)
ibex <- cbind(ibex,year)


url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=NEW+YORK&year=2017"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex2 <- html_table(tmp[[1]])
n2=dim(ibex2)[1]
year = rep(2017,n2)
ibex2 <- cbind(ibex2,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=NEW+YORK&year=2018"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex3 <- html_table(tmp[[1]])
n3=dim(ibex3)[1]
year = rep(2018,n3)
ibex3 <- cbind(ibex3,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=NEW+YORK&year=2019"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex4 <- html_table(tmp[[1]])
n4=dim(ibex4)[1]
year = rep(2019,n4)
ibex4 <- cbind(ibex4,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=NEW+YORK&year=2020"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex5 <- html_table(tmp[[1]])
n5=dim(ibex5)[1]
year = rep(2020,n5)
ibex5 <- cbind(ibex5,year)

# Datos San Francisco

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=SAN+FRANCISCO&year=2016"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex6 <- html_table(tmp[[1]])
n6=dim(ibex6)[1]
year = rep(2016,n6)
ibex6 <- cbind(ibex6,year)


url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=SAN+FRANCISCO&year=2017"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex7 <- html_table(tmp[[1]])
n7=dim(ibex7)[1]
year = rep(2017,n7)
ibex7 <- cbind(ibex7,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=SAN+FRANCISCO&year=2018"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex8 <- html_table(tmp[[1]])
n8=dim(ibex8)[1]
year = rep(2018,n8)
ibex8 <- cbind(ibex8,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=SAN+FRANCISCO&year=2019"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex9 <- html_table(tmp[[1]])
n9=dim(ibex9)[1]
year = rep(2019,n9)
ibex9 <- cbind(ibex9,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=SAN+FRANCISCO&year=2020"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex10 <- html_table(tmp[[1]])
n10=dim(ibex10)[1]
year = rep(2020,n10)
ibex10 <- cbind(ibex10,year)


# Datos Los Angeles

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=LOS+ANGELES&year=2016"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex11 <- html_table(tmp[[1]])
n11=dim(ibex11)[1]
year = rep(2016,n11)
ibex11 <- cbind(ibex11,year)


url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=LOS+ANGELES&year=2017"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex12 <- html_table(tmp[[1]])
n12=dim(ibex12)[1]
year = rep(2017,n12)
ibex12 <- cbind(ibex12,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=LOS+ANGELES&year=2018"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex13 <- html_table(tmp[[1]])
n13=dim(ibex13)[1]
year = rep(2018,n13)
ibex13 <- cbind(ibex13,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=LOS+ANGELES&year=2019"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex14 <- html_table(tmp[[1]])
n14=dim(ibex14)[1]
year = rep(2019,n14)
ibex14 <- cbind(ibex14,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=LOS+ANGELES&year=2020"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex15 <- html_table(tmp[[1]])
n15=dim(ibex15)[1]
year = rep(2020,n15)
ibex15 <- cbind(ibex15,year)



# Datos Chicago

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=CHICAGO&year=2016"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex16 <- html_table(tmp[[1]])
n16=dim(ibex16)[1]
year = rep(2016,n16)
ibex16 <- cbind(ibex16,year)


url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=CHICAGO&year=2017"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex17 <- html_table(tmp[[1]])
n17=dim(ibex17)[1]
year = rep(2017,n17)
ibex17 <- cbind(ibex17,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=CHICAGO&year=2018"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex18 <- html_table(tmp[[1]])
n18=dim(ibex18)[1]
year = rep(2018,n18)
ibex18 <- cbind(ibex18,year)


url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=CHICAGO&year=2019"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex19 <- html_table(tmp[[1]])
n19=dim(ibex19)[1]
year = rep(2019,n19)
ibex19 <- cbind(ibex19,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=CHICAGO&year=2020"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex20 <- html_table(tmp[[1]])
n20=dim(ibex20)[1]
year = rep(2020,n20)
ibex20 <- cbind(ibex20,year)


# Datos Houston

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=HOUSTON&year=2016"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex21 <- html_table(tmp[[1]])
n21=dim(ibex21)[1]
year = rep(2016,n21)
ibex21 <- cbind(ibex21,year)



url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=HOUSTON&year=2017"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex22 <- html_table(tmp[[1]])
n22=dim(ibex22)[1]
year = rep(2017,n22)
ibex22 <- cbind(ibex22,year)


url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=HOUSTON&year=2018"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex23 <- html_table(tmp[[1]])
n23=dim(ibex23)[1]
year = rep(2018,n23)
ibex23 <- cbind(ibex23,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=HOUSTON&year=2019"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex24 <- html_table(tmp[[1]])
n24=dim(ibex24)[1]
year = rep(2019,n24)
ibex24 <- cbind(ibex24,year)

url.ibex <-"https://h1bdata.info/index.php?em=&job=&city=HOUSTON&year=2020"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
ibex25 <- html_table(tmp[[1]])
n25=dim(ibex25)[1]
year = rep(2020,n25)
ibex25 <- cbind(ibex25,year)

#Pegar bases de datos

datitos <- rbind(ibex,ibex2,ibex3,ibex4,ibex5,ibex6,ibex7,ibex8,ibex9,ibex10,ibex11,ibex12,ibex13,ibex14,ibex15,ibex16,ibex17,ibex18,ibex19,ibex20,ibex21,ibex22,ibex23,ibex24,ibex25)
datos <- datitos

#Filtro de datos

table(datos$LOCATION)
datos <- subset(datos, datos$LOCATION == "CHICAGO, IL" | datos$LOCATION == "CHICAGO, ILLINOIS" 
                | datos$LOCATION == "NEW YORK, NEW YORK" | datos$LOCATION == "NEW YORK, NY" 
                |datos$LOCATION == "HOUSTON, TEXAS" | datos$LOCATION == "HOUSTON, TX" 
                | datos$LOCATION == "LOS ANGELES, CA" | datos$LOCATION == "LOS ANGELES, CALIFORNIA" 
                |datos$LOCATION == "SAN FRANCISCO, CA" | datos$LOCATION == "SAN FRANCISCO, CALIFORNIA")

table(datos$`CASE STATUS`) 
datos <- subset(datos, datos$`CASE STATUS` == "CERTIFIED" )

table(datos$`JOB TITLE`) 
datos <- subset(datos, datos$`JOB TITLE` != "")
datos <- subset(datos, datos$EMPLOYER != "") 
datos <- subset(datos, datos$`BASE SALARY`!= ""  )

datos$LOCATION <- gsub("CHICAGO, ILLINOIS","CHICAGO, IL",datos$LOCATION)
datos$LOCATION <- gsub("HOUSTON, TEXAS","HOUSTON, TX",datos$LOCATION)
datos$LOCATION <- gsub("LOS ANGELES, CALIFORNIA","LOS ANGELES, CA",datos$LOCATION)
datos$LOCATION <- gsub("NEW YORK, NEW YORK","NEW YORK, NY",datos$LOCATION)
datos$LOCATION <- gsub("SAN FRANCISCO, CALIFORNIA","SAN FRANCISCO, CA",datos$LOCATION)

datos$`BASE SALARY` <- gsub(","," ",datos$`BASE SALARY`)
datos$`BASE SALARY` <- gsub(" ","",datos$`BASE SALARY`)
datos$`BASE SALARY` <- as.numeric(datos$`BASE SALARY`)

datos$`SUBMIT DATE`<-  as.Date(datos$`SUBMIT DATE`,format="%m/%d/%Y")
datos$`START DATE`<-  as.Date(datos$`START DATE`,format="%m/%d/%Y")

table()


crosstab(datos$year, datos$LOCATION,prop.r = TRUE)

p <- ggplot(data = datos,
            mapping = aes(x = factor(datos$year),
                          fill = factor(datos$LOCATION)))
p + geom_bar(position = 'dodge', stat = 'count')


datos2020<- subset(datos, datos$year ==2020 )
datosny<- subset(datos2020, datos2020$LOCATION =="NEW YORK, NY" )

datos <- na.omit(datitos)

summary(datos)
describe(datos)

aggregate.data.frame()

count
punto6 <- datos  %>%count(datos$year,datos$LOCATION, datos$EMPLOYER, sort=TRUE) 



filter(punto6, punto6$`datos$LOCATION`=="CHICAGO, IL" & punto6$`datos$year`=="2016") %>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="CHICAGO, IL" & punto6$`datos$year`=="2017")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="CHICAGO, IL" & punto6$`datos$year`=="2018")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="CHICAGO, IL" & punto6$`datos$year`=="2019")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="CHICAGO, IL" & punto6$`datos$year`=="2020")%>% head(3)

filter(punto6, punto6$`datos$LOCATION`=="HOUSTON, TX" & punto6$`datos$year`=="2016") %>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="HOUSTON, TX" & punto6$`datos$year`=="2017")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="HOUSTON, TX" & punto6$`datos$year`=="2018")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="HOUSTON, TX" & punto6$`datos$year`=="2019")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="HOUSTON, TX" & punto6$`datos$year`=="2020")%>% head(3)

filter(punto6, punto6$`datos$LOCATION`=="LOS ANGELES, CA" & punto6$`datos$year`=="2016")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="LOS ANGELES, CA" & punto6$`datos$year`=="2017")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="LOS ANGELES, CA" & punto6$`datos$year`=="2018")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="LOS ANGELES, CA" & punto6$`datos$year`=="2019")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="LOS ANGELES, CA" & punto6$`datos$year`=="2020")%>% head(3)

filter(punto6, punto6$`datos$LOCATION`=="NEW YORK, NY" & punto6$`datos$year`=="2016")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="NEW YORK, NY" & punto6$`datos$year`=="2017")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="NEW YORK, NY" & punto6$`datos$year`=="2018")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="NEW YORK, NY" & punto6$`datos$year`=="2019")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="NEW YORK, NY" & punto6$`datos$year`=="2020")%>% head(3)

filter(punto6, punto6$`datos$LOCATION`=="SAN FRANCISCO, CA" & punto6$`datos$year`=="2016")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="SAN FRANCISCO, CA" & punto6$`datos$year`=="2017")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="SAN FRANCISCO, CA" & punto6$`datos$year`=="2018")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="SAN FRANCISCO, CA" & punto6$`datos$year`=="2019")%>% head(3)
filter(punto6, punto6$`datos$LOCATION`=="SAN FRANCISCO, CA" & punto6$`datos$year`=="2020")%>% head(3)



count()