library(ggplot2)
library(dplyr)
library(tidyr)

urban_2060 <- read.csv("urban_2060.csv")

#Agregar columna con fecha y hora

#start <- as.POSIXct("2011-06-01")
start <- as.POSIXct("2011-05-31 18:00:00")
interval <- 60
end <- start + as.difftime(28, units="days")
x<-seq(from=start, by=interval*60, to=end)
FechaHora<- as.factor(x[1:672])

urban_2060$FechaHora <- FechaHora

#SEPARAR MES, DIA, HORA. 
destringmes <- function(x){
  a <- strsplit(x, "-")[[1]]
  a[2]
}

destringdia <- function(x){
  a <- strsplit(x, " ")[[1]]
  b <- strsplit(a, "-")[[1]]
  b[3]
}

destringhora <- function(x){
  a <- strsplit(x, " ")[[1]]
  a[2]
}

destringfecha <- function(x){
  a <- strsplit(x, " ")[[1]]
  a[1]
}

Mes <- rep(0,672)
Dia <- rep(0,672)
Hora <- rep(0,672)
Fecha <- rep(0,672)
separafecha <- as.character(urban_2060$FechaHora)


for (i in 1:672){
  Mes[i] <- destringmes(separafecha[i])
  Dia[i] <- destringdia(separafecha[i])
  Hora[i]<- destringhora(separafecha[i])
  Fecha[i]<- destringfecha(separafecha[i])
} 

urban_2060$Mes <- Mes
urban_2060$Dia <- Dia
urban_2060$Hora <- Hora
urban_2060$Fecha <- Fecha

urban_2060$Fecha <- as.Date(urban_2060$Fecha)

write.csv(urban_2060, file = "urban_2060_v2.csv")

myvars <- c("Station", "Station.1", "Station.2", "Station.3",
            "Station.4", "Station.5", "Station.6", "Station.7",
            "Station.8", "Station.9", "Station.10", "Station.11",
            "Station.12", "Station.13","Fecha", "Mes", "Dia", "Hora")

urban_2060 <- select(urban_2060, myvars)


urban_2060 <- gather(urban_2060, key = "station", value = "temperature",
                       Station, Station.1, Station.2, Station.3,
                       Station.4, Station.5, Station.6, Station.7,
                       Station.8, Station.9, Station.10, Station.11,
                       Station.12, Station.13)

urban_2060$temperature <- urban_2060$temperature - 273.15 

pd=position_dodge(0.9)
ggplot(urban_2060, aes(Fecha, temperature, colour=station, group=station)) +
  stat_summary(fun.y=mean, geom="line", position=pd) +
  #stat_summary(fun.y=mean, geom="point", position=pd) +
  theme_bw()



