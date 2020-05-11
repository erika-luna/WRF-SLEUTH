library(ggplot2)
library(dplyr)
library(tidyr)


#pb.date <- as.POSIXct("2011-06-01")

#[1] "2019-03-14"

#format(pb.date, tz="America/Mexico_City",usetz=TRUE)



control_2011 <- read.csv("control_2011.csv")

#Agregar columna con fecha y hora
start <- as.POSIXct("2011-05-31 18:00:00")
#start <- format(pb.date, tz="America/Mexico_City",usetz=TRUE)
interval <- 60
end <- start + as.difftime(28, units="days")
x<-seq(from=start, by=interval*60, to=end)
FechaHora<- as.factor(x[1:672])

format(FechaHora, tz="America/Mexico_City",usetz=TRUE)

control_2011$FechaHora <- format(FechaHora, tz="America/Mexico_City",usetz=TRUE)
#tmp <- as.POSIXct(FechaHora, tz = "America/Mexico_City",)

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
separafecha <- as.character(control_2011$FechaHora)


for (i in 1:672){
  Mes[i] <- destringmes(separafecha[i])
  Dia[i] <- destringdia(separafecha[i])
  Hora[i]<- destringhora(separafecha[i])
  Fecha[i]<- destringfecha(separafecha[i])
} 

control_2011$Mes <- Mes
control_2011$Dia <- Dia
control_2011$Hora <- Hora
control_2011$Fecha <- Fecha

control_2011$Fecha <- as.Date(control_2011$Fecha)

write.csv(control_2011, file = "control_2011_v2.csv")

#Time series
##Convertir en factor a Mes para poder graficarlo
#control_2011$Dia <- factor(control_2011$Dia)

#Seg??n stackoverflow: 
pd=position_dodge(0.9)
ggplot(control_2011, aes(Fecha, Station)) +
  stat_summary(fun.y=mean, geom="line", position=pd) +
  #stat_summary(fun.y=mean, geom="point", position=pd) +
  theme_bw()


myvars <- c("Station", "Station.1", "Station.2", "Station.3",
            "Station.4", "Station.5", "Station.6", "Station.7",
            "Station.8", "Station.9", "Station.10", "Station.11",
            "Station.12", "Station.13","Fecha", "Mes", "Dia", "Hora")

control_2011 <- select(control_2011, myvars)


control_2011 <- gather(control_2011, key = "station", value = "temperature",
       Station, Station.1, Station.2, Station.3,
       Station.4, Station.5, Station.6, Station.7,
       Station.8, Station.9, Station.10, Station.11,
       Station.12, Station.13)

control_2011$temperature <- control_2011$temperature - 273.15 

pd=position_dodge(0.9)
ggplot(control_2011, aes(Fecha, temperature, colour=station, group=station)) +
  stat_summary(fun.y=mean, geom="line", position=pd) +
  #stat_summary(fun.y=mean, geom="point", position=pd) +
  theme_bw()
