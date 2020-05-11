#Scripts control_2011 and urban_2060 should be run beforehand. 

library(dplyr)
library(plyr)
library(ggplot2)
library(gridExtra)

control_2011$datos <- "2011"
urban_2060$datos <- "2060"
control_urban <- bind_rows(control_2011, urban_2060)
control_urban$Dia <- as.numeric(control_urban$Dia)

Station.1 <- filter(control_urban, station == 'Station.1')

#Gr??ficas
pd=position_dodge(0.5)
ggplot(Station.1, aes(Fecha, temperature, colour=datos, group=datos)) +
  stat_summary(fun.y=mean, geom="line", position=pd) +
  stat_summary(fun.y=mean, geom="point", position=pd) +
  labs(x="Fecha", y="Temperatura [??C]") +
  theme_bw()

ggplot(Station.1, aes(Hora, temperature, colour=datos, group=datos)) +
  stat_summary(fun.y=mean, geom="line", position=pd) +
  stat_summary(fun.y=mean, geom="point", position=pd) +
  labs(x="Hora", y="Temperatura [??C]") +
  theme_bw()


#stations_list <- unique(control_urban$station)

p <- ggplot(control_urban, aes(Hora, temperature, colour=datos, group=datos)) +
  stat_summary(fun.y=mean, geom="line", position=pd) +
  stat_summary(fun.y=mean, geom="point", position=pd) +
  labs(x="Hora", y="Temperatura [??C]") +
  theme_bw()

p + facet_wrap(~station)

tiff("test.tiff", units="in", width=16, height=10, res=600)
p <- ggplot(control_urban, aes(Hora, temperature, colour=datos, group=datos)) +
  stat_summary(fun.y=mean, geom="line", position=pd) +
  stat_summary(fun.y=mean, geom="point", position=pd) +
  labs(x="Hora", y="Temperatura [??C]") +
  theme_bw()

p + facet_wrap(~station)
dev.off()


#### otros 
p <- ggplot(tmp, aes(x = fecha, y = temperature)) + 
  geom_line(aes(color = simulacion), size = 1) +
  labs(title='temp', x="Hora", y="Temperatura [C]")+
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()+
  theme(legend.position='right') 
  #coord_cartesian(xlim=c(0, 23)) + 
  #scale_x_continuous(breaks=seq(0, 23, 1))

