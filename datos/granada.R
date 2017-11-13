library(dplyr)
library(plyr)
library(plotly)
library("ggplot2")
x <- fromJSON("granada.json")
granada <- ldply(x, data.frame)
granada$CONAN<-as.numeric(granada$CO)
granada$O3NAN<-as.numeric(granada$O3)
granada$SO2NAN<-as.numeric(granada$SO2)
granada$NO2NAN<-as.numeric(granada$NO2)
granada$PARTNAN<-as.numeric(granada$PART)
granada_congresos<- granada[granada$estacion=="PALACIO DE CONGRESOS",]
library(data.table)
DT <- data.table(granada_congresos)
granada_congresos_error<-as.data.frame(DT[, 100*sum(is.na(CONAN))/144, by = list(date, estacion) ])
granada_congresos_error$date<-as.Date(granada_congresos_error$date)
granada_congresos_error$month<- as.factor(as.numeric(format(granada_congresos_error$date, "%m")))
granada_congresos_error$year <- as.factor(as.numeric(format(granada_congresos_error$date, "%Y")))
granada_congresos_error$day<- as.factor(as.numeric(format(granada_congresos_error$date, "%d")))
granada_congresos_error$errores<-as.factor(granada_congresos_error$V1)
granada_congresos_error$number_day<-yday(granada_congresos_error$date)

library(reshape)
library(ggplot2)
library(circular)
# Using ggplot2 0.9.2.1

y_labels = levels(granada_congresos_error$errores)
y_breaks = seq_along(y_labels) + 15

p2 = ggplot(granada_congresos_error, aes(x=number_day, y=year, fill=V1)) +
  geom_tile(colour="white") +
  scale_fill_gradient(low = "steelblue", high = "red") +
  ylim(c(0, max(granada_congresos_error$year) + 0.5)) +
  scale_y_discrete(breaks=y_breaks, labels=y_labels) +
  coord_polar(theta="x") +
  theme(panel.background=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_text(size=5))