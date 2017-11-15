library(dplyr)
library(plyr)
library(plotly)
library(data.table)
library(jsonlite)
library(reshape)
library(ggplot2)
library(circular)

x <- fromJSON("granada.json")
granada <- ldply(x, data.frame)
granada$CONAN<-as.numeric(granada$CO)
granada$O3NAN<-as.numeric(granada$O3)
granada$SO2NAN<-as.numeric(granada$SO2)
granada$NO2NAN<-as.numeric(granada$NO2)
granada$PARTNAN<-as.numeric(granada$PART)

for ( i in c("PALACIO DE CONGRESOS","GRANADA-NORTE") ) {
    granada_congresos<- granada[granada$estacion==i,]
    DT <- data.table(granada_congresos)
    granada_congresos_error<-as.data.frame(DT[, 100*sum(is.na(CONAN))/144, by = list(date, estacion) ])
    granada_congresos_error$date<-as.Date(granada_congresos_error$date)
    granada_congresos_error$month<- as.factor(as.numeric(format(granada_congresos_error$date, "%m")))
    granada_congresos_error$year <-as.numeric(format(granada_congresos_error$date, "%Y"))
    granada_congresos_error$day<- as.factor(as.numeric(format(granada_congresos_error$date, "%d")))
    granada_congresos_error$errores<-as.factor(granada_congresos_error$V1)
    granada_congresos_error$number_day<-yday(granada_congresos_error$date)
    
                                        # Using ggplot2 0.9.2.1
    granada_congresos_error$var2 = as.numeric(granada_congresos_error$year)
    y_labels = levels(granada_congresos_error$year)
    y_breaks = seq_along(y_labels) + 15
    
    p2 = ggplot(granada_congresos_error, aes(x=number_day, y=var2, fill=V1)) +
        geom_tile(colour="white") +
        scale_fill_gradient(low = "steelblue", high = "red") +
        ylim(c(0, max(granada_congresos_error$var2) + 0.5)) +
        scale_y_discrete(breaks=y_breaks, labels=y_labels) +
        coord_polar(theta="x") +
        theme(panel.background=element_blank(),
              axis.title=element_blank(),
              panel.grid=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks=element_blank(),
              axis.text.y=element_text(size=5))
    p2
    name <- paste0(i,"-circle")
    ggsave(paste0(name,".png"),width=16,height=9)
}
