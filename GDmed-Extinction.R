library(ggplot2)
require(data.table)
library(tidyr)
library(dplyr)

setwd("/Users/nikolai/Desktop/model/")

process_data <- function(dataset) {
  gn <- subset(dataset, V3 == "Sex")
  gn_keeps <- c("V1", "V2","V6")
  gntot <- gn[gn_keeps]
  colnames(gntot) <- c("Iterations","Generation","Number")
  gntott <- data.table(gntot)
  gntott <- gntott[ , .(Population = sum(Number)), by = .(Generation,Iterations)]
  gntott[Population > 1]$Population <- 1
  gntott$Iterations <- NULL
  gntotta<- aggregate(Population ~ Generation, gntott, sum)
  return(gntotta)
}

type <- "white_tra"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d1 <- process_data(data)
d1$Type <- "white_tra"

type <- "ffer_tra"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d2 <- process_data(data)
d2$Type <- "ffer_tra"

type <- "ffer"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d3 <- process_data(data)
d3$Type <- "ffer"

type <- "tra"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d4 <- process_data(data)
d4$Type <- "tra"

type <- "DOM_tra"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d5 <- process_data(data)
d5$Type <- "tra(dom)"

type <- "ffer_Xshred"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d6 <- process_data(data)
d6$Type <- "ffer_Xshred"

#a <- d1
a <- rbind(d1, d2, d3, d4, d5, d6)

pEX <- ggplot(data=a, aes(x=Generation,y=Population, group=Type)) +
  geom_line(aes(color=Type), size=3) +
  xlim(5,20) +
  ylim(0,100) +
  scale_color_viridis(discrete = TRUE,option = "D") +
  theme(panel.border=element_blank()) +
  ylab("Surviving Populations (%)") 
print(pEX)





pEX2 <- pEX +
  theme(title=element_text(size=10, hjust=0.5),
        axis.title=element_text(size=8),
        axis.text = element_text(size=8),strip.text.x = element_text(size = 8), legend.text=element_text(size=6))

ggsave(filename="Extiction.png", plot=pEX2, device="png", path=getwd(),dpi=500, height=12, units="cm")

gc()
