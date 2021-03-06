library(ggplot2)
require(data.table)
library(tidyr)
library(dplyr)

setwd("/Users/nikolai/Desktop/model/")
#data<-read.table("Output.csv", header=F, sep=",")

#type <- "white_tra"
#type <- "DOM_tra_xxs"
#type <- "ffer_tra"
type <- "ffer_Xshred"
#type <- "DOM_tra"
#type <- "tra"


input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")



#---------------------- BY SEX -------------------------------

gn <- subset(data, V3 == "Sex")
gnx_keeps <- c("V1", "V2","V4","V6")
gnxtot <- gn[gnx_keeps]
colnames(gnxtot) <- c("Iterations","Generation","Sex","Number")
gnxtott <- data.table(gnxtot)

# pnx <- ggplot(data=gnxtott, aes(x=Generation, y=Number, group=Iterations)) +
#   geom_line() +
#   ylim(0,500) + scale_x_continuous(breaks=seq(0,20,1)) +
#   facet_wrap(~ Sex)
# print(pnx)
# ggsave(filename=paste(type,"_sexes.png",sep=""), plot=pnx, device="png", path=getwd(),dpi=500, height=12, units="cm")

pnxm <- ggplot(data=gnxtott, aes(x=Generation, y=Number)) +
  geom_point(size=0.01) +
  geom_smooth() +
  ylim(1,500) + scale_x_continuous(breaks=seq(0,20,1)) +
  facet_wrap(~ Sex)
print(pnxm)

#---------------------- BY KARYOTYPE -------------------------------

k <- subset(data, V3 == "Sex_Karyotype")
k_keeps <- c("V1", "V2","V4","V6")
ktot <- k[k_keeps]
colnames(ktot) <- c("Iterations","Generation","Karyotype","Number")
ktott <- data.table(ktot)

# kp <- ggplot(data=ktott, aes(x=Generation, y=Number, group=Iterations)) +
#   geom_line() +
#   ylim(0,500) + scale_x_continuous(breaks=seq(0,20,1)) +
#   facet_wrap(~ Karyotype)
# print(kp)
# ggsave(filename=paste(type,"_sexes.png",sep=""), plot=pnx, device="png", path=getwd(),dpi=500, height=12, units="cm")

kpxm <- ggplot(data=ktott, aes(x=Generation, y=Number)) +
  geom_point(size=0.01) +
  geom_smooth() +
  ylim(1,500) + scale_x_continuous(breaks=seq(0,20,1)) +
  facet_wrap(~ Karyotype)
print(kpxm)


#---------------------- ALL -------------------------------

gn_keeps <- c("V1", "V2","V6")
gntot <- gn[gn_keeps]
colnames(gntot) <- c("Iterations","Generation","Number")
gntott <- data.table(gntot)
gntott <- gntott[ , .(Population = sum(Number)), by = .(Generation,Iterations)]

pn <- ggplot(data=gntott, aes(x=Generation, y=Population, group=Iterations)) +
  geom_line() +
  geom_point() + ylim(0,500) + xlim(5,20)
print(pn)

ggsave(filename=paste(type,"_population.png",sep=""), plot=pn, device="png", path=getwd(),dpi=500, height=12, units="cm")


gc()
