library(ggplot2)
require(data.table) 
#install.packages("viridis")  # Install
library("viridis")
library(tidyr)
library(dplyr)

setwd("/Users/nikolai/Desktop/model/")

# --- data input
#data<-read.table("Output.csv", header=F, sep=",")

#type <- "white_tra"
#type <- "ffer_tra"
type <- "DOM_tra"
#type <- "tra"

input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")

g <- subset(data, V7 == "all")
g <- subset(g, V1 <= 3)
g <- subset(g, V3 != "Sex")
g$V7 <- NULL
colnames(g) <- c("Iterations","Generation","Trait","Allele1","Allele2","Number")
g$R1[g$Allele1 == "R1"] <- "R1 carrier"
g$R1[g$Allele1 != "R1"] <- "Other"
g$R1[g$Allele2 == "R1"] <- "R1 carrier"
g$Allele1 <- NULL
g$Allele2 <- NULL
g <- droplevels(g)
g <- complete(g,Iterations,Generation, Trait, R1)
g[is.na(g)] <- 0
g <- aggregate(Number ~ ., data=g, FUN=sum)
#g <- setDT(gR)[, Fraction := Number / sum(Number), by=list(Generation, Iterations, Trait)]


# pA <- ggplot(data=g, aes(x=Generation,y=Number,color=R1)) +
#   stat_summary(aes(group=R1), fun = "mean" ,geom = 'line', size=1, alpha=1) +
#   facet_grid(cols = vars(g$Trait)) + scale_color_viridis(discrete = TRUE,option = "D")+
#   theme_minimal() + theme(panel.spacing = unit(0.75, "lines"), panel.border = element_rect(color = "black", fill = NA, size = 1)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   scale_x_continuous(breaks=seq(0,20,1), expand = c(0, 0)) +
#   ylab("Number of Individuals (mean)")
# print(pA)

# pFA <- ggplot(data=g, aes(x=Generation,y=Fraction,color=R1)) +
#   stat_summary(aes(group=R1), fun = "mean" ,geom = 'line', size=1, alpha=1) +
#   scale_color_viridis(discrete = TRUE,option = "D") +
#   theme_minimal() + theme(panel.spacing = unit(0.75, "lines"), panel.border = element_rect(color = "black", fill = NA, size = 1)) + 
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(xlim = c(0, 20)) +
#   ylab("R1 Individuals")
# print(pFA)


pF <- ggplot(data=g, aes(x=Generation,y=Number,color=R1)) +
  geom_line() + 
  geom_point(aes(shape=R1)) +
  scale_shape_manual(values=1:10) +
  facet_grid(Trait ~ Iterations) +
  scale_color_viridis(discrete = TRUE,option = "D") +
  theme_minimal() + theme(panel.spacing = unit(0.75, "lines"), panel.border = element_rect(color = "black", fill = NA, size = 1)) + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 20)) +
  ylab("Number of Individuals")
print(pF)






pFp <- pF +
  theme(title=element_text(size=14, hjust=0.5),
        axis.title=element_text(size=14),
        axis.text = element_text(size=10),strip.text.x = element_text(size = 10), legend.text=element_text(size=10))

ggsave(filename=paste(type,"_fractions.png",sep=""), plot=pFp, device="png", path=getwd(),dpi=500, height=12, units="cm")


pAo <- pA +
  theme(title=element_text(size=18, hjust=0.5),
        axis.title=element_text(size=16),
        axis.text = element_text(size=12),strip.text.x = element_text(size = 12), legend.text=element_text(size=10))

ggsave(filename=paste(type,"_genotypes.png",sep=""), plot=pAo, device="png", path=getwd(),dpi=500, height=12, units="cm")

gc()


gc()