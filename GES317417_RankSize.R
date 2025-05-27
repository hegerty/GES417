#############################################################
###  GES 317/417
###  Summer 2025
###  NEIU
###  Test the Rank-Size Rule using Census Data
#################################################################

# 1. Pull data (provided)
data=read.csv("https://raw.githubusercontent.com/hegerty/GES417/refs/heads/main/GES317-417_RankSize.csv")
head(data) # Check data
data<-data[order(data[,3],decreasing = TRUE),] #Sort by size
data$rank<-rank(-data$POP) #note negative sign
data$lnrank<-log(data$rank) # create variables
data$lnsize<-log(data$POP)

# 2. Choose an appropriate subset
size=100 # here, largest 100 cities
data2<-data[data$rank<=size,]
dim(data2)

# 3. Run regression!
ranksize<-lm(lnrank~lnsize,data=data2)
summary(ranksize)

# 4. Plot
plot(data2$lnsize,data2$lnrank,pch=20, ylab="Log Rank",xlab="Log Size")
abline(ranksize,lwd=1.5,col="dark gray")
