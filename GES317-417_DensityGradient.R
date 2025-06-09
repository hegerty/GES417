#############################################
## GES 317/417 NEIU Summer 2025
## Calculate Chicago's density gradient!
#############################################

## Pull data (generated in Arc; distance from CBD (State and Mason) to tract centroids, in feet)
data<-read.csv("https://raw.githubusercontent.com/hegerty/GES417/refs/heads/main/CHItractsDist.csv")
head(data)
data[is.na(data$CHITRACT),5]<-0 # coded 1 for Chicago; NAs to zeros
data<-data[,c(1,2,3,5)] #subset accordingly
dim(data)
sort(data$POPDENS,decreasing = T)[1:10] # clean and sort data

data<-data[-which.max(data[,3]),] # drop one weird outlier

m1<-mean(data$POPDENS) # just get the basic summary stats
sd1<-sd(data$POPDENS)
print(c(m1,sd1))

#### PLOT with miles from CBD as x-axis, Density on y-axis 
plot(data$NEAR_DIST/5280,data$POPDENS,pch=20,ylab="Pop. Density",xlab=c("Distance (Miles)"),ylim=c(0,15500))

#Perform linear regression
y<-data$POPDENS
x<-data$NEAR_DIST/5280
reg1<-lm(y~x)
summary(reg1) # Are the coefficients significant?


abline(reg1,lwd=3,col="dark grey") # add a linear density gradient

# Try a nonlinear method: squared distance (for a U-shape)
reg2<-lm(y~x+I(x^2))
summary(reg2) # is the new term significant?
yfit2<-reg2$coefficients[1]+x*reg2$coefficients[2]+(x*x)*reg2$coefficients[3]

# Add to plot
par(new=TRUE)
plot(x,yfit2,ylim=c(0,15500),col="grey",pch=20,ylab="",xlab="")
# Does this seem to fit better?


### Chicago only
data2<-data[data$CHITRACT==1,]
m2<-mean(data2$POPDENS)
sd2<-sd(data2$POPDENS)
plot(data2$NEAR_DIST/5280,data2$POPDENS,pch=20,ylab="Pop. Density",xlab=c("Distance (Miles)"))
x2<-data2$NEAR_DIST/5280
y2<-data2$POPDENS
reg3<-lm(y2~x2)
summary(reg3)
summary(reg1) #compare linear gradient (Cook Co.)
abline(reg3,lwd=3,col="dark grey")

# Chicago's Density gradient is linear
reg4<-lm(y2~x2+I(x2*x2))
summary(reg4)
