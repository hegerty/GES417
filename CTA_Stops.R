###########################################################
## GES 317/417 NEIU
## Transportation: El stops and Population Density
###########################################################

## Open data: El stops and surrounding Census Blocks
data<-read.csv("https://raw.githubusercontent.com/hegerty/GES417/refs/heads/main/CTA_Blocks.csv")
dim(data) # Check data
data1<-na.omit(data[,1:6]) # split off El stop columns
data2<-data[,7:10] # Split off Block columns
head(data1) # Check your data
tail(data1)
dim(data1)
head(data2)


## Aggregate block data for every El stop
stoplist<-NULL
for(i in 1:(nrow(data1))){
  j=i-1
  sl1<-data2[data2$NEAR_FID==j,2:3]
  sl2<-c(sum(sl1[,1]),sum(sl1[,2])) # total ALAND and total POP
  sl3<-c(sl2,round(259000*sl2[2]/sl2[1],0)) # convert to density per sq. mile
  stoplist<-rbind(stoplist,sl3)
}
colnames(stoplist)<-c("ALAND","POP","POPDENS")
stoplist<-cbind(data1,stoplist) # combine with El stop data

# Look at most dense areas surrounding stops
colnames(stoplist)
stoplist<-stoplist[order(stoplist$POPDENS,decreasing = T),-c(1,2,7)]
stoplist$RANK<-rank(-stoplist$POPDENS)
head(stoplist,20)


# Look at "outlying" stops--one line only
u1<-sort(unique(stoplist$LINES))  #Make a list of all the branches
u1 #Note the choices
blueonly<-stoplist[stoplist$LINES==u1[1:3] ,]
redonly<-stoplist[stoplist$LINES=="Red Line",]
brownonly<-stoplist[stoplist$LINES==u1[c(4,8)],] #Include the Purple line stops
orangeonly<-stoplist[stoplist$LINES=="Orange Line",]
greenonly<-stoplist[stoplist$LINES==u1[10:12],]
pinkonly<-stoplist[stoplist$LINES=="Pink",]


# Rank by density and take average ranks
ranks<-c(round(mean(blueonly$RANK),1),
round(mean(redonly$RANK),1),
round(mean(brownonly$RANK),1),
round(mean(orangeonly$RANK),1),
round(mean(pinkonly$RANK),1),
round(mean(greenonly$RANK),1))

# Make a table: Which line has the lowest density?
names(ranks)<-c("Blue","Red","Brown","Orange","Pink","Green")
ranks<-sort(ranks)
ranks

