## REGRESSION
# Ratingij = β0i+ β1iX1ij + β2iX2ij + … + βMiXMij + eij
# We run regression to get β estimates called part-utility

# -individual(each person separately)
# -segment (a priori segments using interations or cell means)
# -Aggregate (using all the individuals for one set of coefficient)



# - aggregate
summary(lm(ratings~price+size+motion+style,data = conjointData))


# - segment
data <- merge(conjointData,respondentData,all = TRUE)
summary(lm(ratings~(price+size+motion+style)*age, data = data))
summary(lm(ratings~(price+size+motion+style)*gender, data = data))
# Because gender is not significant, so we just use age segment
summary(lm(ratings~price+size+motion+style,subset=age==1,data = data)) # 3-4 years old
summary(lm(ratings~price+size+motion+style,subset=age==0, data = data))
summary(lm(ratings~price+size+motion+style,subset=gender==0,data = data))


# - Aggregate by Individual
b = cbind(rep(1,nrow(conjointData)),conjointData[,c(4:7)])
partworths = matrix(nrow=nrow(respondentData),ncol=ncol(b))
for(i in 1:200){ #for each individual run the regression
  partworths[i,]=lm(ratings~price+size+motion+style,subset=ID==i,data = conjointData)$coef
}
colnames(partworths) = c("Intercept","price","size","motion","style")
# Coefficient for each individual 


#===============================================================================
## CLUSTER ANALYSIS 
# - Segment Individual
toClust = partworths
source("ClusterCode.R")
tmp <-  clustTest(toClust)
clusts <- runClusts(toClust, 2:3)
plotClust(clusts$kms[[2]],toClust)


#===============================================================================
##PREDICT MISSING CELLS RATINGS
##predict missing cells (preparing for market simulation)
##repeat individual level partworths for multiplication
partworths.full = matrix(rep(partworths,each=16),ncol=5)
pratings = rowSums(b*partworths.full)
finalratings = ifelse(is.na(conjointData$ratings),pratings,conjointData$ratings)
finaldata = cbind(data,finalratings)[,c("ID","profile","finalratings")]
library(reshape)
finaldata <- cast(finaldata, ID ~ profile)
colnames(finaldata) <- c("ID","P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15","P16")

write.csv(finaldata,"finaldata.csv")
#===============================================================================
##Generate profits given market simulation
scens = list()

scens[[1]]=c(6,14,8)    # current scenario (p5,p13; competitor:p7)
scens[[2]]=c(6,14,9)    # competitor reduces price (p5,p13; competitor:p8)

# If competitor reduces price
scens[[3]]=c(5,9)       # Only launch segment(age == 1) preference (p4,competitor:p8)
scens[[4]]=c(7,9)       # Only launch segment(age == 0) preference (p6,competitor:p8)
scens[[5]]=c(5,7,9)     # Launch two product (p4,p6;competitor:p8)

# p6,p12,p8
scens[[6]]=c(7,13,9)
# p6,p7,p8
scens[[7]]=c(7,8,9)
# p12,p7,p8
scens[[8]]=c(13,8,9)
# p6,p11,p8
scens[[9]]=c(7,12,9)
# p11,p7,p8
scens[[10]]=c(12,8,9)

## Since local retailers only sell three models so we do not consider more scenarios


## Calculate the market share, if there is more than 2 same highest ratings then separate the market share 
library(matrixStats)
simFCShares = function(scens,data){
  inmkt = finaldata[,scens]
  inmkt$rowMax <- rowMaxs(as.matrix(inmkt))
  decs <- as.data.frame(ifelse(inmkt==rowMaxs(as.matrix(inmkt)),1,0))
  decs$rowMax <- NULL
  decs$rowSum <- rowSums(decs)
  decs <- as.matrix(decs)
  for (i in 1:nrow(decs)){
    if (decs[i,ncol(decs)] == 1){
      decs[i,] <- decs[i,]
    }else {
      decs[i,(1:ncol(decs)-1)][which(decs[i,(1:ncol(decs)-1)]==1)] <- 1/decs[i,ncol(decs)]
    }
  }
  decs <- as.data.frame(decs)
  decs <- decs[,1:length(decs)-1]
  shs = colSums(decs)/sum(decs)
  shs
}



#simFCShares = function(scens,data){
#  inmkt = finaldata[,scens]
#  inmkt$rowMax <- rowMaxs(as.matrix(inmkt))
#  decs <- as.data.frame(ifelse(inmkt==inmkt$rowMax,1,0))
#  decs$rowMax <- NULL
#  shs = colSums(decs)/sum(decs)
#  shs
#}



simFCShares(scens[[1]],finaldata)
simFCShares(scens[[2]],finaldata)
simFCShares(scens[[3]],finaldata)
simFCShares(scens[[4]],finaldata)
simFCShares(scens[[5]],finaldata)
simFCShares(scens[[6]],finaldata)
simFCShares(scens[[7]],finaldata)
simFCShares(scens[[8]],finaldata)
simFCShares(scens[[9]],finaldata)
simFCShares(scens[[10]],finaldata)



profilesData$variable <- ifelse(profilesData$sizeLabel=="18 inches"&profilesData$motionLabel=="Rocking",33,0)
profilesData$variable <- ifelse(profilesData$sizeLabel=="26 inches"&profilesData$motionLabel=="Rocking",41,profilesData$variable)
profilesData$variable <- ifelse(profilesData$sizeLabel=="18 inches"&profilesData$motionLabel=="Bouncing",38,profilesData$variable)
profilesData$variable <- ifelse(profilesData$sizeLabel=="26 inches"&profilesData$motionLabel=="Bouncing",46,profilesData$variable)


simProfit = function(scens,data,prices,vcosts,year,fcosts=20000,newProductCost=7000,mktsize=4000){
  mktshr = simFCShares(scens,data)
  profit <- ifelse(year == 1,
                   mktshr*mktsize*(prices-vcosts)-fcosts-newProductCost,
                   mktshr*mktsize*(prices-vcosts)-fcosts)
  profit
}

simProfit(scens[[1]],finaldata,c(139.99,139.99,139.99),c(33,33,41),c(0,0,0))
simProfit(scens[[2]],finaldata,c(139.99,139.99,119.99),c(33,33,41),c(0,0,0),20000)
simProfit(scens[[3]],finaldata,c(119.99,119.99),c(46,41),c(1,0),20000)
simProfit(scens[[4]],finaldata,c(119.99,119.99),c(33,41),c(0,0),20000)
simProfit(scens[[5]],finaldata,c(119.99,119.99,119.99),c(46,33,41),c(1,0,0),20000)
simProfit(scens[[6]],finaldata,c(119.99,119.99,119.99),c(33,46,41),c(0,1,0),20000)
simProfit(scens[[7]],finaldata,c(119.99,139.99,119.99),c(33,41,41),c(0,1,0),20000)
simProfit(scens[[8]],finaldata,c(119.99,139.99,119.99),c(46,41,41),c(1,1,0),20000)
simProfit(scens[[9]],finaldata,c(119.99,139.99,119.99),c(33,46,41),c(0,1,0),20000)
simProfit(scens[[10]],finaldata,c(139.99,139.99,119.99),c(46,41,41),c(1,1,0),20000)
#.....
