# script for channel 1
#set.seed(100)
setwd("//dcsinfosys.com/DCS/Users/SSubramanian/Documents/eDX Courses/MITx+15.071x_3/Kaggle Competition/Grupo Bimbo Inventory Demand/Files")

# training set - c1train
c1train=fread('c1train.csv', select=c("Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID", "Demand", "NumRecent2Weeks", "NumRecent3Weeks", "NumRecent4Weeks", "NumRecent5Weeks", "RecentWeek", "cum2WeekAverage", "cum3WeekAverage", "cum4WeekAverage", "AverageDemand", "WeekIntroduced", "OrderWeekRank"))

# testing set  - c1test
c1test=fread('c1test.csv',select = c("ID", "Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID"))


#get list of all products introduced after week #3; calculate average demand in the first week introduced
proddemand=c1train[Week==WeekIntroduced & Week==3]
newproddemand=c1train[Week==WeekIntroduced & Week>3]

proddemand=proddemand[, .(AvgProdDemand= mean(Demand,na.rm=T)), by=ProductID]
newproddemand=newproddemand[, .(AvgProdDemand= mean(Demand,na.rm=T)), by=ProductID]

newproddemand=rbind(proddemand, newproddemand)

newproddemand = newproddemand %>%
  group_by(ProductID) %>%
  summarise(AvgProdDemand=mean(AvgProdDemand))
rm(proddemand)

ClientProdAverage = c1train[, .(ClientProdAvg=mean(Demand, na.rm=T)), by=c("ProductID","ClientID")]
c1train = merge(c1train, ClientProdAverage, by=c("ProductID","ClientID"), all.x=T)

c1train = merge(c1train, newproddemand, by="ProductID", all.x=T)
c1train = c1train[, cumAverage:=(cum2WeekAverage + cum3WeekAverage + cum4WeekAverage + AverageDemand)/4]
c1train = c1train[, cumRecentAverage:=(cum2WeekAverage + cum3WeekAverage + cum4WeekAverage)/3]
c1train = c1train[, NewCumAverage:=ifelse((NumRecent4Weeks >=2), cumRecentAverage, (cumAverage + AverageDemand)/2 )]
#RecentWeekData=filter(bimbotrain, OrderWeekRank==1)

train=c1train[Week>7]
test = c1test
rm(c1train)
rm(c1test)

#Add new column to indicate it is not from test set (train dataset)
train[,tst:=0]
train$WeekIntroduced=NULL
train$OrderWeekRank=NULL
train$ID=0
train$target = train$Demand
train$Demand=NULL

#set target variable to 0 for test set
test$target=0

# merge details from newproddemand will have only averages for products in training set
test =  merge(test,  newproddemand, by="ProductID", all.x=TRUE)

# merge details from ClientProdAverage will have only averages for clients/products in training set
test = merge(test, ClientProdAverage, by=c("ProductID","ClientID"), all.x=T)

train$NewAverage=0
train$NewAverage=ifelse(train$NewCumAverage==0, ifelse(train$ClientProdAvg==0, train$AvgProdDemand, train$ClientProdAvg), train$NewCumAverage)

test$cumAverage=0
test$NewAverage=0
test$NewAverage=ifelse(test$cumAverage==0, ifelse((is.na(test$ClientProdAvg) |test$ClientProdAvg==0), test$AvgProdDemand, test$ClientProdAvg), test$cumAverage)

#test$NewAverage=ifelse(test$cumAverage==0, ifelse(test$ClientProdAvg==0, test$AvgProdDemand, test$ClientProdAvg), test$cumAverage)

# dataset with products that have entries in training set
testintrain=test[!is.na(NewAverage)]

# dataset with products that do not have entries in training set
testnotintrain=test[is.na(NewAverage)]

# loop through each ClientID in the dataset 'testnotintrain' and get the 
# average demand when new product is introduced specific to the client
# Use ClientAverage for products which are new in test set as compared to training set
if(nrow(testnotintrain) > 0) {
  for (i in 1:nrow(testnotintrain)) {
    clientid= testnotintrain[i]$ClientID
    #print (paste(clientid))
    testnotintrain[i]$NewAverage=mean(train[ClientID==clientid]$NewAverage)
  }
  rm(clientid)
  rm(i)
}

# Predict using model only for those products in training set
test = testintrain
#Add new column to indicate test set
test[,tst:=1]

backuptrain=train
train$cum2WeekAverage=NULL
train$cum3WeekAverage=NULL
train$cum4WeekAverage=NULL
train$NumRecent2Weeks=NULL
train$NumRecent3Weeks=NULL
train$NumRecent4Weeks=NULL
train$NumRecent5Weeks=NULL
train$RecentWeek=NULL
train$NewCumAverage=NULL
train$cumRecentAverage=NULL
#train$AverageDemand=NULL

#test$AvgProdDemand.y=NULL
#test$AvgProdDemand = test$AvgProdDemand.x
#test$AvgProdDemand.x=NULL
#test$NewAverage=0
test$AverageDemand=0

#combine train and test datasets
data=rbind(train,test)

#remove test and train datasets from workspace
#rm(test)  
#rm(train)
#train = data[tst==0]
#test=data[tst==1]

data1<-data[,.(Week=Week+1,ClientID, ProductID, NewAverage)]
data=merge(data,data1[Week>8,.(targetl1=mean(NewAverage)), by=.(Week,ClientID,ProductID)],all.x=T, by=c("Week","ClientID","ProductID"))

#data1<-data[,.(Week=Week+2,ClientID, ProductID, target, NewAverage)]
#data=merge(data,data1[Week>8,.(targetl2=mean(NewAverage)), by=.(Week,ClientID,ProductID)],all.x=T, by=c("Week","ClientID","ProductID"))
rm(data1)

data$targetl1=ifelse(is.na(data$targetl1), data$ClientProdAvg, data$targetl1)

#data$targetl1=ifelse((data$Week==11), data$targetl2, data$targetl1)

#select data where week# is 9 and above
data=data[Week>8,]

# Creating frequency features for some factor variables
# get number of records for each agency - grouped by week
nAgencyID=data[,.(nAgencyID=.N),by=.(AgencyID,Week)]
nAgencyID=nAgencyID[,.(nAgencyID=mean(nAgencyID,na.rm=T)),by=AgencyID]
data=merge(data,nAgencyID,by='AgencyID',all.x=T)

nRoute=data[,.(nRoute=.N),by=.(Route,Week)]
nRoute=nRoute[,.(nRoute=mean(nRoute,na.rm=T)),by=Route]
data=merge(data,nRoute,by='Route',all.x=T)

nClientID=data[,.(nClientID=.N),by=.(ClientID,Week)]
nClientID=nClientID[,.(nClientID=mean(nClientID,na.rm=T)),by=ClientID]
data=merge(data, nClientID, by='ClientID',all.x=T)

nProductID=data[,.(nProductID=.N),by=.(ProductID,Week)]
nProductID=nProductID[,.(nProductID=mean(nProductID,na.rm=T)),by=ProductID]
data=merge(data,nProductID,by='ProductID',all.x=T)

data$target=log(data$target+1)
#data$target = exp(data$target) - 1

#data=rbind(data_train,data_test)
data_train=data[tst==0,]
data_test=data[tst==1,]

# select features for modeling
features=names(data_train)[!(names(data_train) %in% c("ID","target","tst","AvgProdDemand","cumAverage", "NewAverage", "AverageDemand"))] 
rm(data)

samplesize=ifelse(nrow(data_train) < 30000, nrow(data_train)-(0.25*nrow(data_train)), 30000)
wltst=sample(nrow(data_train), samplesize)  

dval<-xgb.DMatrix(data=data.matrix(data_train[wltst,features,with=FALSE]),
                  label=data.matrix(data_train[wltst,target]),missing=NA)
watchlist<-list(dval=dval)

clf <- xgb.train(params=list(  objective="reg:linear", 
                               booster = "gbtree",
                               eta=0.1, 
                               max_depth=10, 
                               subsample=0.85,
                               colsample_bytree=0.7) ,
                 data = xgb.DMatrix(data=data.matrix(data_train[-wltst,features,with=FALSE]),
                                    label=data.matrix(data_train[-wltst,target]),missing=NA), 
                 nrounds = 75, 
                 verbose = 1,
                 print_every_n=5,
                 early_stopping_rounds    = 10,
                 watchlist           = watchlist,
                 maximize            = FALSE,
                 eval_metric='rmse'
)

# Make prediction for the 10th week
data_test1=data_test[Week==10,]
pred<-predict(clf,xgb.DMatrix(data.matrix(data_test1[,features,with=FALSE]),missing=NA))
#res=round(pred,5)
res=exp(round(pred,5))-1

# Create lagged values of target variable which will be used as a feature for the 11th week prediction 
data_test_lag1=data_test1[,.(ClientID,ProductID)]
data_test_lag1$targetl1=res
data_test_lag1=data_test_lag1[,.(targetl1=mean(targetl1)), by=.(ClientID,ProductID)]

results=data.frame(id=data_test1$ID,Demanda_uni_equil=res)

data_test2=data_test[Week==11,]
data_test2[,targetl1:=NULL]

# Merge lagged values of target variable to test the set for the 11th week
data_test2=merge(data_test2,data_test_lag1,all.x=T,by=c('ClientID','ProductID'))
pred<-predict(clf,xgb.DMatrix(data.matrix(data_test2[,features,with=FALSE]),missing=NA))
res=exp(round(pred,5))-1
res.df=data.frame(id=data_test2$ID,Demanda_uni_equil=res)
results=rbind(results, res.df)

str(results)
res.newdf=data.frame(id=testnotintrain$ID, Demanda_uni_equil=testnotintrain$NewAverage)
results=rbind(results, res.newdf)
results$Demanda_uni_equil=ifelse(is.na(results$Demanda_uni_equil), 3, results$Demanda_uni_equil)

results[results[,2]<0,2]=0
results[,2]=round(results[,2],0)
results[,1]=as.integer(results[,1])
class(results[,1])='int32'
options(digits=18)

write.csv(results,file='c1results1.csv',row.names=F)



#remove datasets after run
rm(data_test)
rm(data_train)

rm(data_test_lag1)
rm(data_test1)
rm(data_test2)

rm(res.df)
rm(results)
rm(res.newdf)
rm(testintrain)

rm(clf)
rm(samplesize)
rm(dval)
rm(features)
rm(pred)
rm(res)
rm(watchlist)
rm(wltst)


rm(nAgencyID)
rm(nClientID)
rm(nProductID)
rm(nRoute)
rm(testnotintrain)
rm(newproddemand)
rm(test)
rm(train)
rm(backuptrain)
rm(ClientProdAverage)

