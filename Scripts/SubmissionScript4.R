# Submission Script for Bimbo 

# Libraries used
library(data.table)
library(dplyr)
library(xgboost)
library(rowr)

# read in bimbotrain dataset, which has aggregated columns already calculated and saved
#bimbotrain=fread('bimbotrain.csv', select=c("Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID", "Demand", "cum2WeekAverage", "cum3WeekAverage", "cum4WeekAverage", "AverageDemand", "WeekIntroduced", "OrderWeekRank"))
setwd("//dcsinfosys.com/DCS/Users/SSubramanian/Documents/eDX Courses/MITx+15.071x_3/Kaggle Competition/Grupo Bimbo Inventory Demand/Files")
bimbotrain=fread('bimbotrain2.csv', select=c("Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID", "Demand", "WeekIntroduced", "cumAverage", "AvgProdDemand", "OrderWeekRank"))

#get list of all products introduced after week #3; calculate average demand in the first week introduced
newproddemand=bimbotrain[Week==WeekIntroduced & Week>3]
newproddemand = newproddemand %>%
  group_by(ProductID) %>%
  summarise(AvgProdDemand=mean(Demand))

#bimbotrain = merge(bimbotrain, newproddemand, by="ProductID", all.X=TRUE)
#bimbotrain[, cumAverage:=(cum2WeekAverage + cum3WeekAverage + cum4WeekAverage + AverageDemand)/4]

#test using smallset
#smallset=bimbotrain[ProductID<100]
#smallset = merge(smallset, newproddemand, by="ProductID", all.X=TRUE)

#smallset[, cumAverage:=(cum2WeekAverage + cum3WeekAverage + cum4WeekAverage + AverageDemand)/4]

#RecentWeekData=filter(bimbotrain, OrderWeekRank==1)

test=fread('test.csv',select = c('id', 'Semana','Agencia_ID', 'Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID'))
test=setNames(test, c("ID", "Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID"))

train=bimbotrain[Week>7]

#Add new column to indicate it is not from test set (train dataset)
train[,tst:=0]
train$WeekIntroduced=NULL
train$OrderWeekRank=NULL
train$ID=0
train$target = train$Demand
train$Demand=NULL

#set target variable to 0 for test set
test$target=0
test$cumAverage=0
test = merge(test, newproddemand, by="ProductID", all.x=TRUE)

#Add new column to indicate test set
test[,tst:=1]

train$NewAverage=ifelse(train$cumAverage==0, train$AvgProdDemand, train$cumAverage)
test$NewAverage=ifelse(test$cumAverage==0, test$AvgProdDemand, test$cumAverage)

#test$NewAverage=0
backuptrain=train

#train$Week=9

rm(bimbotrain)

#combine train and test datasets
data=rbind(train,test)

#remove test and train datasets from workspace
rm(test)  
rm(train)
train = data[tst==0]
test=data[tst==1]

data1<-data[,.(Week=Week+1,ClientID, ProductID, NewAverage)]
data=merge(data,data1[Week>8,.(targetl1=mean(NewAverage)), by=.(Week,ClientID,ProductID)],all.x=T, by=c("Week","ClientID","ProductID"))

#data1<-data[,.(Week=Week+2,ClientID, ProductID, target, NewAverage)]
#data=merge(data,data1[Week>8,.(targetl2=mean(target), NewAveragel2=mean(NewAverage)), by=.(Week,ClientID,ProductID)],all.x=T, by=c("Week","ClientID","ProductID"))

rm(data1)
#select data where week# is 9 and above
data=data[Week>8,]

#data$targetl1[is.na(data$targetl1)] = data$AvgProdDemand

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

#data=rbind(data_train,data_test)
data$target=log(data$target+1)
#data$target = exp(data$target) - 1

data_train=data[tst==0,]
data_test=data[tst==1,]
#data_train [is.na(Demandlag1)] <- data_train$AvgProdDemand

#lm1=lm(target ~ ClientID + ProductID + Route + AgencyID + ChannelID + NewAverage + targetl1 + nAgencyID + nRoute + nClientID + nProductID, data=data_train)
#pred1=predict(lm1, newdata=data_test)
#res1=exp(round(pred1,5))-1
#res1=round(pred1,5) 

#res1.df=data.frame(id=data_test$ID,Demanda_uni_equil=res1)
#head(res1.df)


features=names(data_train)[!(names(data_train) %in% c("ID","target","tst","AvgProdDemand","cumAverage","NewAverage"))] 
rm(data)

wltst=sample(nrow(data_train),30000)  

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

results[results[,2]<0,2]=0
results[,2]=round(results[,2],0)
results[,1]=as.integer(results[,1])
class(results[,1])='int32'
options(digits=18)
write.csv(results,file='results7.csv',row.names=F)




rm(data_test)
rm(data_test_lag1)
rm(data_test1)
rm(data_test2)
rm(data_train)
rm(res.df)
rm(res1.df)
rm(results)
rm(results4)

