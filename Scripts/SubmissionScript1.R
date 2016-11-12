# Submission Script for Bimbo 

# Libraries used
library(data.table)
library(dplyr)
library(xgboost)
library(rowr)

# read in columns listed & rename all column names
#train=fread('train.csv',select = c('Semana','Agencia_ID','Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID', 'Venta_uni_hoy', 'Dev_uni_proxima', 'Demanda_uni_equil'))
#train = setNames(train, c("Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID", "SalesUnits", "ReturnUnits", "Demand"))

bimbotrain=fread('train.csv',select = c('Semana','Agencia_ID','Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID', 'Venta_uni_hoy', 'Dev_uni_proxima', 'Demanda_uni_equil'))
bimbotrain = setNames(bimbotrain, c("Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID", "SalesUnits", "ReturnUnits", "Demand"))

bimbotrain = bimbotrain %>%
  group_by(ClientID, ProductID) %>%
  mutate(cum2WeekAverage=rollApply(Demand, mean, window=2, minimum=1, align="right"),
         cum3WeekAverage=rollApply(Demand, mean, window=3, minumum=1, align="right"),
         cum4WeekAverage=rollApply(Demand, mean, window=4, minumum=1, align="right"),
         AverageDemand=mean(Demand),
         OrderWeekRank=order(Week, decreasing=TRUE))

#OverallAverage=mean(c(cum2WeekAverage, cum3WeekAverage, cum4WeekAverage, AverageDemand)),

# additional features
#NumRecent2Weeks= sum(Week==9 | Week==8), 
#NumRecent3Weeks= sum(Week==9 | Week==8 | Week==7), 
#NumRecent4Weeks= sum(Week==9 | Week==8 | Week==7 | Week==6), 
#NumRecent5Weeks= sum(Week==9 | Week==8 | Week==7 | Week==6 | Week==5),
#RecentWeek=max(Week),


#bimbotrain = bimbotrain %>%
#  group_by(AgencyID, ChannelID, Route, ClientID) %>%
#  mutate(cum2WeekSalesAverage=rollApply(SalesAmount, mean, window=2, minimum=1, align="right"),
#         cum3WeekSalesAverage=rollApply(SalesAmount, mean, window=3, minumum=1, align="right"),
#         cum4WeekSalesAverage=rollApply(SalesAmount, mean, window=4, minumum=1, align="right"))


#nProducto_ID=bimbotrain[,.(DemandByProd=sum(Demand)),by=.(ProductID, Week)]
#nProducto_ID=nProducto_ID[,.(nProducto_ID=mean(nProducto_ID,na.rm=T)),by=ProductID]

RecentWeekData=filter(bimbotrain, OrderWeekRank==1)

test=fread('test.csv',select = c('id', 'Semana','Agencia_ID', 'Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID'))
test=setNames(test, c("ID", "Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID"))
train=RecentWeekData

train$ID=0
train[, tst:=0]
test[, tst:=1]

train$Week=NULL
setkey(train, ProductID, ClientID)
setkey(test, ProductID, ClientID)

data=merge(train, test, all.y=TRUE)
selectcols = colnames(data)
Client26 = filter(data, ClientID==26)
data$AgencyID.x=NULL
data$ChannelID.x=NULL
data$Route.x=NULL
data$Demand=NULL
data$ID.x=NULL
data$tst.x=NULL

data = setNames(data, c("ProductID", "ClientID", "SalesUnits", "ReturnUnits", "cum2WeekAverage", "cum3WeekAverage", "cum4WeekAverage", "AverageDemand", "OrderWeekRank", "ID", "Week", "AgencyID", "ChannelID", "Route", "tst"))
test = filter(data, tst==1)
test$Demand=0
test$Demand=log(test$Demand+1)
train$Demand=log(train$Demand+1)

rm(data)

features=names(train)[!(names(train) %in% c('ID',"Demand",'tst'))] 

wltst=sample(nrow(train),30000)  

dval<-xgb.DMatrix(data=data.matrix(train[wltst,features,with=FALSE]),
                  label=data.matrix(train[wltst,Demand]),missing=NA)
watchlist<-list(dval=dval)

lreg <- xgb.train(params=list(  objective="reg:linear", 
                               booster = "gbtree",
                               eta=0.1, 
                               max_depth=10, 
                               subsample=0.85,
                               colsample_bytree=0.7) ,
                 data = xgb.DMatrix(data=data.matrix(train[-wltst,features,with=FALSE]),
                                    label=data.matrix(train[-wltst,Demand]),missing=NA), 
                 nrounds = 75, 
                 verbose = 1,
                 print_every_n=5,
                 early_stopping_rounds    = 10,
                 watchlist           = watchlist,
                 maximize            = FALSE,
                 eval_metric='rmse'
)

# Make prediction for the 10th week
data_test1=test
pred<-predict(lreg,xgb.DMatrix(data.matrix(data_test1[,features,with=FALSE]),missing=NA))
res=exp(round(pred,5))-1


results=data.frame(id=data_test1$ID,Demanda_uni_equil=res)

results[results[,2]<0,2]=0
results[,2]=round(results[,2],1)
results[,1]=as.integer(results[,1])
class(results[,1])='int32'
options(digits=18)
write.csv(results,file='results2.csv',row.names=F)

