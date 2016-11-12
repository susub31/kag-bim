# Libraries used
library(data.table)
library(dplyr)
library(xgboost)
#library(rowr)

setwd("//dcsinfosys.com/DCS/Users/SSubramanian/Documents/eDX Courses/MITx+15.071x_3/Kaggle Competition/Grupo Bimbo Inventory Demand/Files")
bimbotrain=fread('bimbotrain3.csv', select=c("Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID", "Demand", "NumRecent2Weeks", "NumRecent3Weeks", "NumRecent4Weeks", "NumRecent5Weeks", "RecentWeek", "cum2WeekAverage", "cum3WeekAverage", "cum4WeekAverage", "AverageDemand", "WeekIntroduced", "OrderWeekRank"))

test=fread('test.csv',select = c('id', 'Semana','Agencia_ID', 'Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID'))
test=setNames(test, c("ID", "Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID"))

# split training set based on channels
c1train = bimbotrain[ChannelID==1]
c2train = bimbotrain[ChannelID==2]
c4train = bimbotrain[ChannelID==4]
c5train = bimbotrain[ChannelID==5]
c6train = bimbotrain[ChannelID==6]
c7train = bimbotrain[ChannelID==7]
c8train = bimbotrain[ChannelID==8]
c9train = bimbotrain[ChannelID==9]
c11train= bimbotrain[ChannelID==11]

rm(bimbotrain)

# split test set based on channels
c1test = test[ChannelID==1]
c2test = test[ChannelID==2]
c4test = test[ChannelID==4]
c5test = test[ChannelID==5]
c6test = test[ChannelID==6]
c7test = test[ChannelID==7]
c8test = test[ChannelID==8]
c9test = test[ChannelID==9]
c11test= test[ChannelID==11]

rm(test)

write.csv(c1train, "c1train.csv")
write.csv(c2train, "c2train.csv")
write.csv(c4train, "c4train.csv")
write.csv(c5train, "c5train.csv")
write.csv(c6train, "c6train.csv")
write.csv(c7train, "c7train.csv")
write.csv(c8train, "c8train.csv")
write.csv(c9train, "c9train.csv")
write.csv(c11train, "c11train.csv")

write.csv(c1test, "c1test.csv")
write.csv(c2test, "c2test.csv")
write.csv(c4test, "c4test.csv")
write.csv(c5test, "c5test.csv")
write.csv(c6test, "c6test.csv")
write.csv(c7test, "c7test.csv")
write.csv(c8test, "c8test.csv")
write.csv(c9test, "c9test.csv")
write.csv(c11test, "c11test.csv")


