# Working Theory of Predictive Features of Bimbo dataset

# Libraries used
library(data.table)
library(dplyr)
library(ggplot2)
library(zoo)
library(rowr)

# read in columns listed & rename all column names
train=fread('train.csv',select = c('Semana','Agencia_ID','Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID', 'Venta_uni_hoy', 'Venta_hoy', 'Dev_uni_proxima', 'Dev_proxima',  'Demanda_uni_equil'))
train = setNames(train, c("Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID", "SalesUnits", "SalesAmount", "ReturnUnits", "ReturnAmount", "Demand"))

# read in the client/product files
client = read.csv("cliente_tabla.csv")
client = setNames(client, c("ClientID", "ClientName"))

product = read.csv("producto_tabla.csv")
product = setNames(product, c("ProductID", "ProductName"))

town = read.csv("town_state.csv")
town = setNames(town, c("AgencyID", "Town", "State"))
town = as.data.table(town)

# capture only town & state to assign unique identifiers for Town-State combination
townstate = unique(town[, .(Town, State)])
townstate$TownState_id = as.numeric(as.factor(with(townstate, paste(Town, State))))

# join town and townstate datasets by Town & State to link unique_id to AgencyID
town = town %>%
  inner_join (townstate, 
              by=c("Town", "State"))
rm(townstate)

#form a small set based on training data
#smallset = train %>% sample_frac(0.005)
smallset = train[ProductID < 750]

smallset_new = smallset %>% group_by(Week, AgencyID, ChannelID, Route, ClientID, ProductID) %>% 
  summarise(TotalReturns = sum(ReturnUnits)) %>% 
  mutate(Is.TrueDemand=ifelse(TotalReturns>0, 1, 0))

smallset = smallset %>%
  inner_join(smallset_new, by=c("Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID"))
rm(smallset_new)

# number of weeks with demand for product for client in recent 2, 3, 4 and 5 weeks
smallset3 = smallset %>% 
  group_by(ProductID, ClientID) %>% 
  summarise(NumRecent2Weeks= sum(Week==9 | Week==8), 
            NumRecent3Weeks= sum(Week==9 | Week==8 | Week==7), 
            NumRecent4Weeks= sum(Week==9 | Week==8 | Week==7 | Week==6), 
            NumRecent5Weeks= sum(Week==9 | Week==8 | Week==7 | Week==6 | Week==5), 
            RecentWeek=max(Week))


smallset3 = smallset %>% 
  group_by(Route, ProductID) %>% 
  summarise(NumRecent2Weeks= sum(Week==9 | Week==8), 
            NumRecent3Weeks= sum(Week==9 | Week==8 | Week==7), 
            NumRecent4Weeks= sum(Week==9 | Week==8 | Week==7 | Week==6), 
            NumRecent5Weeks= sum(Week==9 | Week==8 | Week==7 | Week==6 | Week==5), 
            RecentWeek=max(Week))


# df=df %>% group_by(year) %>% mutate(rolltemp=rollapply(temp, 3, mean,  fill=NA, align="right"))
smallset4 = smallset %>%
  group_by(ClientID, ProductID) %>%
  summarise(NumRecent2Weeks= sum(Week==9 | Week==8), 
            NumRecent3Weeks= sum(Week==9 | Week==8 | Week==7), 
            NumRecent4Weeks= sum(Week==9 | Week==8 | Week==7 | Week==6), 
            NumRecent5Weeks= sum(Week==9 | Week==8 | Week==7 | Week==6 | Week==5)) 

smallset4 = smallset %>%
  group_by(ClientID, ProductID) %>%
  mutate(cum2WeekAverage=rollApply(Demand, mean, window=2, minimum=1, align="right"),
         cum3WeekAverage=rollApply(Demand, mean, window=3, minumum=1, align="right"),
         cum4WeekAverage=rollApply(Demand, mean, window=4, minumum=1, align="right"))

smallset5 = smallset %>%
  group_by(AgencyID, ChannelID, Route, ClientID) %>%
  mutate(cum2WeekSalesAverage=rollApply(SalesAmount, mean, window=2, minimum=1, align="right"),
         cum3WeekSalesAverage=rollApply(SalesAmount, mean, window=3, minumum=1, align="right"),
         cum4WeekSalesAverage=rollApply(SalesAmount, mean, window=4, minumum=1, align="right"))


