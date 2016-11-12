# Exploration of Bimbo datasets

# Libraries used
library(data.table)
library(dplyr)
library(ggplot2)

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

smallset_new = smallset %>% group_by(Week, AgencyID, ProductID) %>% 
  summarise(TotalReturns = sum(ReturnUnits)) %>% 
  mutate(Is.TrueDemand=ifelse(HasReturns>0, 1, 0))

smallset = smallset %>%
  inner_join(smallset_new, by=c("Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID"))
rm(smallset_new)

# Demand by agencies by week for all products
AgenciesDemandByWeek = smallset %>%
  group_by(AgencyID, Week) %>%
  summarise(NetDemand=sum(Demand), NetSales =sum(SalesAmount))

# products by routes - get number of distinct products by each route
# summarise multiple values into single value
# products_by_routes is a dataset with # of products for each route - helps to get an idea of routes with most number of products
ProductsByAgencies = train %>% group_by(Week, AgencyID) %>% summarise(numproducts = n_distinct(ProductID), totaldemand = sum(Demand), totalsales=sum(SalesAmount), average.demand=mean(Demand))


# get details such as first week product was introduced, total demand & average demand for the product across all clients / weeks
prod_week_introduced = smallset %>% 
  group_by(ProductID) %>% 
  summarise(week.introduced=min(Week), 
            week.total = sum(Demand),
            mean.demand = mean(Demand))

# get details across the entire training set 
prod_week_introduced = train %>% 
  group_by(ProductID) %>% 
  summarise(week.introduced=min(Week))

# join datasets based on Producto_ID
smallset_joined = smallset %>%
  left_join (prod_week_introduced, 
             by=c("ProductID"))

# include average demand of each product specific to each client (across all weeks information is available for)
prod_details = smallset %>% 
  group_by(ProductID, ClientID) %>% 
  summarise(mean.demandbyclient= mean(Demand))

smallset_joined = smallset_joined %>%
  left_join (prod_details, 
             by=c("ProductID", "ClientID"))


smallset2=subset(smallset, smallset$Week >= 7)
smallset3 = smallset2 %>% 
  group_by(ProductID, ClientID) %>% 
  summarise(NumRecentWeeks= n(), AvgDemandRecentWeeks=mean(Demand), RecentWeek=max(Week))

smallset2 = smallset2 %>%
  left_join(smallset3,
            by=c("ProductID", "ClientID"))
rm(smallset3)

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

# trend across agencies / routes / most common product


#select(Week, AgencyID, ChannelID, Route, ClientID, ProductID) %>%

# Identify products in test set that are not in train set
train.products = unique(train$ProductID)
#test.products = unique(test$ProductID)
#NewProducts = test.products[!(test.products %in% train.products)]
#str(NewProducts)

# demand by agencies
demand_by_agencies = smallset %>%
  group_by(AgencyID) %>%
  summarise(total.demand = sum(Demand))



