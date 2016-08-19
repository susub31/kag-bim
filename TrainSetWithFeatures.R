# Exploration of Bimbo datasets

# Libraries used
library(data.table)
library(dplyr)
library(ggplot2)

# read in columns listed & rename all column names
train=fread('train.csv',select = c('Semana','Agencia_ID','Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID', 'Venta_uni_hoy', 'Dev_uni_proxima', 'Demanda_uni_equil'))
train = setNames(train, c("Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID", "SalesUnits", "ReturnUnits", "Demand"))

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

# clients by routes - get number of distinct clients by each route
# summarise multiple values into single value
# clients_by_routes is a dataset with # of clients for each route - helps to get an idea of routes with most number of clients
clients_by_routes = smallset %>% group_by(Route) %>% summarise(numclients = n_distinct(Client_ID))

# tag clients as chain based on number of occurrences of the client name with different Cliente_ID values
# new column 'NumWithSameName' gets distinct count of Cliente_ID with same name 
# new column that indicates if the store is a chain or not based on number of occurrences of Cliente_ID with same client names
clients_by_num = client %>% group_by(ClientName) %>% 
  summarise(NumWithSameName = n_distinct(ClientID)) %>% 
  mutate(Is.Chain=ifelse(NumWithSameName>=25, 1, 0))

# count of clients identified as chain stores -- 2309 stores
sum(clients_by_num$Is.Chain==1)

# merge details on whether the client is part of chain store into the client dataset
client = client %>%
  inner_join(clients_by_num, by=c("ClientName"))
rm(clients_by_num)

#form a small set based on training data
#smallset = train %>% sample_frac(0.005)
smallset = train[ProductID < 750]

smallset_new = smallset %>% group_by(Week, AgencyID, ChannelID, Route, ClientID, ProductID) %>% 
  summarise(HasReturns = sum(ReturnUnits)) %>% 
  mutate(Is.TrueDemand=ifelse(HasReturns>0, 1, 0))

smallset = smallset %>%
  inner_join(smallset_new, by=c("Week", "AgencyID", "ChannelID", "Route", "ClientID", "ProductID"))
rm(smallset_new)

canalsByWeek = smallset %>%
                    group_by(ChannelID, Week) %>%
                    summarise(NetDemand=sum(Demand))
                    
# view total demand for a specific week (week #3) across all channels
arrange(canalsByWeek[Week==3], ChannelID)

# plot of NetDemand (calculated in the dataset canals_by_semana) for each channel (1 through 11)
ggplot(canalsByWeek, aes(x=ChannelID, y=NetDemand)) + geom_bar(stat="identity") + scale_x_continuous(name="Demand by Channel", breaks=1:11)


# products by routes - get number of distinct products by each route
# summarise multiple values into single value
# products_by_routes is a dataset with # of products for each route - helps to get an idea of routes with most number of products
#ProductsByRoutes = smallset %>% group_by(Route) %>% summarise(numproducts = n_distinct(ProductID))
ProductsByRoutes = train %>% group_by(Route) %>% summarise(numproducts = n_distinct(ProductID))


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



