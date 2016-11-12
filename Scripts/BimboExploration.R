# Exploration of Bimbo datasets

# Libraries used
library(data.table)
library(dplyr)
library(ggplot2)
library(treemap)

#read in columns listed
train=fread('train.csv',select = c('Semana','Canal_ID', 'Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK', 'Demanda_uni_equil'))
test=fread('test.csv',select = c('Semana','id', 'Canal_ID', 'Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))

# read in the client/product files
client = read.csv("cliente_tabla.csv")
product = read.csv("producto_tabla.csv")
town = read.csv("town_state.csv")

#Weeks in Training Set - histogram (fraction of the entire set)
ggplot(train %>% sample_frac(0.005)) +
  geom_histogram(aes(x=Semana), color="black", fill="red", alpha=1.0) +
  scale_x_continuous(breaks=1:20) +
  scale_y_continuous(name="Client / Product deliveries") +
  theme_bw() 

#Weeks in Test Set - histogram (fraction of the entire set)
ggplot(test %>% sample_frac(0.005)) +
  geom_histogram(aes(x=Cana), color="black", fill="red", alpha=1.0) +
  scale_x_continuous(breaks=1:20) +
  scale_y_continuous(name="Client / Product deliveries") +
  theme_bw() 

#Channels in Training Set - histogram (fraction of the entire set)
ggplot(train %>% sample_frac(0.005)) +
  geom_histogram(aes(x=Canal_ID), color="black", fill="red", alpha=1.0) +
  scale_x_continuous(breaks=1:20) +
  scale_y_continuous(name="Client / Product deliveries") +
  theme_bw() 

#Channels in Training Set - histogram (fraction of the entire set)
ggplot(test %>% sample_frac(0.005)) +
  geom_histogram(aes(x=Canal_ID), color="black", fill="red", alpha=1.0) +
  scale_x_continuous(breaks=1:20) +
  scale_y_continuous(name="Client / Product deliveries") +
  theme_bw() 

#form a small set based on training data
smallset = train %>% sample_frac(0.005)

canals_by_semana = smallset %>%
                    group_by(Canal_ID, Semana) %>%
                    summarise(NetDemand=sum(Demanda_uni_equil))
                    
# view total demand for a specific week (week #3) across all channels
arrange(canals_by_semana[Semana==3], Canal_ID)

# treemap showing distribution of the demand across weeks by channel
#treemap(canals_by_semana, index="Canal_ID", vSize="NetDemand", type = "index", title = "Partition by Channels")

# plot of NetDemand (calculated in the dataset canals_by_semana) for each week (weeks 3 through 9)
#ggplot(canals_by_semana, aes(x=Semana, y=NetDemand)) + geom_bar(stat="identity") +
#  + scale_x_continuous(breaks=1:20)

# plot of NetDemand (calculated in the dataset canals_by_semana) for each channel (1 through 11)
#ggplot(canals_by_semana, aes(x=Canal_ID, y=NetDemand)) + geom_bar(stat="identity") + scale_x_continuous()
#ggplot(canals_by_semana, aes(x=Canal_ID, y=NetDemand)) + geom_bar(stat="identity") + scale_x_continuous(name="Demand by Channel", lim=c(0,11))
ggplot(canals_by_semana, aes(x=Canal_ID, y=NetDemand)) + geom_bar(stat="identity") + scale_x_continuous(name="Demand by Channel", breaks=1:11)


#unique counts for each factor
unique_clients = unique(train$Cliente_ID)
str(unique_clients)  # 880604 unique clients

unique_routes = unique(train$Ruta_SAK)
str(unique_routes)  # 3603 unique routes

unique_products = unique(train$Producto_ID)
str(unique_products)  # 1799 unique products

unique_agencies = unique(train$Agencia_ID)
str(unique_agencies)  # 552 unique agencies

# products by routes - get number of distinct products by each route
# summarise multiple values into single value
# products_by_routes is a dataset with # of products for each route - helps to get an idea of routes with most number of products
products_by_routes = smallset %>% group_by(Ruta_SAK) %>% summarise(numproducts = n_distinct(Producto_ID))
products_by_routes = train %>% group_by(Ruta_SAK) %>% summarise(numproducts = n_distinct(Producto_ID))


# clients by routes - get number of distinct clients by each route
# summarise multiple values into single value
# clients_by_routes is a dataset with # of clients for each route - helps to get an idea of routes with most number of clients
clients_by_routes = smallset %>% group_by(Ruta_SAK) %>% summarise(numclients = n_distinct(Cliente_ID))

# tag clients as chain based on number of occurrences of the client name with different Cliente_ID values
# new column 'NumWithSameName' gets distinct count of Cliente_ID with same name 
clients_by_num = client %>% group_by(NombreCliente) %>% summarise(NumWithSameName = n_distinct(Cliente_ID))

# new column that indicates if the store is a chain or not based on number of occurrences of Cliente_ID with same client names
clients_by_num = mutate(clients_by_num, Is.Chain=ifelse(NumWithSameName>=25, 1, 0))

# count of clients identified as chain stores -- 2309 stores
sum(clients_by_num$Is.Chain==1)

# get details such as first week product was introduced, total demand & average demand for the product across all clients / weeks
prod_week_introduced = smallset %>% 
                       group_by(Producto_ID) %>% 
                       summarise(week.introduced=min(Semana), 
                                 week.total = sum(Demanda_uni_equil),
                                 mean.demand = mean(Demanda_uni_equil))

# get details across the entire training set 
prod_week_introduced = train %>% 
  group_by(Producto_ID) %>% 
  summarise(week.introduced=min(Semana))

# join datasets based on Producto_ID
smallset_joined = smallset %>%
  left_join (prod_week_introduced, 
        by=c("Producto_ID"))

# include average demand of each product specific to each client (across all weeks information is available for)
prod_details = smallset %>% 
  group_by(Producto_ID, Cliente_ID) %>% 
  summarise(mean.demandbyclient= mean(Demanda_uni_equil))

smallset_joined = smallset_joined %>%
  left_join (prod_details, 
             by=c("Producto_ID", "Cliente_ID"))


# Identify products in test set that are not in train set
train.products = unique(train$Producto_ID)
test.products = unique(test$Producto_ID)
NewProducts = test.products[!(test.products %in% train.products)]
str(NewProducts)

# demand by agencies
demand_by_agencies = smallset %>%
  group_by(Agencia_ID) %>%
  summarise(total.demand = sum(Demanda_uni_equil))
