# Libraries used
library(data.table)

setwd("//dcsinfosys.com/DCS/Users/SSubramanian/Documents/eDX Courses/MITx+15.071x_3/Kaggle Competition/Grupo Bimbo Inventory Demand/Files/TestByChannel_Results")
c1results=fread('c1results1.csv')
c2results=fread('c2results1.csv')
c4results=fread('c4results1.csv')
c5results=fread('c5results1.csv')
c6results=fread('c6results1.csv')
c7results=fread('c7results1.csv')
c8results=fread('c8results1.csv')
c9results=fread('c9results1.csv')
c11results=fread('c11results1.csv')


results=rbind(c1results, c2results)
results=rbind(results, c4results)
results=rbind(results, c5results)
results=rbind(results, c6results)
results=rbind(results, c7results)
results=rbind(results, c8results)
results=rbind(results, c9results)
results=rbind(results, c11results)

str(results)
summary(results)
nrow(results)

write.csv(results, "combinedresults1.csv", row.names = F)

rm(c1results)
rm(c2results)
rm(c4results)
rm(c5results)
rm(c6results)
rm(c7results)
rm(c8results)
rm(c9results)
rm(c11results)
rm(results)

