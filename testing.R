## install.packages("devtools")
## install.packages("rgeos")
library("devtools")
dev_mode()
install_github(username="thl-mjv",repo="euromomo-temp",subdir="euroMoMoTemp")

library(euroMoMoTemp)

loadDefaults()
getOption("tempmomo")$all$cache$dir
file.path(getOption("tempmomo")$all$cache$dir,c("download","blaa"))
getEuroMap()

sites<-noaaGetSites(TRUE)
require("rgeos")
require("maptools")
mappath<-file.path(getOption("tempmomo")$all$cache$dir,"NUTS_RG_03M_2010.shp")
getOption("tempmomo")$all$cache$dir
file.exists(mappath)
map<-readShapeSpatial(file.path(getOption("tempmomo")$all$cache$dir,"NUTS_RG_03M_2010.shp"))

foo<-noaaMapRegions(sites,map,3,"i")
table(foo)
