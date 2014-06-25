library(euroMoMoTemp)

loadDefaults()
getOption("tempmomo")$all$cache$dir
file.path(getOption("tempmomo")$all$cache$dir,c("download","blaa"))

getEuroMap()

sites<-noaaGetSites(TRUE)
system.file("extdata", "noaa-countries.txt",package = "euroMoMoTemp")