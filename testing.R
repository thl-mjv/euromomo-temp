library(euroMoMoTemp)

loadDefaults()
getOption("tempmomo")$all$cache$dir
file.path(getOption("tempmomo")$all$cache$dir,c("download","blaa"))

getEuroMap()