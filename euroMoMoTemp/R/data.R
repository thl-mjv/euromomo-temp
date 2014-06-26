#' @name european.countries
#' @title List of European countries
#' @description List of countries in Europe, including EU and other countries for which NUTS is defined
#'
#' @docType data
#' @keywords data
NULL

#' Load defaults for parameters
#'
#' Loads various defaults files and stores them globally. 
#' Simplifies the code as all country specific issues can be resolved in these files.
#' All files with pattern defaults-*.dat will be used in alphapetical order. 
#' Thus the latter files override the former. So, using defaults-global.dat and 
#' defaults-local.dat the latter will override the former. 
#' Also, you can use either .dat or .txt so that .txt overrides .dat
#'
#' There is also a default default in the package. It just states that the source files should 
#' be downloaded to a subdirectory "download" in the current directory
#'
#'@export
loadDefaults<-function() {
  files0<-system.file("extdata","defaults.txt",package="euroMoMoTemp")
  files1<-list.files(patt="^defaults-.*[.]dat$")
  files2<-list.files(patt="^defaults-.*[.]txt$")
  if(length(files1)+length(files2)==0) warning("No defaults files found, using package defaults")
  dats<-unlist(sapply(c(files0,sort(files1),sort(files2)),readLines))
  dats<-dats[!grepl("^#",dats)] # remove comments
  splits<-strsplit(dats,"=")
  labels<-sapply(splits,function(a) strsplit(a[1],"[.]")[[1]])
  values<-sapply(splits,function(a) a[2])
  optmat<-t(rbind(labels,values))
  out<-list()
  for(i in 1:nrow(optmat)) {
    country<-optmat[i,1]
    chapter<-optmat[i,2]
    option <-optmat[i,3]
    if(!country%in%names(out))
      out[[country]]<-list()
    if(!chapter%in%names(out[[country]]))
      out[[country]][[chapter]]<-list()
    out[[country]][[chapter]][[option]]<-optmat[i,4]
  }
  options(tempmomo=out)
  invisible(out)
}

#' Download the station specific metadata from the NOAA site
#'
#' Downloads the list of stations, reads it in and cleans the variables
#'
#' @param force should the file be downloaded or use the cached file?
#' @return a data frame of weather stations
#' @export
noaaGetSites<-function(force=FALSE) {
  ## todo (28.10.2013): Fix country codes using Ajay's table (done 28.10.2013)
  ## todo (28.10.2013): calculate the NUTS region for each site (done 06.11.2013)
  if(force) {
    sites<-read.table("ftp://ftp@ftp.ncdc.noaa.gov/pub/data/gsod/ish-history.csv",sep=",",header=TRUE)
    names(sites)<-c("usaf","wban","station.name","country","fips","state","call","lat","lon","elev","begin","end")
    sites$begin<-as.Date(as.character(sites$begin),"%Y%m%d")
    sites$end  <-as.Date(as.character(sites$end  ),"%Y%m%d")
    sites$lat  <-with(sites,ifelse(lat == -99999,NA,lat ))
    sites$lon  <-with(sites,ifelse(lon ==-999999,NA,lon ))
    sites$elev <-with(sites,ifelse(elev== -99999,NA,elev))
    sitetrans<-read.table(system.file("extdata","noaa-countries.txt",package="euroMoMoTemp"),sep=",",header=TRUE)
    sites$NUTS        <-sitetrans$NUTS   [match(sites$country,sitetrans$NOAA)]
    sites$country.name<-sitetrans$Country[match(sites$country,sitetrans$NOAA)]
    sites$Europe<-sites$country%in%european.countries
    sites$EuropeProper<-with(sites,(-28000<lon) & (lon<35000) & (33000<lat) & (lat<73000)) 
    warning("This sites file does not include proper NUTS region information.")
  } else {
    sites<-read.table("download/sites.dat") # FIXME: use default location for the cache
    sites$begin<-as.Date(as.character(sites$begin),"%Y-%m-%d")
    sites$end  <-as.Date(as.character(sites$end  ),"%Y-%m-%d")
  }
  ## TODO: need to add saving the file to above place
  ## TODO: fix nuts areas etc
  
  sites
}

#' Download map data from EuroStat
#' 
#' Downloads NUTS regions from EuroStat website at \url{http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/NUTS_2010_03M.zip}
#' @param year The year of NUTS regions (Use with caution)
#' @param resolution The resolution of the shapefile (use with caution)
#' @param force Should the file be downloaded even if it already exists
#' @param basepath The directory where the files should be downloaded 
#' @return None
#' @export
getEuroMap<-function(year="2010",resolution="03M",force=FALSE,basepath=NULL) {
  file<-paste("NUTS_",year,"_",resolution,"_SH.zip",sep="")
  url <-paste("http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/",file,sep="")
  if(is.null(basepath)) basepath<-getOption("tempmomo")$all$cache$dir
  down<-file.path(basepath,file)
  if(!file.exists(down) | force) {
    err<-try(download.file(url,down))
    if(inherits(err,"try-err"))
      stop("could not download file")
  }
  ## this is not windows compatible
  unzip(down,exdir=basepath,overwrite=TRUE,junkpaths=TRUE)
  #files<-list.files(path=file.path(basepath,paste("NUTS_",year,"_",resolution,"_SH",sep=""),"Data"),patt="NUTS_RG_*",full=TRUE)
  #comms<-paste("mv ",files," ",basepath,sep="")
  #sapply(comms,system)
  #system(paste("cd ",basepath," && rm -rf ",file," NUTS_",year,"_",resolution,"_SH",sep=""))
  invisible(NULL)
}
#' Match sites to map areas
#'
#' @param sites data frame with lat&lon
#' @param map   ShapeSpatialDataFrame with the regions
#' @param level Level of area
#' @param type  What kind of match should be returned
#' @return A matched label of the region
#' @export
noaaMapRegions<-function(sites,map=NULL,level=0,type=c("inarea","distance","bestguess")) {
  require("sp")
  require("maptools")
  require("fields")
  if(is.null(map)) { # default file
    map <- readShapeSpatial(file.path(getOption("tempmomo")$all$cache$dir,"NUTS_RG_03M_2010.shp"))
  }
  sp<-SpatialPolygons(subset(map,STAT_LEVL_==level)@polygons)
  pcents<-coordinates(sp)
  sites$row<-1:nrow(sites)
  csites<-subset(sites,!is.na(lat) & !is.na(lon))
  csites$x <- csites$lon/1000
  csites$y <- csites$lat/1000
  coordinates(csites) <- ~x+y
  
  e.st<-over(csites,sp,) # primary match
  
  dists <- rdist(pcents,csites@coords)
  dists.min <- apply(dists, 2, which.min)
  
  x.data.country <- subset(map,STAT_LEVL_==level)@data
  
  inarea<-as.character(x.data.country[e.st,2]) # site residing within the area
  distan<-as.character(x.data.country$NUTS_ID[dists.min]) # by minimun dist
  bestqu<-na.0(inarea,distan) # "best" guess
  
  ## Shlemiel at work here
  type<-match.arg(type)
  res<-switch(type,
              inarea   =inarea,
              distance =distan,
              bestquess=bestqu)
  res[match(sites$row,csites$row)]
}

##                                                                                          111111111111111111111111111111111111111111111111111
##          11111111112222222222333333333344444444445555555555666666666688888888889999999999000000000011111111112222222222333333333344444444445
## 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
## STN--- WBAN   YEARMODA    TEMP       DEWP      SLP        STP       VISIB      WDSP     MXSPD   GUST    MAX     MIN   PRCP   SNDP   FRSHTT
## 840010 99999  20130101    74.2  5    68.6  5  1011.9  5  1009.9  5   12.4  5    7.6  5   15.0  999.9    79.5    67.1   0.00I 999.9  000000

#' Downloads data for one station and year
#'
#' The NOAA site has each station's data separated into files. These files are further split into years with one directory for each year.
#' The data is downloaded, read in and transformed to use metric measurement units
#'
#' @param site     the identificator of the station, must be six letter string (i.e. with trailing 0's if shorter)
#' @param year     the year whose data is to be downloaded
#' @param force    should the data be downloaded even if it is found in the cache (default=FALSE)
#' @param thisy    should this years data be downloaded even if force=FALSE
#' @param uplag    how old files should be updated, days
#' @param basepath where the cached files should go
#' @return a dataframe with single years data from a single station
#' @export
noaaGetSiteYear<-function(site,year=2013,force=FALSE,thisy=TRUE,uplag=0,basepath=NULL) {
  file<-paste(site,"-99999-",year,".op.gz",sep="")
  url <-paste("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/",year,"/",file,sep="")
  if(is.null(basepath)) basepath<-getOption("tempmomo")$all$cache$dir
  down<-paste(basepath,file,sep="/")
  if(file.exists(down)) {
    lastupd<-as.Date(file.info(down)$mtime)
    updsinc<-min(as.numeric(Sys.Date()-lastupd))
    cat("Last updated",as.character(lastupd),",",updsinc,"days ago...")
  } else {
    lastupd<-NA
    updsinc<-uplag+1
    cat("New file, newver updated...")
  }
  if(year==format(Sys.Date(),"%Y") & thisy) force<-(uplag<updsinc)
  if(!file.exists(down) | force) {
    err<-try(download.file(url,down))
    if(inherits(err,"try-err"))
      stop("could not download file")
  }
  data<-read.fwf(gzfile(down),header=FALSE,skip=1,as.is=TRUE,
                 widths=diff(c(0,6,12,22,30,33,41,44,52,55,63,66,73,76,83,86,93,100,108,109,116,117,123,124,130,138)),
                 col.names=c("site","wban","yearmoda","temp","tempc","dewp","dewpc","slp","slpc","stp","stpc","visib","visibc",
                             "wdsp","wdspc","mxspd","gust",
                             "max","maxf","min","minf","prcp","prcpf","sndp","frshtt"))
  try(data$date <-as.Date(as.character(data$yearmoda),"%Y%m%d"))
  try(data$temp <-to.cels(data$temp))
  try(data$dewp <-to.cels(data$dewp))
  try(data$max  <-to.cels(data$max ))
  try(data$min  <-to.cels(data$min ))
  try(data$slp  <-to.na.0(data$slp,9999.9))
  try(data$stp  <-to.na.0(data$stp,9999.9))
  try(data$visib<-to.na.0(data$visib,999.9)*1.609344)
  try(data$wdsp <-to.na.0(data$wdsp ,999.9)*0.1852)
  try(data$mxspd<-to.na.0(data$mxspd,999.9)*0.1852)
  try(data$gust <-to.na.0(data$gust ,999.9)*0.1852)
  try(data$prcp <-to.na.0(data$prcp ,99.99)*25.4)
  try(data$sndp <-na.0(to.na.0(data$sndp ,999.9)*25.4))
  try(data$frshtt<-sprintf("%06d",data$frshtt))
  try(data$fog  <-as.numeric(substring(data$frshtt,1,1)))
  try(data$rain <-as.numeric(substring(data$frshtt,2,2)))
  try(data$snow <-as.numeric(substring(data$frshtt,3,3)))
  try(data$hail <-as.numeric(substring(data$frshtt,4,4)))
  try(data$thun <-as.numeric(substring(data$frshtt,5,5)))
  try(data$torn <-as.numeric(substring(data$frshtt,6,6)))
  thissite<-subset(sites,sprintf("%06d",usaf)==site)
  if(nrow(thissite)==0) cat("Site",site,"not found in sites\n")
  try(data$pop0 <-rep(mean(thissite$pop0),nrow(data)))
  try(data$pop3 <-rep(mean(thissite$pop3),nrow(data)))
  try(data$nuts3<-rep(as.character(thissite$NUTS3i[1]),nrow(data)))
  attr(data,"downloaded")<-na.0(lastupd,Sys.Date())
  data
}

#' Get all data from all stations in a country
#'
#' Go through all stations in the country by each year and download the data
#'
#' @param usecountry the name of the country as listed in ftp://ftp.ncdc.noaa.gov/pub/data/gsod/country-list.txt
#' @param years vector of years to use
#' @param sitesdata a data frame created using noaaGetSites
#' @param force Should the data be downloaded even if it exists in the cache
#' @param thisy Should this year's potentially incomplete data be downloaded even if it exists in the cache
#' @param basepath Directory where the data is downloaded (in subdirectory download)
#' @param uplag How old files should be updated
#' @param countrycode What variable in the sites -file gives the country name
#' @return A data frame with all stations and all years data
#' @export
noaaGetCountry<-function(usecountry="FI",years=2008:2013,sitesdata=sites,force=FALSE,thisy=TRUE,basepath=NULL,uplag=0,
                         countrycode=c("country","NUTS","NUTS0i","NUTS3i")) {
  ## todo (xx.05.2013): error checking for missing sitesdata
  ## todo (28.10.2013): allow selection of country using either fields country, NUTS, country.name
  countrycode<-match.arg(countrycode)
  csites<-try(subset(sitesdata,get(countrycode)==usecountry))
  if(inherits(csites,"try-error")) {
    sitesdata<-noaaGetSites()
    csites<-try(subset(sitesdata,get(countrycode)==usecountry))
  }
  cat(nrow(csites),"stations\n")
  res<-list()
  for(i in years) {
    ## kludge: if either begin or end is missing, use anyways
    ysites<-with(subset(csites,na.0(format(begin,"%Y"),"9999")<=i & na.0(format(end,"%Y"),"0") >= i),sprintf("%06d",usaf))
    cat(length(ysites),"stations year",i,"\n")
    yres<-list()
    for(j in ysites) {
      cat(usecountry,i,j,which(j==ysites),length(ysites),"...")
      cat(system.time(yres[[j]]<-noaaGetSiteYear(j,i,force=force,thisy=thisy,basepath=basepath,uplag=uplag))[3])
      cat(" seconds to download\n")
    }
    res[[i]]<-do.call("rbind",yres)
  }
  out<-do.call("rbind",res)
  attr(out,"downloaded")<-with(out,max(date)) # collect the date of last update. Should use max from yres?
  out
}
