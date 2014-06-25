#' @name auxfun
#' @rdname auxfun
#' @title Auxilliary functions for tempmomo
#' @description 
#' Various little helper functions, not exported, for use within the package
NULL
## replace missing value in a with b
#' @rdname auxfun
na.0<-function(a,b=0) ifelse(is.na(a), b,a)

#' @rdname auxfun
## replace all a==b with missing value
to.na.0<-function(a,b=0) ifelse(a==b    ,NA,a)

#' @rdname auxfun
## transform farenheiths to celsius assuming 9999.9 missing
to.cels<-function(a) as.numeric(5*(ifelse(a>99.9,NA,a)-32)/9)
## transform dates to years with fractions
#' @rdname auxfun
numdate<-function(a) as.numeric(a-as.Date("2000-1-1"))/365.25+2000
## number of day in a year
#' @rdname auxfun
seasday<-function(a) as.numeric(format(as.Date(a),"%j"))/366
#' @description min and max with NA:s removed
#' @rdname auxfun
namax <-function(a) max (a,na.rm=TRUE)
#' @rdname auxfun
namin <-function(a) min (a,na.rm=TRUE)
#' @rdname auxfun
namean<-function(a) mean(a,na.rm=TRUE)
### calculate Monday from a date
#' @rdname auxfun
monday<-function(a,...) as.Date(a, ...)-na.0(to.na.0(as.numeric(format(as.Date(a,...),"%w"))), 7) + 1
#' @description create a list of full weeks between dates
#' @rdname auxfun
mondays<-function(first,last,...) { # optional parameters for as.Date
  first<-monday(first,...)
  last <-monday(last ,...)
  length<-as.numeric(last-first)/7
  first+(0:length)*7
}
#' @description create a factor variable of full series of mondays
#' @rdname auxfun
mondayf<-function(a,first,last,...) { # optional parameters for as.Date
  expected<-mondays(first,last,...)
  observed<-monday(a,...)
  factor(as.numeric(observed),levels=as.numeric(expected),labels=as.character(expected))
}
#' @description give the monday of first (last) full week
#' @rdname auxfun
fullweek<-function(a,type=c("first","last"),...) {
  a<-as.Date(a,...)
  m<-monday(a)
  type<-match.arg(type)
  if(type=="first") { # truncate to right
    if(a<m) m<-m+7
  } else { # truncate to left
    if(a>m) m<-m-7
  }
  m
}
#' @description create pertty grid of n frames
#' @rdname auxfun
mkpar<-function(n,row=TRUE) {
  mn<-ceiling(sqrt(n))
  if(row) c(ceiling(n/mn),mn) else c(mn,ceiling(n/mn))
}
