#' @title bin multidimensional data into a two-way contingency table
#' @name bin2d
#' @description a function to bin multidimensional data into a two-way contingency table
#' @param x Data matrix: every column refers to a categorical variable
#' @param column1,column2 according to the column1-th,column2-th columns to classify 
#' @param WhetherNominal1 logical;if \code{TRUE}, the column1-th variable is a nominal variable;if \code{FALSE},the column1-th variable is a numerical variable
#' @param WhetherNominal2 logical;if \code{TRUE}, the column2-th variable is a nominal variable;if \code{FALSE},the column2-th variable is a numerical variable
#' @param breaks1,breaks2 see the argument \code{breaks} of \code{Histograms}
#' @return a list including the table of bin frequencies, vectors of breakpoints, and vectors of midpoints
#' @import Ball DAAG MASS RANN boot bootstrap energy microbenchmark xtable
#' @importFrom Rcpp cppFunction
#' @importFrom graphics hist persp
#' @useDynLib StatComp20029
#' @examples
#' \dontrun{
#' bin2d(iris,1,5,FALSE,TRUE)
#' }
#' @export
bin2d<-function(x,column1,column2,WhetherNominal1=FALSE,WhetherNominal2=FALSE,breaks1 = "Sturges", breaks2 = "Sturges"){ 
  
  if (WhetherNominal1==FALSE){
    histg1 <- hist(x[,column1], breaks = breaks1, plot = FALSE) 
    br1 <- histg1$breaks
    mids1 <- histg1$mids
  }else {br1<-colnames(x)[column1];mids1 <-unique(x[,column2])}
  
  if (WhetherNominal2==FALSE){
    histg2 <- hist(x[,column2], breaks = breaks2, plot = FALSE) 
    br2 <- histg2$breaks
    mids2 <- histg2$mids
  }else {br2<-colnames(x)[column2];mids2 <-unique(x[,column2])}
  
  freq <- with(x,table(
    if (WhetherNominal1==FALSE) cut(x[,column1],br1) else get(br1), 
    if (WhetherNominal2==FALSE) cut(x[,column2],br2) else get(br2)))
  
  return(list(call = match.call(), freq = freq, breaks1 = br1, breaks2 = br2, mids1 = mids1, mids2 = mids2))
}

#' @title construct density polygon of bivariate data
#' @description construct a bivariate frequency polygon
#' @param x Data matrix x is n by 2
#' @param breaks1,breaks2 see the argument \code{breaks} of \code{Histograms}
#' @return a bivariate frequency polygon
#' @examples
#' \dontrun{
#' Dplot(faithful)
#' Dplot(matrix(rnorm(1000*2),1000,2))
#' }
#' @export
Dplot<-function(x,breaks1 = "Sturges", breaks2 = "Sturges"){
  histg1 <- hist(x[,1], breaks = breaks1, plot = FALSE) 
  histg2 <- hist(x[,2], breaks = breaks2, plot = FALSE)
  br1 <- histg1$breaks
  br2 <- histg2$breaks
  freq <- table(cut(x[,1], br1), cut(x[,2], br2))
  h1 <- diff(br1) 
  h2 <- diff(br2)
  n <- nrow(x)
  h <- outer(h1, h2, "*")
  Z <- freq / (n * h) # the density estimate
  persp(x=histg1$mids, y=histg2$mids, z=Z, shade=TRUE, xlab="Column1", ylab="Column2", zlab="density estimate",main="", theta=45, phi=30, ltheta=60)
}


