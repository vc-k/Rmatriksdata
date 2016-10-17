#' Fetch Symbols
#'
#' Fetches symbol data from historical data service
#'
#' @param exchange_id A numeric value or NULL(meaning all) of exchanges.
#' @param market A character value or NULL(meaning all) of markets.
#' @param state A character value or NULL(meaning all) of  symbol activity.One of "active" ,"passive" and "all".
#'
#' \deqn{You can enter the default arguments(NULL) 'exchange_id', 'market' and 'state' as empty arrays [ ].}
#' @return A data.frame is returned that  contains symbols on Matriks.
#' @seealso \code{\link{matriksExchanges}},\code{\link{matriksMarkets}}, \code{\link{matriksMarketsViop}}
#' @export
matriksSymbols <- function (exchange_id=NULL,market=NULL,state=NULL){

  auth<-suppressWarnings(tryCatch(strsplit(readBin("~/matriks/.tkn","character"),",")[[1]][2],
                                  error=function(e) {auth<-getToken()}))



  mainurl <- "http://web1.matriksdata.com/dumrul-api/v2/meta/symbols?"

  if(!is.null(exchange_id)){
    mainurl <- paste(mainurl,"&exchange_id=",exchange_id,sep = "")
  }
  if(!is.null(market)){
    mainurl<-paste(mainurl,"&market=",market,sep="")
  }
  if(!is.null(state)){
    mainurl<-paste(mainurl,"&state=",state,sep = "")
  }

  # mainurl <- paste(exchange_id,market,state,sep="&")
  # mainurl <- paste(urlhead,  mainurl,sep="")

  req <- GET(mainurl, add_headers(Authorize = paste("jwt",auth)))
  if(req$status_code!=200){
    auth<-getToken()
    req <- GET(mainurl, add_headers(Authorize = paste("jwt",auth)))
  }

  temp <- fromJSON(content(req,"text"))
  temp <- temp[,c('symbolCode','symbolId','deleted','description','exchangeId','fraction','marketCode')]
  return(temp)
  # temp<-unlist(strsplit(temp,'"'))
  #
  # symbol<-temp[2+which(temp=="symbol")]
  #
  # id<-temp[1+which(temp=="id")]
  # id<-as.numeric(substr(id,2,nchar(id)-1))
  #
  # active<-temp[1+which(temp=="active")]
  # active<-substr(active,2,nchar(active)-1)
  #
  # description<-temp[2+which(temp=="description")]
  #
  # exchange_id<-temp[1+which(temp=="exchange_id")]
  # exchange_id<-as.numeric(substr(exchange_id,2,nchar(exchange_id)-1))
  #
  # fraction<-temp[1+which(temp=="fraction")]
  # fraction<-as.numeric(substr(fraction,2,nchar(fraction)-1))
  #
  # market_code<-temp[2+which(temp=="market_code")]
  #
  # return(data.frame(symbol,id,active,description,exchange_id,fraction,market_code))
}

#' Fetch Exchanges
#'
#' Fetches exchange data from historical data service
#'
#' @return A data.frame is returned that contains  exchanges' code and description  on Matriks.
#' @details
#' All of exchanges on Matriks, is returned.There is not any classification.
#' @seealso  \code{\link{matriksSymbols}},\code{\link{matriksMarkets}}, \code{\link{matriksMarketsViop}}
#' @export
matriksExchanges <- function (){

  auth<-suppressWarnings(tryCatch(strsplit(readBin("~/matriks/.tkn","character"),",")[[1]][2],
                                  error=function(e) {auth<-getToken()}))

  mainurl <- "http://web1.matriksdata.com/dumrul-api/v2/meta/exchanges?"

  req <- GET(mainurl, add_headers(Authorize = paste("jwt",auth)))
  if(req$status_code!=200){
    auth<-getToken()
    req <- GET(mainurl, add_headers(Authorize = paste("jwt",auth)))
  }

  temp <- fromJSON(content(req,"text"))
  return(temp)
  # temp<-content(req,"text")
  # temp<-unlist(strsplit(temp,'"'))
  #
  # id<-temp[1+which(temp=="id")]
  # id<-as.numeric(substr(id,2,nchar(id)-1))
  #
  # code<-temp[2+which(temp=="code")]
  # code<-substr(code,2,nchar(code)-1)
  #
  # description<-temp[2+which(temp=="description")]
  #
  # return(data.frame(id,code,description))
}

#' Fetch Markets
#'
#' Fetches market data from historical data service
#'
#' @return A data.frame is returned that contains markets' code and descrioption on Matriks.
#' @details
#' All of markets on Matriks, is returned.There is not any classification.
#'@seealso \code{\link{matriksSymbols}},\code{\link{matriksExchanges}}, \code{\link{matriksMarketsViop}}
#'@export
matriksMarkets <- function (){

  auth<-suppressWarnings(tryCatch(strsplit(readBin("~/matriks/.tkn","character"),",")[[1]][2],
                                  error=function(e) {auth<-getToken()}))

  mainurl <- "http://web1.matriksdata.com/dumrul-api/v2/meta/markets?"

  req <- GET(mainurl, add_headers(Authorize = paste("jwt",auth)))
  if(req$status_code!=200){
    auth<-getToken()
    req <- GET(mainurl, add_headers(Authorize = paste("jwt",auth)))
  }
  temp <- fromJSON(content(req,"text"))
  return(temp)
  # temp<-content(req,"text")
  # temp<-unlist(strsplit(temp,'"'))
  #
  # market_code<-temp[2+which(temp=="market_code")]
  #
  # description<-temp[2+which(temp=="description")]
  #
  # return(data.frame(market_code,description))
}

#' Fetch Futures and Options Markets
#'
#' Fetches futures and options market data from historical data service
#'
#' @return A data.frame is returned that contains futures and options markets on Matriks with market code, description and id.
#' @details
#' All of futures and options markets on Matriks, is returned.There is not any classification.
#' @seealso \code{\link{matriksSymbols}},\code{\link{matriksExchange}},\code{\link{matriksMarkets}}
#' @export
matriksMarketsViop <- function (){

  auth<-suppressWarnings(tryCatch(strsplit(readBin("~/matriks/.tkn","character"),",")[[1]][2],
                                  error=function(e) {auth<-getToken()}))

  mainurl <- "http://web1.matriksdata.com/dumrul-api/v2/meta/markets/viop?"

  req <- GET(mainurl, add_headers(Authorize = paste("jwt",auth)))
  if(req$status_code!=200){
    auth<-getToken()
    req <- GET(mainurl, add_headers(Authorize = paste("jwt",auth)))
  }

  temp <- fromJSON(content(req,"text"))
  return(temp)
  # temp<-content(req,"text")
  # temp<-unlist(strsplit(temp,'"'))
  #
  # market_code<-temp[2+which(temp=="market_code")]
  #
  # description<-temp[2+which(temp=="description")]
  #
  # market_id<-unlist(strsplit(temp[1+which(temp=="market_id")],"}")[])
  # market_id<-as.numeric(substr(market_id,2,nchar(market_id)))

  return(data.frame(market_code,description,market_id))
}
