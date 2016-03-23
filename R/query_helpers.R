
#' bsveGET
#'
#' @param conn list with elements list(api_key, secret_key, email) by name
#' @param URL bsve URL
#' @param ... arguments to GET
#'
#' @return
#' @export
bsveGET = function(conn, URL, ...){
	GET(URL, add_headers("harbinger-authentication" = do.call(bsve_sha1, conn)), ...)	
}


#' dataSources
#'
#' @param conn conn list with elements list(api_key, secret_key, email) by name
#'
#' @return vector of dataSources in BSVE
#' @export
#'
#' @examples
#' conn = list()
#' conn$api_key = APIKEY
#' conn$secret_key = SECRETKEY
#' conn$email = EMAIL
#' 	sources = dataSources(conn)
#'	url1 <- "http://search.bsvecosystem.net/api/data/query/RSS"
#'	r1 <- bsveGET(conn,url1, query = list(`$source` = "CDC MMWR Quick Stats",  `$filter`="") )
#'	http_status(r1)
#'	r1 = startQuery(conn)
#'	http_status(r1)
#'	r2 = getResult(conn, r1)
#'	http_status(r2)
#'	x2 = content(r2)
#'	sapply(x2$result[[1]]$hits$hit, function(y) y$data$title[[1]])
dataSources = function(conn){
	URL <- "http://search.bsvecosystem.net/api/data/list"
	r = bsveGET(conn, url)
	http_status(r)
	x = content(r)
	#b1 <- rbindlist( lapply( x$result, function(y) y[1:5] ))
	#b1$fields <- sapply( x$result, function(y) paste( sapply(y$fields, "[[", "name"), collapse=";"))
	 unlist(sapply( x$result, function(y)  sapply(y$dataSources, "[[", "name") ))

}

#' Start  A Query
#'
#' @param conn list with elements list(api_key, secret_key, email) by name
#' @param source a dataSource (one of those from dataSources(conn))
#' @param filter filter for query
#'
#' @return a query appropriate for input into getQuery()
#' @export
#'
#' @examples
#' conn = list()
#' conn$api_key = APIKEY
#' conn$secret_key = SECRETKEY
#' conn$email = EMAIL
#' 	sources = dataSources(conn)
#'	url1 <- "http://search.bsvecosystem.net/api/data/query/RSS"
#'	r1 <- bsveGET(conn,url1, query = list(`$source` = "CDC MMWR Quick Stats",  `$filter`="") )
#'	http_status(r1)
#'	r1 = startQuery(conn)
#'	http_status(r1)
#'	r2 = getResult(conn, r1)
#'	http_status(r2)
#'	x2 = content(r2)
#'	sapply(x2$result[[1]]$hits$hit, function(y) y$data$title[[1]])
startQuery = function(conn, source= "CDC MMWR Quick Stats", filter=""){
	url1 <- "http://search.bsvecosystem.net/api/data/query/RSS"
	r1 <- bsveGET(conn,url1, query = list(`$source` = source,  `$filter`=filter) )
	r1
}

#' Get Results From  a Query
#'
#' Warnng currently gets everything
#'
#' @param conn list with elements list(api_key, secret_key, email) by name
#' @param query return from startQuery
#'
#' @return a list of query results
#' @export
#'
#' @examples
#' #' conn = list()
#' conn$api_key = APIKEY
#' conn$secret_key = SECRETKEY
#' conn$email = EMAIL
#' 	sources = dataSources(conn)
#'	url1 <- "http://search.bsvecosystem.net/api/data/query/RSS"
#'	r1 <- bsveGET(conn,url1, query = list(`$source` = "CDC MMWR Quick Stats",  `$filter`="") )
#'	http_status(r1)
#'	r1 = startQuery(conn)
#'	http_status(r1)
#'	r2 = getResult(conn, r1)
#'	http_status(r2)
#'	x2 = content(r2)
#'	sapply(x2$result[[1]]$hits$hit, function(y) y$data$title[[1]])
getResult = function(conn, query){
	url2 <- "http://search.bsvecosystem.net/api/data/result/"
	r2 <-  bsveGET(conn, paste0(url2, content(query)$requestId))
	r2

}