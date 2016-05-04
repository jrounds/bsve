
#' bsveGET
#'
#' @param token from hashing api_key, secret_key, email
#' @param URL bsve URL
#' @param ... arguments to GET
#'
#' @return
#' @export
bsveGET = function(token, URL, ...){
	GET(URL, add_headers("harbinger-authentication" = token), ...)	
}


getMaxTop = function(token , dataSourceType="RSS", source ="CDC MMWR Reports", filter=NULL){
	#getMaxTop induces an error to see how large top can be and then parses the error. 
	#Find me a better way within BSVE?  
	res = unlist(try({get_bsve(token = token, dataSourceType=dataSourceType, source=source, filter=filter,top = 10^9)},silent=TRUE))
	#x <<- x
	#try({ res = x$errors[[1]]$errorMessage})
	#res <<- res;
	res = unlist(strsplit(res, " "))
	res = tail(res,1)
	as.integer(res)
	as.integer(tail(unlist(strsplit(unlist(res), " ")),1))
	
	
}

getAll = function(token , dataSourceType="RSS", source ="CDC MMWR Reports", filter= NULL, top = NULL){
	if(is.null(top))
		top =getMaxTop(token, dataSourceType, source,filter)
	system.time({
		x = get_bsve(token = token, dataSourceType=dataSourceType, source=source,filter=filter, top)
	})
	x
	
	
	
}

#to slow to be useful...
getAllIterative = function(token, dataSourceType="RSS",  source ="CDC MMWR Reports" , filter= NULL){

	system.time({
		res = list()
		skip = 0
		nrecs = getMaxTop(token, dataSourceType, source, filter)
		s = seq(0, nrecs, by =1000)
		x = lapply(s, function(skip) get_bsve(token = token, dataSourceType=dataSourceType, source=source,filter=filter, skip=skip))
	})
	x 
}
if(FALSE){
	dataSourceType = "SODA"
	source = "Babesiosis to Campylobacteriosis"
	system.time({
		x2 = getAllIterative(token, dataSourceType, source=source)
	})
	system.time({
		x1 = getAll(token, dataSourceType, source=source)
	})
}

#' dataSources
#'
#' @param token 
#'
#' @return vector of dataSources in BSVE
#' @export
#'
#' @examples
#' token = bsve_sha1(APIKEY, SECRETKEY, EMAIL)
#' 	sources = dataSources(token)
#'	url1 <- "http://search.bsvecosystem.net/api/data/query/RSS"
#'	r1 <- bsveGET(token,url1, query = list(`$source` = "CDC MMWR Quick Stats",  `$filter`="") )
#'	http_status(r1)
#'	r1 = startQuery(token)
#'	http_status(r1)
#'	r2 = getResult(token, r1)
#'	http_status(r2)
#'	x2 = content(r2)
#'	sapply(x2$result[[1]]$hits$hit, function(y) y$data$title[[1]])
dataSources = function(token){
	URL <- "http://search.bsvecosystem.net/api/data/list"
	r = bsveGET(token, URL)
	http_status(r)
	x = content(r)
	#b1 <- rbindlist( lapply( x$result, function(y) y[1:5] ))
	#b1$fields <- sapply( x$result, function(y) paste( sapply(y$fields, "[[", "name"), collapse=";"))
	 ret = lapply( x$result, function(y){
	 	y <<- y
	 	sources = "NA_ZERO_LENGTH_"
	 	if(length(y$dataSources) > 0)
	 		sources = unlist(lapply(y$dataSources, "[[", "name"))
	 	data.frame(name = y$name, sources = sources, stringsAsFactors = FALSE)
	 		
	 	})
	 return(do.call(rbind, ret))

}

# extract RSS data 
extract.RSS = function(r){
	r <<- r
	if(is.null(r) || !inherits(r, "list") || is.null(r$query) || is.null(r$query$type) || r$query$type != "RSS")
		return(NULL)
	res = r$result
	hits = res[[1]]$hits
	if(hits$found == 0)
		return(NULL)
	h = hits$hit[[1]]
	
	hdata= lapply(hits$hit,  function(h) {
		h <<- h
		d = h$data
		d = lapply(d, unlist)
		is_null = sapply(d, is.null)
		d[is_null] = "NA"
		d
	})
	hdata = rbindlist(hdata)
	hdata$source = r$query$source
	hdata
}
extract.SODA = function(r){
	r <<- r
	if(is.null(r) || !inherits(r, "list") ||is.null(r$query) || is.null(r$query$type) || r$query$type != "SODA")
		return(NULL)
	res = r$result
	json = res[[1]]
	data = lapply(res, function(json){
		j0 = jsonlite::fromJSON(json)
		j0 = as.list(unlist(j0))
		wide = unlist(lapply(c("mmwr_", "location_", "reporting_area", "country_", "seriescode", "series_"),
			grep, names(j0), value=TRUE))
		long = setdiff(names(j0), wide)
		names = unique(unlist(lapply(data, names)))
		wide_var = j0[wide]
		long_var =unlist(j0[long])
		long_var = data.frame(variable = names(long_var), value= long_var)
		long_var$value[long_var$value == "-"] =NA
		long_var$value = as.numeric(long_var$value)
		for(n in names(wide_var))
			long_var[[n]] = wide_var[[n]]
		long_var
	})

	data2 = rbindlist(data, use.names=TRUE, fill=TRUE)
	data2$source = r$query$sources
	data2
	
}

#' Start  A Query
#'
#' @param token 
#' @param source a dataSource (one of those from dataSources(token))
#' @param filter filter for query
#'
#' @return a query appropriate for input into getQuery()
#' @export
#'
#' @examples
#' 	sources = dataSources(token)
#'	url1 <- "http://search.bsvecosystem.net/api/data/query/RSS"
#'	r1 <- bsveGET(token,url1, query = list(`$source` = "CDC MMWR Quick Stats",  `$filter`="") )
#'	http_status(r1)
#'	r1 = startQuery(token)
#'	http_status(r1)
#'	r2 = getResult(token, r1)
#'	http_status(r2)
#'	x2 = content(r2)
#'	sapply(x2$result[[1]]$hits$hit, function(y) y$data$title[[1]])
startQuery = function(token, source= "CDC MMWR Quick Stats", filter=""){
	url1 <- "http://search.bsvecosystem.net/api/data/query/RSS"
	r1 <- bsveGET(token,url1, query = list(`$source` = source,  `$filter`=filter) )
	r1
}

#' Get Results From  a Query
#'
#' Warnng currently gets everything
#'
#' @param token 
#' @param query return from startQuery
#'
#' @return a list of query results
#' @export
#'
#' @examples
#' 	sources = dataSources(token)
#'	url1 <- "http://search.bsvecosystem.net/api/data/query/RSS"
#'	r1 <- bsveGET(token,url1, query = list(`$source` = "CDC MMWR Quick Stats",  `$filter`="") )
#'	http_status(r1)
#'	r1 = startQuery(token)
#'	http_status(r1)
#'	r2 = getResult(token, r1)
#'	http_status(r2)
#'	x2 = content(r2)
#'	sapply(x2$result[[1]]$hits$hit, function(y) y$data$title[[1]])
getResult = function(token, query){
	url2 <- "http://search.bsvecosystem.net/api/data/result/"
	r2 <-  bsveGET(token, paste0(url2, content(query)$requestId))
	r2

}