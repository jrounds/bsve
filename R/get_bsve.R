get_bsve_retry = function(url, token, query=NULL, required_status = 0, sleep.time = 10, ntry=3, verbose=TRUE){
	 for(i in 1:ntry){
   		if(verbose)
  			message("Attempt ", i)
	 	x1 = NULL
		try({
			if(is.null(query))
				r2 <-  GET(url, add_headers("harbinger-authentication" = token))
			if(!is.null(query))
				r2 <-  GET(url, add_headers("harbinger-authentication" = token), query= query)
		 #  	r2 <<- r2;
		  	x1=  content(r2)
		  	if(x1$status == required_status)
	  			return(x1)  #only good way out
		})
	 	if(!is.null(x1) && x1$status == -1)
	 		stop(x1$errors[[1]]$errorMessage)
	 	#x1 <<- x1
	 	Sys.sleep(sleep.time)
	 }
	if(is.null(x1)){
	 	stop("GET content after all attempts is null")	
	}
	#if we are here we didnt get out with a status == 1
	if(x1$status == -1)
		   stop(x1$errors[[1]]$errorMessage)
	if(x1$status != required_status)
		stop("required_statuts not met through time allotted")
}
get_bsve <- function(token , dataSourceType="RSS", source ="CDC MMWR Reports",  filter , orderby, top, skip,sleep.time = 10, ntry=3, verbose=TRUE){

   url1 <- paste0("http://search.bsvecosystem.net/api/data/query/", dataSourceType)

   # require filter 
   q1 <-  list(`$source` = source )
   if(!missing(filter) && !is.null(filter))   q1 <- c( q1, list(`$filter` = filter))
   if(!missing(orderby) && !is.null(orderby))  q1 <- c( q1, list(`$orderby` = orderby))
   if(!missing(top) && !is.null(top))      q1 <- c( q1, list(`$top` = as.integer(top)))
   if(!missing(skip) && !is.null(skip))     q1 <- c( q1, list(`$skip` = as.integer(skip)))
#print(q1)    
	if(verbose)
		message("Preparing BSVE query")
   x1 <- get_bsve_retry(url1, token,  query = q1, required_status = 0, sleep.time=sleep.time, ntry=ntry, verbose=verbose )
   Sys.sleep(1)
   url2 <- "http://search.bsvecosystem.net/api/data/result/"
   if(verbose)
   		message("Getting results")
	x =  get_bsve_retry(paste0(url2, x1$requestId), token, required_status = 1, sleep.time=sleep.time, ntry=ntry, verbose=verbose )
   x
  

}
 
