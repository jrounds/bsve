get_bsve <- function(token , dataSourceType="RSS", source ="CDC MMWR Reports",  filter="pubdate ge 2016-02-01" , orderby, top, skip){

   url1 <- paste0("http://search.bsvecosystem.net/api/data/query/", dataSourceType)

   # require filter 
   q1 <-  list(`$source` = source )
    if(!missing(filter) )  q1 <- c( q1, list(`$filter` = filter))
   if(!missing(orderby) )  q1 <- c( q1, list(`$orderby` = orderby))
   if(!missing(top) )      q1 <- c( q1, list(`$top` = top))
    if(!missing(skip) )    q1 <- c( q1, list(`$skip` = skip))
#print(q1)    
   r1 <- GET(url1,  add_headers("harbinger-authentication" = token), query = q1 )
   x1 <- content(r1)
   Sys.sleep(1)
   url2 <- "http://search.bsvecosystem.net/api/data/result/"
   r2 <-  GET( paste0(url2, x1$requestId), add_headers("harbinger-authentication" = token))
   content(r2)

}
 
