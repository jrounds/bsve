`bsve` is an `R` package for developers to access the
[BSVE](http://developer.bsvecosystem.net) [API](http://developer.bsvecosystem.net/wp/tutorials/bsve-data-api)


###Installation

The package requires a recent version of `openssl` to create the hash
key, `httr` and `plyr` packages 

```
library(devtools)
install_github("cstubben/bsve")
```

###Connecting

You will need to get the API and secret key from the developer site
under My Account -> Manage API Credentials and add them below.

```
api_key <- "AKcfef08f1-c852-43b9-bce7-8923880e3b68"
secret_key <-  "#replace with SECRET key"
```

The `bsve_sha1` function creates the authentication header using the
two keys and a valid email.

```
email <-  "#replace with your@email"
token <- bsve_sha1(api_key, secret_key, email)
token
[1] "apikey=AKcfef08f1-c852-43b9-bce7-8923880e3b68;timestamp=1457989545992;nonce=535514;signature=f6b90ed483b37..."
```


###4.4 Find All DataSources

If you are not familiar with `httr`, be sure to check the [quickstart
guide](https://github.com/hadley/httr/blob/master/vignettes/quickstart.Rmd).

I may add functions to simplify some queries, but for now I will
go through each step and include the URL  (`/api/data/list`)  and  authentication header to `GET`.

```
url <- "http://search.bsvecosystem.net/api/data/list"
r <- GET(url, add_headers("harbinger-authentication" = token))
r
Response [http://search.bsvecosystem.net/api/data/list]
  Date: 2016-03-16 21:44
  Status: 200
  Content-Type: application/json;charset=UTF-8
  Size: 72.1 kB
http_status(r)
$category
[1] "success"
```

Parse the results into a nested list (`content` uses `fromJSON` in
`jsonlite` by default).
```
x <- content(r)

sapply(x, length)
   status   message requestId    result    errors 
        1         1         0        15         0 
names(x$result[[1]])
[1] "name"        "description" "fileTypes"   "label"       "shortLabel"  "fields"     
[7] "dataSources"
```

Both `fields` and `dataSources` are arrays with 0 to many elements, so just grab the first 5 keys and count the number of fields
and dataSources below.

```
b1 <- ldply(lapply( x$result, function(y) y[1:5] ), "data.frame",
stringsAsFactors=FALSE)
b1$fields <- sapply( x$result, function(y) length( y$fields) )
b1$dataSources <- sapply( x$result, function(y) length( y$dataSources) )

b1[, c(1:2,6:7)]
              name                             description fields dataSources
1           AFCENT                            Clinic Visit     39           0
2   Demo Data Type Testing addition of data to repository.      2           0
3            EIDSS                         EIDSS Flat File     22           0
4  HydraSourceType          Test dataSource type for hydra      4           2
5        HydraType          Test dataSource type for hydra      4           1
6        Leidos Wx          Weather for use by Leidos apps      2           0
7         LeidosWx          Weather for use by Leidos apps      2           0
8              NDD                           NDD Flat File     20           0
9              PON                           PON Flat File     31           1
10             RSS                                RSS Feed     18          65
11    RSS_FLATFILE                           RSS Flat File      0           0
12              SD                     Syndromic Flat File     15           0
13            SODA                          SODA Flat File      0          15
14         TWITTER                       Twitter Flat File      0           1
15       WEBSEARCH                              Web Search      0           0
```


###Notes on parsing lists with NULLs

If you want to list fields or dataSources, `ldply` will return an error if [NULLs are present](http://stackoverflow.com/questions/15793759/convert-r-list-to-dataframe-with-missing-null-elements). I have included a few options below to avoid NULLs and parse the RSS
fields in element 10.

OPTION 1. Remove nulls before combining

If you don't know where NULLs are found, you can remove them
using `Filter`

```
y <- lapply(x$result[[10]]$fields, Filter, f = Negate(is.null))
ldply(y, "data.frame")
        name    type
1      cases  String
2     deaths  String
3       when  String
4       link  String
5      where  String
6  longitude   Float
7  simulated Boolean
8   latitude   Float
9         id  String
10    source  String
...
```

OPTION 2.  Skip NULL fields

When you figure out where NULLs are located, you can skip those keys.
```
ldply( lapply(x$result[[10]]$fields, "[",  1:2), "data.frame")
```

OPTION 3. USE `do.call`

There are two problems with `do.call`.  First, if a tag if missing, I think it
 will fill the row by silenting repeating values.  Second, while the table
 looks correct, each column is actually a
 [list](https://stat.ethz.ch/pipermail/r-help//2012-November/340399.html)
 
```
as.data.frame(do.call("rbind", x$result[[10]]$fields ))
    name   type format description
1  cases String   NULL        NULL
2 deaths String   NULL        NULL
3   when String   NULL        NULL
...
```

OPTION 4.   Use RJSONIO and replace NULLs with NAs

The `RJSONIO` package has a `nullValue` option that lets you replace NULLs with NAs.
This returns a different nested list than `jsonlite` with named vectors
instead of lists, so use rbind.

```
x1 <- RJSONIO::fromJSON(content(r, "text"), nullValue=NA)
str(x1$result[[10]]$fields[[1]])
       name        type      format description 
    "cases"    "String"          NA          NA 
ldply( x1$result[[10]]$fields, "rbind")  
    name   type format description
1  cases String   <NA>        <NA>
2 deaths String   <NA>        <NA>
3   when String   <NA>        <NA>
```


You can list the 65 RSS dataSources below.  Only the first 3 keys have
non-NULL values and fields is an empty list except in SODA, which has
an array with 4 elements like the result fields above. 

```
names(x$result[[10]]$dataSources[[1]])
[1] "name"        "category"    "description" "feedType"    "selected"    "status"     
[7] "fields"     

ldply( lapply(x$result[[10]]$dataSources, "[",  1:2), "data.frame")
                                           name        category
1                            Agriculture Canada   Expert Domain
2         Agrifeeds Animal Diseases and Control   Expert Domain
3 AgriFeeds News on Phytosanitary Measures IPPC   Expert Domain
4 AgriFeeds News on Plant Pathology and Disease   Expert Domain
5                        Agrifeeds Pest Control   Expert Domain
6                           AP Top Science News Non-Domain News
...
```

###4.6 Querying the Datasource API

Use `api/data/query/{data}` to query RSS or other datasets.  I think `$filter` is required, but I'm not familiar with all the query options, so please send me examples or post them to [issues](https://github.com/cstubben/bsve/issues/1).

```
url1 <- "http://search.bsvecosystem.net/api/data/query/RSS"

r1 <- GET(url1,  add_headers("harbinger-authentication" = token),
  query = list(`$source` = "CDC MMWR Reports",  `$filter`="pubDate ge 2016-02-01") )

x1 <- content(r1)
x1[!sapply(x1, is.null)]
$status
[1] 0

$message
[1] "In Progress"

$requestId
[1] "abfe96a8-1ce8-4336-8abb-4ce89550e204"

$query
$query$type
[1] "RSS"

$query$sources
[1] "CDC MMWR Reports"

$query$filter
[1] "pubDate ge 2016-02-01"
...
```


###4.7 Getting Datasource Results

You need the `requestId` from the results above to download the
results.  I have not looked at this carefully, but I did figure out
how to find titles (and these nested lists are way too complicated)

```
url2 <- "http://search.bsvecosystem.net/api/data/result/"
r2 <-  GET( paste0(url2, x1$requestId), add_headers("harbinger-authentication" = token))
x2 <- content(r2)

sapply(x2, length)

names(x2$result[[1]]$hits)
[1] "found"                "start"                "hit"                 
[4] "additionalProperties"

sapply(x2$result[[1]]$hits$hit, function(y) y$data$title[[1]])
[1] "EARLY RELEASE: Vital Signs: Preventing Antibiotic-Resistant Infections in Hospitals - United States, 2014" 
[2] "SUPPLEMENTS: Development of the Community Health Improvement Navigator Database of Interventions" 
[3] "RECOMMENDATIONS AND REPORTS: CDC Guideline for Prescribing Opioids for Chronic Pain - United States, 2016" 
[4] "EARLY RELEASE: Transmission of Zika Virus Through Sexual Contact with Travelers to Areas of Ongoing Transmission - Continental United States, 2016"
...
```
