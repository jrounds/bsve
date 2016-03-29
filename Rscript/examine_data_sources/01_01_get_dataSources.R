#---------------------------------------------------------------------------------
# Get data sources from BSVE
#---------------------------------------------------------------------------------
library(data.table)
library(jsonlite)
library(httr)
package_root_path = function (path = ".") 
{
  stopifnot(is.character(path))
  has_description <- function(path) {
    file.exists(file.path(path, "DESCRIPTION"))
  }
  path <- normalizePath(path, mustWork = FALSE)
  while (!has_description(path) && !devtools:::is_root(path)) {
    path <- dirname(path)
  }
  if (devtools:::is_root(path)) {
    NULL
  }
  else {
    path
  }
}

package_root = package_root_path()
lapply(list.files(file.path(package_root, "R"), full=TRUE), source)


APIKEY = "AK8120026b-2e93-433c-9da0-a9010fbfaa18"
EMAIL = "jeremiah.rounds@pnnl.gov"
token = bsve_sha1(APIKEY, SECRETKEY, EMAIL)  #SECRETKEY IS R SOURCED


dsources = dataSources(token)
#---------------------------------------------------------------------------------
# Process data sources in lapply
#---------------------------------------------------------------------------------
ret = lapply(1:nrow(dsources), function(i){
	i0 <<- i
	dst <<- dataSourceType <- dsources[i,1]
	s <<- source <- dsources[i,2]
	message(dst)
	message(s)
	if(source == "NA_ZERO_LENGTH_")
		return(NULL)
	get_bsve(token, dataSourceType, source)
})
is_null = sapply(ret, is.null)

str(ret, max.level = 3)

saveRDS(ret, file.path(package_root, "data", "working_data_dump.rds"))
ret  = readRDS(file.path(package_root, "data", "working_data_dump.rds"))


#---------------------------------------------------------------------------------
# Extraction section
#---------------------------------------------------------------------------------


#---------------------------------------------------------------------------------
# Extract RSS DATA
#---------------------------------------------------------------------------------
RSS = lapply(ret, extract.RSS) 
RSS = rbindlist(RSS, use.names = TRUE)

fout = file.path(package_root, "data", "RSS.rds")
saveRDS(RSS, fout)


#---------------------------------------------------------------------------------
# Extract SODA DATA
#---------------------------------------------------------------------------------
SODA = lapply(ret, extract.SODA)
fout = file.path(package_root, "data", "SODA.rds")
saveRDS(SODA, fout)
#SODA seems to have two types
# Type1: weekly disease
# Type2: development indicators
type1 = unlist(lapply(SODA, function(v) length(grep("mmwr", colnames(v))) > 0))
type2 = unlist(lapply(SODA, function(v) length(grep("seriescode", colnames(v))) > 0))
SODA_1 = rbindlist(SODA[type1])
table(SODA_1$mmwr_year)
SODA_2 = rbindlist(SODA[type2])


#some info about diseases
which = grep("_cum_2016$", SODA_1$variable, value=TRUE) 
u = unique(which)
u = gsub("_cum_2016", "", u)


#---------------------------------------------------------------------------------
# Some output
#---------------------------------------------------------------------------------
library(XLConnect)
wb <- loadWorkbook(file.path(package_root, "data", "bsve_data.xlsx"), create=TRUE)
createSheet(wb, t <- "dataSources")
writeWorksheet(wb, dsources, t)
saveWorkbook(wb)
createSheet(wb, t <- "example_RSS")
writeWorksheet(wb, head(as.data.frame(RSS), 500), t)
saveWorkbook(wb)
createSheet(wb, t <- "example_SODA")
writeWorksheet(wb, head(as.data.frame(SODA_1), 500), t)
saveWorkbook(wb)






