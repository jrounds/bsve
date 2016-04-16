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
v = ret[[8]]
names = lapply(ret, function(v) {
	r = list()
	r$name = "NULL"
	r$type = "NULL"
	r$source = "NULL"
	try({
		r$type = v$query$type
		r$source = v$query$sources
		r$name = paste(v$query$type, v$query$sources)	
	})
	r
})
source = sapply(names, "[[", "source")


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
RSS0 = sample(1:nrow(RSS), 10)
RSS0 = RSS[RSS0]
print(xtable(RSS0), "html")
paste(colnames(RSS), collapse=", ")

#---------------------------------------------------------------------------------
# Extract SODA DATA
#---------------------------------------------------------------------------------
SODA = lapply(ret, extract.SODA)
fout = file.path(package_root, "data", "SODA.rds")
saveRDS(SODA, fout)
SODA[[89]]
#SODA seems to have two types
# Type1: weekly disease
# Type2: development indicators
type1 = unlist(lapply(SODA, function(v) length(grep("mmwr", colnames(v))) > 0))
type2 = unlist(lapply(SODA, function(v) length(grep("seriescode", colnames(v))) > 0))
SODA_1 = rbindlist(SODA[type1])
table(SODA_1$mmwr_year)
SODA_2 = rbindlist(SODA[type2])





#---------------------------------------------------------------------------------
# A series of "ONE OFF" examinations to understand some issues in the data
#---------------------------------------------------------------------------------
	setkeyv(SODA_1, c("reporting_area", "source"))
	unique(subset(SODA_1, reporting_area == "Worcester, MA")$source)
	unique(subset(SODA_1, reporting_area == "W.S. CENTRAL")$source)
	unique(subset(SODA_1, reporting_area == "W.S. Central")$source)
	subset(SODA_1, source=="Human Development Index")
	subset(SODA_1, source =="Hepatitis viral and acute")
	filter = "reporting_area eq KENTUCKY and mmwr_year eq 2016"
	unique(SODA_1)
	setkey(SODA_1, variable)
	subset(SODA_1, variable == "spotted_fever_rickettsiosis_including_rmsf_confirmed_current_week" & reporting_area == "KENTUCKY")
	hmm = lapply(1:14, function(i) {
		f = paste(filter, "and mmwr_week eq ", i) 
		token = bsve_sha1(APIKEY, SECRETKEY, EMAIL)  #SECRETKEY IS R SOURCED

		hmm = get_bsve(token, "SODA", "Spotted Fever Rickettsiosis to Syphilis", filter = f)
		#extract.SODA(hmm)
		hmm
	})
	hmm4 = lapply(hmm, extract.SODA)
	hmm3 = lapply(hmm4, subset,  variable == "syphilis_primary_and_secondary_current_week")
	hmm2 = rbindlist(hmm3)
	setkeyv(hmm2, colnames(hmm2))
	hmm2 = unique(hmm2)
	setorder(hmm2, "mmwr_week")
	hmm5 = subset(hmm2, select=c("variable", "value", "mmwr_year", "mmwr_week", "reporting_area", "source"))
	library(xtable)
	print(xtable(hmm5), type="html")
	hmm0 = subset(hmm2,  variable == "syphilis_primary_and_secondary_current_week")
	total = unlist(lapply(SODA_AGGREGATIONS, function(a) length(unique(subset(SODA_1, reporting_area == a)$variable))))
	names(total) = SODA_AGGREGATIONS
	subset(SODA_1, reporting_area == "Ogden, UT")
	
	
	hmm =  get_bsve(token, "SODA", "Spotted Fever Rickettsiosis to Syphilis", filter = f)

		SODA_SERIES = unique(SODA_1$variable)
	SODA_AGGREGATIONS = unique(SODA_1$reporting_area)
#some info about diseases
which = grep("_cum_2016$", SODA_1$variable, value=TRUE) 
u = unique(which)
u = gsub("_cum_2016", "", u)
#


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






