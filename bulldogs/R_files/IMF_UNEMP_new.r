#-----	IFS
#-----	LUR_PT
#-----	Labor Markets, Unemployment Rate, Percent
library(httr)
library(jsonlite)
library(tidyverse)
options(warn=-1)
options(messages=-1)

databaseID		<-	'IFS'
this_indicator	<-	'LUR_PT'
startdate		<-	'1990-01-01'
enddate			<-	'2019-12-01'

fname_base		<- 	'IMF_UNEMP'
this_dir 		<-	paste(mydir,'/IMF_DATA/',sep="")
urlbase <- 'http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/'

#-----	lets loop on countries
for(x in 1:length(iso2s)){ 
	iso2		<- iso2s[x]
	iso3		<- iso3s[x]
	this_country<- these_countries[x]
cat(x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")

#-----	QUARTERLY
	this_freq	<- 'Q'
	outfile <- paste(fname_base,'_',this_freq,'_',iso3,'.txt',sep="")
	fname <- paste(this_dir,outfile,sep="")	
	api_url <- paste(urlbase,databaseID,'/',this_freq,'.',iso2,'.',this_indicator,'?startPeriod=',startdate,'&endPeriod=',enddate,sep="")
cat("\t",api_url,"\n")

	try_api(api_url)

#-----	MONTHLY
	this_freq <- 'M'
	outfile <- paste(fname_base,'_',this_freq,'_',iso3,'.txt',sep="")
	fname <- paste(this_dir,outfile,sep="")	
	api_url <- paste(urlbase,databaseID,'/',this_freq,'.',iso2,'.',this_indicator,'?startPeriod=',startdate,'&endPeriod=',enddate,sep="")
cat("\t",api_url,"\n")

	try_api(api_url)

if(x%%25==0){Sys.sleep(1)}
}



