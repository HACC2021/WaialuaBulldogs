https://worldbank.github.io/debt-data/api-guide/ids-api-guide-r-1.html
http://datahelp.imf.org/knowledgebase/articles/667681-using-json-restful-web-service
http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/IFS/Q.BR.LUR_PT?startPeriod=1990-01-01&endPeriod=2019-12-01

BOP	ILOLNG_BP6_USD	Liabilities, Other Investment, Loans, General Government, US Dollars

#Gross debt (D4) at market value
#-----	GFSMAB	G63_FD4
#-----	http://dataservices.imf.org/REST/SDMX_JSON.svc/DataStructure/PSBSFAD
#-----	http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/PSBSFAD/A.156.NGDP?startPeriod=1990-01-01&endPeriod=2019-12-01


PGI	D_L_USD	External Debt, Long-term, US Dollars


options(warn=-1)
options(messages=-1)
this_dir 	<- 'C:/therocrates/IMF_DATA/'

countries_tmp <- data.frame(matrix(ncol=3, nrow=length(iso2s)))
	colnames(countries_tmp) <- c('iso2','iso3','country')
		countries_tmp$iso2 = iso2s
		countries_tmp$iso3 = iso3s
		countries_tmp$country = these_countries
	countries_tmp <- countries_tmp[countries_tmp$iso2!='UM',]
	countries_tmp <- countries_tmp[countries_tmp$iso2!='AN',]
		countries_tmp$matcher = gsub(' ','',tolower(substring(countries_tmp[,3],0,10)))

thesecountries <- data.frame(matrix(ncol=3, nrow=length(thing[2][[1]][[1]])))
	colnames(thesecountries) <- c('CodeValue','CodeText','matcher')
		thesecountries$CodeValue <- thing[2][[1]][[1]]
		thesecountries$CodeText <- thing[2][[1]][[2]]
		thesecountries$matcher <- gsub(' ','',tolower(substring(thesecountries$CodeText,0,10)))
	thesecountries <- thesecountries[thesecountries$CodeValue!='353',]

thesecountries <- merge(x=countries_tmp,y=thesecountries,by='matcher')
#write.table(thesecountries,file='C:/therocrates/these_countries_IMF_PUB_DEBT.txt',row.names=F,col.names=F,append=F)


##########################################################
#	PGI	D_L_USD	External Debt, Long-term, US Dollars

this_dir 	<- 'C:/therocrates/IMF_DATA/'
databaseID	<-'PGI'
thing <- DataStructureMethod(databaseID)
startdate	<-'1990-01-01'
enddate		<-'2019-12-01'

this_indicator	<- 'D_L_USD'
this_country	<- '1C_ALLC'

fname <- paste(this_dir,'IMF_EXDEBT_ALL_COUNTRIES_M.txt',sep="")
#MONTHLY
	queryfilter <- list('CL_FREQ'='M','CL_AREA_PGI'=this_country, 'CL_INDICATOR_PGI'=this_indicator)
	result <- try(
		suppressMessages(EXDEBT <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
	)
	if(class(result) == "try-error" || is.null(result)) {
		cat("\t\tNO ",fname,"\n",sep="")
	} else if(!is.null(EXDEBT)) {
		res <- data.frame(EXDEBT$Obs[1])
			names(res) <- c('yearmo','EXDEBT')
		write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
	}
			
#QUARTERLY
fname <- paste(this_dir,'IMF_EXDEBT_ALL_COUNTRIES_Q.txt',sep="")
	queryfilter <- list('CL_FREQ'='Q','CL_AREA_PGI'=this_country, 'CL_INDICATOR_PGI'=this_indicator)
	result <- try(
		suppressMessages(EXDEBT <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
	)
	if(class(result) == "try-error" || is.null(result)) {
		cat("\t\tNO ",fname,"\n",sep="")
	} else if(!is.null(EXDEBT)) {
		res <- data.frame(EXDEBT$Obs[1])
			names(res) <- c('yearQ','EXDEBT')
		write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
	}

#ANNUAL
fname <- paste(this_dir,'IMF_EXDEBT_ALL_COUNTRIES_A.txt',sep="")
	queryfilter <- list('CL_FREQ'='A','CL_AREA_PGI'=this_country, 'CL_INDICATOR_PGI'=this_indicator)
	result <- try(
		suppressMessages(EXDEBT <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
	)
	if(class(result) == "try-error" || is.null(result)) {
		cat("\t\tNO ",fname,"\n",sep="")
	} else if(!is.null(EXDEBT)) {
		res <- data.frame(EXDEBT$Obs[1])
			names(res) <- c('year','EXDEBT')
		write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
	}


###########################################################################
#-----	http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/IFS/Q.BR.LUR_PT?startPeriod=1990-01-01&endPeriod=2019-12-01
#-----	Request <- GET(url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/IFS/Q.BR.LUR_PT?startPeriod=1990-01-01&endPeriod=2019-12-01")

request <- GET(url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/IFS/Q.BR.LUR_PT?startPeriod=1990-01-01&endPeriod=2019-12-01")
response <- content(request, as = "text", encoding = "UTF-8")

#-----	Parse the JSON / convert to data frame
json_thing <- fromJSON(txt=response,simplifyVector=T,simplifyDataFrame=T,simplifyMatrix=T,flatten = TRUE)
res <- data.frame(matrix(ncol=2,nrow=nrow(json_thing[[1]]$DataSet$Series$Obs)))
	colnames(res) <- c('time_period','obs_value')
	res$time_period <- json_thing[[1]]$DataSet$Series$Obs[1]
	res$obs_value <- json_thing[[1]]$DataSet$Series$Obs[2]

res	

data.frame()
cols <- c("id","name")
json_thing[,cols]

# Print the data frame and review the source names and ids
cols <- c("id","name")
sourceJSON[,cols]



###########################################
try_api <- function(api_url='http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/IFS/Q.AU.LUR_PT?startPeriod=1990-01-01&endPeriod=2019-12-01 ') {
	counter = 0 
	while(counter < 10) {
		result <- try(
			request <- GET(url=api_url)
		)
		if(class(result) == "try-error" || is.null(result)) {
			cat("\tNO ",fname,":     x=",x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
			counter <- counter + 1
			Sys.sleep(.25)
		} else if(!is.null(request)) {
			result <- try(
				response <- content(request, as = "text", encoding = "UTF-8")
			)
			if(class(result) == "try-error" || is.null(result)) {
				cat("\t\tresponse is null or ERROR\n",sep="")
				counter <- counter + 1
			}else if(!is.null(response)) {
				if(length(grep(pattern='DOCTYPE HTML',x=response[[1]],ignore.case=T)) == 0) {
					json_thing <- fromJSON(txt=response,simplifyVector=T,simplifyDataFrame=T,simplifyMatrix=T,flatten=T)
					if(!is.null(json_thing[[1]]$DataSet$Series$Obs)) {
						res <- data.frame(matrix(ncol=2,nrow=length(json_thing[[1]]$DataSet$Series$Obs[[1]])))
							colnames(res) <- c('time_period','obs_value')
							res$time_period <- json_thing[[1]]$DataSet$Series$Obs[[1]]
							res$obs_value <- json_thing[[1]]$DataSet$Series$Obs[[2]]
						write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=T,fileEncoding='UTF-8')
						counter <- 10
					} else {
						cat("\t\tNO OBSERVATIONS or ERROR\n",sep="")
						counter <- 10
					}
				} else {
					counter <- counter + 1
					Sys.sleep(.25)
				}
			}
		} else {
			counter <- counter + 1
			Sys.sleep(.25)
		}
	}
}