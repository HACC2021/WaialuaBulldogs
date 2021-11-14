#Public Sector, Total, Liabilities, by instrument: Debt securities
#-----	PSBSFAD	PS|XDCG63F3W0|S1
#-----	http://dataservices.imf.org/REST/SDMX_JSON.svc/DataStructure/PSBSFAD
#-----	http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/PSBSFAD/A.156.PS|XDCG63F3W0|S1?startPeriod=1990-01-01&endPeriod=2019-12-01

options(warn=-1)
options(messages=-1)
databaseID	<-'PSBSFAD'
thing <- DataStructureMethod(databaseID)
startdate	<-'1990-01-01'
enddate		<-'2019-12-01'
this_dir 		<- 'C:/therocrates/IMF_DATA/'

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
write.table(thesecountries,file='C:/therocrates/these_countries_IMF_PUB_DEBT.txt',row.names=F,col.names=F,append=F)

for(x in 1:nrow(thesecountries)) {
	this_indicator	<- 'PS|XDCG63F3W0|S1'
		iso2			<- thesecountries$iso2[x]
		iso3			<- thesecountries$iso3[x]
		this_country	<- thesecountries$country[x]
		imf_countrycode	<- thesecountries$CodeValue[x]
cat(x,this_country,iso2,iso3,imf_countrycode,this_indicator,"\n",sep="_")

		fname <- paste(this_dir,'IMF_PUB_DEBT_',iso3,'.txt',sep="")
		queryfilter <- list('CL_FREQ'='A',"CL_PSBS Country_PSBSFAD"=imf_countrycode, "CL_FAD Input Indicators_PSBSFAD"=this_indicator)
			result <- try(
				suppressMessages(PUB_DEBT <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
			)
			if(class(result) == "try-error" || is.null(result)) {
				cat("NO ",fname,":     x=",x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
			} else if(!is.null(PUB_DEBT)) {
				res <- data.frame(PUB_DEBT$Obs[1])
				names(res) <- c('year_mo','PUB_DEBT')
				write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
			}
if(x%%25==0){Sys.sleep(1)}
}

