databaseID	<-'IFS'
thing <- DataStructureMethod(databaseID)
startdate	<-'1990-01-01'
enddate		<-'2019-12-01'

for(x in 1:nrow(thesecountries)) {
#x<-28
	this_indicator	<- 'RAFA_MV_USD'
		iso2			<- thesecountries$iso2[x]
		iso3			<- thesecountries$iso3[x]
		this_country	<- thesecountries$country[x]
		imf_countrycode	<- thesecountries$CodeValue[x]
cat(x,this_country,iso2,iso3,imf_countrycode,this_indicator,"\n",sep="_")

		fname <- paste(this_dir,'IMF_RSRVT_M_',iso3,'.txt',sep="")
#MONTHLY
		queryfilter <- list('CL_FREQ'='M','CL_AREA_IFS'=iso2, 'CL_INDICATOR_IFS'=this_indicator)
			result <- try(
				suppressMessages(RSRVST <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
			)
			if(class(result) == "try-error" || is.null(result)) {
				cat("\t\tNO ",fname,":     x=",x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
			} else if(!is.null(RSRVST)) {
				res <- data.frame(RSRVST$Obs[1])
				names(res) <- c('yearmo','RSRVST')
				write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
			}
			
			file_check <- dir(this_dir,paste('IMF_RSRVT_M_',iso3,'.txt',sep=""))
			if(length(file_check)==0){
			
		#RSRVS - Quarterly
				fname <- paste(this_dir,'IMF_RSRVT_Q_',iso3,'.txt',sep="")
				queryfilter <- list('CL_FREQ'='Q','CL_AREA_IFS'=iso2, 'CL_INDICATOR_IFS'=this_indicator)

				result <- try(
					suppressMessages(RSRVT <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
				)
				if(class(result) == "try-error" || is.null(result)) {
					cat("NO ",fname,":     x=",x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
				} else if(!is.null(RSRVST)) {
					res <- data.frame(RSRVST$Obs[1])
					names(res) <- c('year_qtr','RSRVST')
					write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
				}
			}
if(x%%25==0){Sys.sleep(1)}
}

