options(warn=-1)
options(messages=-1)
#	PGI	D_L_USD	External Debt, Long-term, US Dollars
this_dir 	<- 'C:/therocrates/IMF_DATA/'
databaseID	<-'PGI'
thing <- DataStructureMethod(databaseID)
startdate	<-'1990-01-01'
enddate		<-'2019-12-01'
this_indicator	<- 'D_L_USD'

for(x in 1:length(iso2s)){ 
		iso2		<- iso2s[x]
		iso3		<- iso3s[x]
		this_country	<- these_countries[x]
cat(x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")

#MONTHLY
fname <- paste(this_dir,'IMF_EXDEBT_M_',iso3,'.txt',sep="")

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
fname <- paste(this_dir,'IMF_EXDEBT_Q_',iso3,'.txt',sep="")

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
fname <- paste(this_dir,'IMF_EXDEBT_A_',iso3,'.txt',sep="")

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

if(x%%25==0){Sys.sleep(1)}
}

