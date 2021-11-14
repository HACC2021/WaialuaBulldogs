#-----	IFS
#-----	NGDP_R_XDC
#-----	National Accounts, Expenditure, Gross Domestic Product, Real, Domestic Currency
#-----	IFS
#-----	NGDP_R_PC_CP_A_PT
#-----	National Accounts, Expenditure, Gross Domestic Product, Real, Percentage change, corresponding period previous year, Percent

options(warn=-1)
options(messages=-1)
databaseID	<-'IFS'
thing <- DataStructureMethod(databaseID)
startdate	<-'1990-01-01'
enddate		<-'2019-12-01'
this_dir 		<- 'C:/therocrates/IMF_DATA/'

for(x in 1:length(iso2s)){ 
		iso2		<- iso2s[x]
		iso3		<- iso3s[x]
		this_country	<- these_countries[x]
cat(x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
#RGDP
		this_indicator	<- 'NGDP_R_XDC'
		fname <- paste(this_dir,'IMF_RGDP_Q_',iso3,'.txt',sep="")
		
		queryfilter <- list(CL_FREA="Q", CL_AREA_IFS=iso2, CL_INDICATOR_IFS =c(this_indicator))
			result <- try(
				suppressMessages(RGDP <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
			)
			if(class(result) == "try-error" || is.null(result)) {
				cat("NO ",fname,":     x=",x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
			} else if(!is.null(RGDP)) {
				res <- data.frame(RGDP$Obs[1])
				names(res) <- c('year_qtr','RGDP')
				write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
			}

#RGDP CHANGE
		this_indicator	<- 'NGDP_R_PC_CP_A_PT'
		fname <- paste(this_dir,'IMF_RGDPC_Q_',iso3,'.txt',sep="")
		queryfilter <- list(CL_FREA="Q", CL_AREA_IFS=iso2, CL_INDICATOR_IFS =c(this_indicator))

			result <- try(
				suppressMessages(RGDPC <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
			)
			if(class(result) == "try-error" || is.null(result)) {
				cat("NO ",fname,":     x=",x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
			} else if(!is.null(RGDPC)) {
				res <- data.frame(RGDPC$Obs[1])
				names(res) <- c('year_qtr','RGDPC')
				write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
			}
if(x%%20==0){Sys.sleep(1)}
}
