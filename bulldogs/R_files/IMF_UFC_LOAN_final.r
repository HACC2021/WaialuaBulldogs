#-----	IFS
#-----	HPU_USD
#-----	Fund Accounts, UFC & Loans, US Dollars

options(warn=-1)
options(messages=-1)
databaseID	<-'IFS'
startdate	<-'1990-01-01'
enddate		<-'2019-12-01'
this_dir 		<- 'C:/therocrates/IMF_DATA/'

for(x in 1:length(iso2s)){ 
		iso2		<- iso2s[x]
		iso3		<- iso3s[x]
		this_country	<- these_countries[x]
cat(x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")

		this_indicator	<- 'HPU_USD'
		fname <- paste(this_dir,'IMF_UFC_LOAN_M_',iso3,'.txt',sep="")
		queryfilter <- list(CL_FREA="M", CL_AREA_IFS=iso2, CL_INDICATOR_IFS =c(this_indicator))

			result <- try(
				suppressMessages(UFC_LOAN <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
			)
			if(class(result) == "try-error" || is.null(result)) {
				cat("NO ",fname,":     x=",x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
			} else if(!is.null(UFC_LOAN)) {
				res <- data.frame(UFC_LOAN$Obs[1])
				names(res) <- c('yearmo','UFC_LOAN')
				write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
			}

			file_check <- dir(this_dir,paste('IMF_UFC_LOAN_M_',iso3,'.txt',sep=""))
			if(length(file_check)==0){
				fname <- paste(this_dir,'IMF_UFC_LOAN_Q_',iso3,'.txt',sep="")
				queryfilter <- list(CL_FREA="Q", CL_AREA_IFS=iso2, CL_INDICATOR_IFS =c(this_indicator))

				result <- try(
					suppressMessages(UFC_LOAN <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
				)
				if(class(result) == "try-error" || is.null(result)) {
					cat("NO ",fname,":     x=",x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
				} else if(!is.null(UFC_LOAN)) {
					res <- data.frame(UFC_LOAN$Obs[1])
					names(res) <- c('year_qtr','UFC_LOAN')
					write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
				}
			}
if(x%%25==0){Sys.sleep(1)}
}
