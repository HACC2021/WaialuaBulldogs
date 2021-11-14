#-----	'DOT'
#-----	TXG_FOB_USD
#-----		External Trade, Goods, Value of Exports, Free on Board (FOB), US Dollars
#-----	TMG_CIF_USD
#-----		External Trade, Goods, Value of Imports, Cost, Insurance, Freight (CIF), US Dollars

thing <- DataStructureMethod(databaseID)
options(warn=-1)
options(messages=-1)
databaseID	<-'DOT'
startdate	<-'1990-01-01'
enddate		<-'2019-12-01'
this_dir 		<- 'C:/therocrates/IMF_DATA/'

for(x in 1:length(iso2s)){ 
		iso2		<- iso2s[x]
		iso3		<- iso3s[x]
		this_country	<- these_countries[x]
cat(x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")

# TERMS OF TRADE - TOT - Monthly
		these_indicators	<- c('TXG_FOB_USD','TMG_CIF_USD')
		k_names				<- c('EXP_FOB','IMP_CIF')
			for(y in 1:length(these_indicators)) {
				fname <- paste(this_dir,k_names[y],'_M_',iso3,'.txt',sep="")
				queryfilter <- list(CL_FREQ="M", CL_AREA_DOT=iso2, CL_INDICATOR_DOT=these_indicators[y],CL_COUNTERPART_AREA_DOT='W00')
				result <- try(
					suppressMessages(TOT <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
				)
				if(class(result) == "try-error" || is.null(result)) {
					cat("NO ",fname,":     x=",x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
				} else if(!is.null(TOT)) {
					res <- data.frame(TOT$Obs[1])
					names(res) <- c('year_mo',k_names[y])
					write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
				}

				file_check <- dir(this_dir,paste(k_names[y],'_M_',iso3,'.txt',sep=""))
				if(length(file_check)==0){
			# TERMS OF TRADE - Quarterly
					fname <- paste(this_dir,k_names[y],'_Q_',iso3,'.txt',sep="")
						queryfilter <- list(CL_FREQ="Q", CL_AREA_DOT=iso2, CL_INDICATOR_DOT=these_indicators[y],CL_COUNTERPART_AREA_DOT='W00')
						result <- try(
							suppressMessages(TOT <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
						)
						if(class(result) == "try-error" || is.null(result)) {
							cat("NO ",fname,":     x=",x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
						} else if(!is.null(TOT)) {
							res <- data.frame(TOT$Obs[1])
							names(res) <- c('year_qtr',k_names[y])
							write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
						}
				}
			}
if(x%%25==0){Sys.sleep(1)}
}
