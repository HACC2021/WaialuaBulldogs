options(warn=-1)
options(messages=-1)
databaseID	<-'FM'
startdate	<-'1990-01-01'
enddate		<-'2019-12-01'
this_dir 		<- 'C:/therocrates/IMF_DATA/'

for(x in 1:length(iso2s)){ 
#x <- 29
		iso2		<- iso2s[x]
		iso3		<- iso3s[x]
		this_country	<- these_countries[x]
cat(x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")

#GROSS DEBT % OF GDP
		this_indicator	<- 'G_XWDG_G01_GDP_PT'
		fname <- paste(this_dir,'IMF_DEBT_GDP_',iso3,'.txt',sep="")
		queryfilter <- list(CL_FREQ="A", CL_AREA_FM=iso2, CL_INDICATOR_FM=this_indicator)
			result <- try(
				suppressMessages(DEBT_GDP <- CompactDataMethod(databaseID,queryfilter,startdate,enddate,FALSE,FALSE,FALSE))
			)
			if(class(result) == "try-error" || is.null(result)) {
				cat("NO ",fname,":     x=",x,"_",this_country,"_",iso2,"_",iso3,"\n",sep="")
			} else if(!is.null(DEBT_GDP)) {
				res <- data.frame(DEBT_GDP$Obs[1])
				names(res) <- c('year_mo','DEBT_GDP')
				write.table(res,fname,append=F,quote=F,sep='|',na='',row.names=F,col.names=F,fileEncoding='UTF-8')
			}
if(x%%25==0){Sys.sleep(1)}
}
