#########################################################################################
#########################################################################################
#	R utils for Kamakura Corporation written by Ted Spradlin -February 2013
#	1.  you will need to install R from this url
#		http://cran.r-project.org/  
#		Ted is running 64bit R on windows 7 Ultimate
#	2.  once R is installed you'll need to download and install the DBI and RODBC packages located here
#		http://cran.r-project.org/web/packages/available_packages_by_name.html
#		or use R's load package functionality from within an R session
#	3. create a sql server 2008 data source for windows that R can use: 
#		this link:     http://statistical-research.com/wp-content/uploads/2012/09/database-connect.pdf
#		offers an excellent set of instructions for setting up a data connection on windows 
#		R can use these connections as a local client
#		name your data sources 'FIJI' and 'OKINAWA' and use the credentials below 
#	4. execute the instructions above using FIJI and OKINAWA and credentials below 
#		and you will be able to connect by sourcing this document in an R sessions
#		source("c:/your_favorite_dir/kamakura_Rutils.r")
#	5. below are functions for querying the okinawa and fiji servers in R
#	6. lastly there are a few helpful functions for getting company identifiers, etc...
#########################################################################################
#########################################################################################
#	leave this at defaults and it will write to your 'My Documents' folder in windows os
.Library.site <- "C:/Program Files/R/R-3.6.1/library"
library(RODBC)
library(DBI)
library(VIF)
library(fmsb)
library(gvlma)
library(lmtest)
library(car)
library(sandwich)
library(fBasics)
library(Rmisc)
library(quantmod)
library(XBRL)

Sys.setenv(R_HISTSIZE=999999)
.First <- function() {
	mydir <<- ifelse(Sys.getenv('COMPUTERNAME') == 'TED-PC','/therocrates','D:/kamakura_r')
	Sys.setenv(HOME=mydir)
	Sys.setenv(R_USER=mydir)
	setwd(mydir)
	source(paste(mydir,"/kamakura_Rutils.r",sep=""))
} 

.Last <- function() {
	if(Sys.getenv('SESSIONNAME')=='Console') savehistory()
	odbcCloseAll()
}

if(!exists("tahiti")) {	#10.100.25.117
	dbdb <- 'TAHITI'
	dbuser <- 'TAHITI_User'
	dbpw <- 'An#51$q4!7g3'
	dbConnectTahiti <- function() {
		res <- odbcConnect(dbdb,dbuser,dbpw)
	if(res==-1)  { 
		paste("failed to connect to TAHIT") 
	} else {
		assign('tahiti',res,env=.GlobalEnv)
		invisible()
	}
	}
	dbConnectTahiti()
}

if(!exists("guam")) {	#10.100.25.117
	dbdb <- 'GUAM'
	dbuser <- 'GUAM_User'
	dbpw <- 'An#51$q4!7g3'
	dbConnectGuam <- function() {
			res <- odbcConnect(dbdb,dbuser,dbpw)
		if(res==-1)  { 
			paste("failed to connect to GUAM") 
		} else {
			assign('guam',res,env=.GlobalEnv)
			invisible()
		}
	}
	dbConnectTahiti()
}

if(!exists("therocrates") & Sys.getenv('COMPUTERNAME') == 'TED-PC') {
	dbdb <- 'therocrates'
	dbuser <- 'tspradlin'
	dbpw <- '123NEWpassword098_'
	dbConnectThero <- function() {
		res <- odbcConnect(dbdb,dbuser,dbpw,interpretDot=F)
	if(res==-1)  { 
		paste("failed to connect to THEROCRATES") 
	} else {
		assign('therocrates',res,env=.GlobalEnv)
		invisible()
	}
	}
	dbConnectThero()
}

sendQuery <- function(query="select name from sys.databases",dbh=tahiti,as.is=T) {
	res <- sqlQuery(dbh,query,as.is=as.is,errors=T,rows_at_time=1024)
	if(class(res) == 'character' && length(res) ==2) {
		res  <- append(res,odbcGetErrMsg(dbh))
		writeLines(res)
	} else {
		names(res) <- tolower(names(res))
	return(res)
	}
}

tq <- function(query="select name from sys.databases order by name",dbh=tahiti,as.is=T) {
	sendQuery(query,dbh,as.is)
}
thq	 <- function(query="select name from sys.databases order by name",dbh=therocrates,as.is=T) {
	sendQuery(query,dbh,as.is)
}

kamaWriteTable <- function(ds, dblocation, append=T, dbh=tahiti) {
	strs <- unlist(strsplit(dblocation, "\\."))
		dbname <- strs[1]
		tablename <- strs[2]
	dblocation <- paste(dbname,".dbo.",tablename,sep="")
	sendQuery(paste("use", dbname),dbh,T)
	if(!append) sendQuery(paste("truncate table ", tablename,sep=""),dbh,T)
	names(ds) <- toupper(names(ds))
	sqlSave(dbh, ds, tablename, append=T, rownames=F, safer=T, fast=T)
	invisible()
}
tahitiWriteTable <- function(ds,dblocation,append=T,dbh=tahiti) {
	kamaWriteTable(ds,dblocation,append,dbh)
}
theroWriteTable <- function(ds,dblocation,append=T,dbh=therocrates) {
	kamaWriteTable(ds,dblocation,append,dbh)
}

show_dbase_tables <- function(dbase='therocrates', db_con='tahiti') {
	query <- paste("select table_name from ",
				dbase,
			".information_schema.tables where table_type='base table'
			order by table_name", sep="")
	if(db_con=='tahiti')		res <- tq(query)
	if(db_con=='therocrates')	res <- thq(query)
return(res)
}

show_table_cols <- function(dbase = 'COMPUSTAT', table = 'COMPANY',db_con='TAHITI') {
query <- paste("select column_name as column_ , 
		(cast(data_type as varchar) + '  ' +
			(case when character_maximum_length is null then 
				(cast(numeric_precision as varchar) + ',' + 
				cast(numeric_precision_radix as varchar)) 
			else cast(character_maximum_length as varchar) end)) 
		as data_type 
		from ",dbase,".information_schema.columns 
		where table_name='",table,"'",sep="")
	if(db_con=='tahiti')		res <- tq(query)
	if(db_con=='therocrates')	res <- thq(query)

return(res)
}

mergeVectors <- function(dates_vector, merge_dates, merge_vals) {	# merge asynchronous data
	fullsetdates=dates_vector
	mdates=merge_dates
	mvals=merge_vals

	fullsetdates <- data.frame(fullsetdates)
		names(fullsetdates) <- c("date")
		fullsetdates$date <- as.integer(as.character(fullsetdates$date))
	mset <- data.frame(cbind(mdates, mvals))
		names(mset) <- c("date", "mvals")
		mset$date <- as.integer(as.character((mset$date)))
	msetone <- mset[mset$date <= fullsetdates$date[1],]

	mset <- merge(fullsetdates, mset, by="date", all.x=T, all.y=F)
	currentmval <- ifelse(nrow(msetone)>0, msetone$mvals[nrow(msetone)], NA)

		for(i in 1:nrow(mset)) {
			if(is.na(mset$mvals[i])) {
				mset$mvals[i] <- currentmval
			} else {
		currentmval <- mset$mvals[i]
			}
		}
	return(mset$mvals)
}
lagn <- function(d, n) {	#----- returns a vector that is d lagged by n
    if(length(d) <= n) return(rep(NA, length(d)))
    return(c(rep(NA, n), d[1:(length(d)-n)]))

}

uRank <- function(x, napos="mid") {
	lx <- length(x)
	if(lx==1) {
		if(is.na(x[1])) {
			if(napos=="lo") return (0)
			if(napos=="mid") return (0.5)
			if(napos=="hi") return (1)
		return(NA)
		}
	return(1)
	}
	dfo <- cbind((1:lx), x) 	# ----- preserve NA's here 
	df <- dfo[!is.na(dfo[,2]),]	
		t <- df[,1]
		x <- df[,2]
		r <- (rank(x) - 1) / (length(x) - 1)
	dfm <- merge(dfo, cbind(t, r), by=1, all.x=TRUE)
	#----- our new vector with NA's preserved 
	y <- dfm[,3]
	if(napos=="lo")	y <- ifelse(is.na(y), 0, y)
	if(napos=="hi")	y <- ifelse(is.na(y), 1, y)
	if(napos=='mid')	y <- ifelse(is.na(y),.5, y)
return(y)
}

ma <- function(x, n, na.rm=TRUE) {

    lx <- length(x)
    if(length(x) < n) { return(rep(NA, lx)) }
    if(sum(!is.na(x)) < n) { return(rep(NA, lx)) }

    if(na.rm==TRUE) {
        dfo <- cbind((1:lx), x)
        df <- dfo[!is.na(dfo[,2]),]
        t <- df[,1]
        x <- df[,2]
    }

    csx <- cumsum(x)
    csxlag <- c(rep(NA, (n-1)), 0, csx[0:(length(csx)-n)])
    mav <- (csx - csxlag) / n

    if(na.rm==TRUE) {
        dfm <- merge(dfo, cbind(t, mav), by=1, all.x=TRUE)
        mav <- dfm[,3]
    }

    return(mav)
}
MovingUnivariateRegression <- function(y, x, n, na.rm=TRUE, returnVal="residuals") {
	olen <- length(y)
	if(na.rm==TRUE) {
		dfo <- cbind(x,y)
		dfo <- data.frame(dfo)
		dfo$mergex <- 1:nrow(dfo)
		df <- dfo[!(is.na(dfo[,2]) | is.na(dfo[,1])),]
		x <- df[,1]
		y <- df[,2]
		mergexr <- df$mergex
	}
if (length(y) < n) return(rep(NA, olen))
	xy <- x * y
	csxy <- cumsum(xy)
	csxylag <- c(rep(NA, (n-1)), 0, csxy[0:(length(csxy)-n)])
	movsumxy <- csxy - csxylag
	xx <- x * x
	csxx <- cumsum(xx)
	csxxlag <- c(rep(NA, (n-1)), 0, csxx[0:(length(csxx)-n)])
	movsumxx <- csxx - csxxlag
	movxbar <- ma(x, n, na.rm)
	movybar <- ma(y, n, na.rm)
	movbeta <- (movsumxy - n*movxbar*movybar) / (movsumxx - n*(movxbar^2))
	movalpha <- movybar - movbeta*movxbar
	movresid <-  y - movbeta*x - movybar + movbeta*movxbar

	if(na.rm==TRUE) {
		dfm <- merge(dfo, cbind(mergexr, x, movresid, movbeta, movalpha),by=1, all.x=TRUE)
		xs <- dfm$x
		movresid <- dfm$movresid
		movbeta <- dfm$movbeta
		movalpha <- dfm$movalpha
	}
	percentchange <- xs*movbeta / movalpha

	if(returnVal=='all') {
		thing <- list(movalpha,movbeta,percentchange,movresid)
			names(thing) <- c('alpha','beta','pct_chg','resid')
		return(thing)
	}
	if(returnVal=="alphas") return(movalpha)
	if(returnVal=="betas") return(movbeta)
	if(returnVal=="percentChange") return(percentchange)
	#return((movbeta / (movalpha + xs*movbeta)))
return(movresid)     # default is to return residuals
}
getGrowthRate <- function(y, x, ppy, na.rm=TRUE) {

    # NA the negative or zero y's
    fy <- ifelse(y <= 0, NA, y)
    numy <- sum(!is.na(fy))
    if(numy >= 3) {
        b <- MovingUnivariateRegression(log(fy), x, numy, na.rm, returnVal="betas")
        b <- b[length(y)]
        ra <- exp(b*ppy) - 1
        return(100*ra)
    }
    return(NA)

}
getTimeStamp <- function() { return(format(Sys.time(),"%Y%m%d %a %X")) }
getMacroFactors <- function(rset=5) { 	# a number from 1 - 4 in  mfs.dbo.regress_set.reg_id column
							# any other number gives everything in the mfs.dbo.rf_values table

	rset_name <- ifelse(rset == 2, 'base_40', ifelse(rset == 3,'ccar',
		ifelse(rset == 5,'all_mac_facs', paste('macro_facs_',rset,sep=""))))

query <- paste("select v.rf_code, v.rf_value, substring(convert(char,v.data_date,112),1,6) as year_mo
	from (select rf_code, 
		year(data_date) as y_date, month(data_date) as m_date, max(data_date) as max_date 
		from mfs.dbo.rf_values group by year(data_date), month(data_date),  rf_code
	) m 
	join mfs.dbo.rf_values v on v.rf_code=m.rf_code and v.data_date=m.max_date order by v.data_date ")

	rf_values <- oq(query)
	rf_values <- rf_values[!is.element(rf_values$rf_code,c('OilChg1','Oilchg2','Oilchg3')),]
	rf_values <- unique(rf_values)
		rf_values$year_mo <- as.integer(rf_values$year_mo)
		rf_values$rf_value <- as.numeric(rf_values$rf_value)

	r_codes <- oq("select distinct rs.rf_num, rs.reg_id, rf.rf_code from mfs.dbo.regress_set rs 
			join mfs.dbo.risk_factors rf on rf.rf_num=rs.rf_num where rs.use_in_regress = 1
			group by rs.reg_id, rf_code, rs.rf_num")
							
#-----	which regress set do we want
	if(is.element(rset,c(1:4))) rf_values <-  rf_values[is.element(rf_values$rf_code,r_codes[r_codes$reg_id == rset,'rf_code']),]
	
	colz <- as.character(unique(rf_values$rf_code))
	rv <- data.frame(matrix(NA,nrow=length(unique(rf_values$year_mo)), ncol=(1+length(colz))))
		names(rv) <- c('year_mo',colz)
			rv$year_mo <- as.integer(unique(rf_values$year_mo))
				for(y in 1:nrow(rv))  {
					year_mo_set <-  rf_values[rf_values$year_mo == rv$year_mo[y],c('rf_code','rf_value')]
					colz <- as.character(unique(year_mo_set$rf_code))
				for(cl in colz) rv[y,cl] <- as.numeric(year_mo_set[year_mo_set$rf_code==cl,'rf_value'][1])
				}
			for(cl in names(rv[2:length(names(rv))])) rv[,cl]<-	mergeVectors(rv$year_mo,rv$year_mo,rv[,cl])
		rv <- rv[order(rv$year_mo,decreasing=F),]
#-----write out a copy of this thing	you will need to set your personal directory here...
	write.table(rv,file=paste(Sys.getenv('HOME'),"/mfs.dbo.",rset_name,"_rf_values.csv",sep=""),sep=",",quote=F,row.names=F)
	MacroFactors <- list(rv)
		names(MacroFactors) <- rset_name
return(MacroFactors)
}
getMacroFactorsRRF2 <- function(rset=5) { 	# a number from 1 - 4 in  mfs.dbo.regress_set.reg_id column
					# any other number gives everything in the mfs.dbo.rf_values table
	rset_name <- ifelse(rset == 2, 'base_40', ifelse(rset == 3,'ccar',
		ifelse(rset == 5,'all_mac_facs', paste('macro_facs_',rset,sep=""))))

	rf_values <- tq("select * from rrf2.dbo.rf_values")
	rf_values <- unique(rf_values)
		rf_values$year_mo <- as.integer(rf_values$year_mo)
		rf_values$rf_value <- as.numeric(rf_values$rf_value)

	r_codes <- tq("select distinct rs.rf_num, rs.reg_id, rf.rf_code from rrf2.dbo.regress_set rs 
			join rrf2.dbo.risk_factors rf on rf.rf_num=rs.rf_num where rs.use_in_regress = 1
			group by rs.reg_id, rf_code, rs.rf_num")
							
#-----	which regress set do we want
	if(is.element(rset,c(1:4))) rf_values <-  rf_values[is.element(rf_values$rf_code,r_codes[r_codes$reg_id == rset,'rf_code']),]
	
#-----	make a dataframe for ease of handing to lm vs kdp values
	colz <- as.character(unique(rf_values$rf_code))
	rv <- data.frame(matrix(NA,nrow=length(unique(rf_values$year_mo)), ncol=(1+length(colz))))
		names(rv) <- c('year_mo',colz)
			rv$year_mo <- as.integer(unique(rf_values$year_mo))
				for(y in 1:nrow(rv))  {
					year_mo_set <-  rf_values[rf_values$year_mo == rv$year_mo[y],c('rf_code','rf_value')]
					colz <- as.character(unique(year_mo_set$rf_code))
				for(cl in colz) rv[y,cl] <- as.numeric(year_mo_set[year_mo_set$rf_code==cl,'rf_value'][1])
				}
			for(cl in names(rv[2:length(names(rv))])) rv[,cl]<-	mergeVectors(rv$year_mo,rv$year_mo,rv[,cl])
		rv <- rv[order(rv$year_mo,decreasing=F),]
#-----write out a copy of this thing	you will need to set your personal directory here...
	write.table(rv,file=paste(Sys.getenv('HOME'),"/rrf2.dbo.",rset_name,"_rf_values.csv",sep=""),sep=",",quote=F,row.names=F)
	MacroFactors <- list(rv)
		names(MacroFactors) <- rset_name
return(MacroFactors)
}
getKdpJc5 <- function(gvkey=6066) {
	query <- paste("select jc.gvkey, substring(convert(char,jc.data_date,112),1,6) as year_mo,
				(case jc.kdp_1mo when 0 then -11.5129 when 100 then 11.5129 else
					-log((1-jc.kdp_1mo/100.0)/(jc.kdp_1mo/100.0)) end ) as kdp_1mo,
				(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 else
					-log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo,
				(case jc.kdp_1yr when 0 then -11.5129 when 100 then 11.5129 else
					-log((1.0-jc.kdp_1yr/100.0)/(jc.kdp_1yr/100.0))  end) as kdp_1yr,				
				(case jc.kdp_5yr when 0 then -11.5129 when 100 then 11.5129 else
					-log((1.0-jc.kdp_5yr/100.0)/(jc.kdp_5yr/100.0))  end) as kdp_5yr
			from kdp_ts.dbo.kdp_jc5  jc, 
					(select gvkey,  
						year(data_date) as y_date, 
						month(data_date) as m_date, 
						max(data_date) as max_date
					from kdp_ts.dbo.kdp_jc5 where gvkey='",
						gvkey, 
					"' group by year(data_date), month(data_date), gvkey
					) m
				where jc.gvkey=",
					gvkey, " and jc.gvkey=m.gvkey and jc.data_date=m.max_date order by jc.data_date",sep="")
		kdp_values <- oq(query)	
		kdp_values$year_mo <- as.integer(kdp_values$year_mo)
		kdp_values$kdp_1mo <- as.numeric(kdp_values$kdp_1mo)
		kdp_values$kdp_3mo <- as.numeric(kdp_values$kdp_3mo)
		kdp_values$kdp_1yr <- as.numeric(kdp_values$kdp_1yr)
		kdp_values$kdp_5yr <- as.numeric(kdp_values$kdp_5yr)
return(kdp_values[,-1])	#drop gvkey column
}
getKdpJc5RRF2 <- function(gvkey=6066) {
	query <- paste("select jc.gvkey, substring(convert(char,jc.data_date,112),1,6) as year_mo,
				(case jc.kdp_1mo when 0 then -11.5129 when 100 then 11.5129 else
					-log((1-jc.kdp_1mo/100.0)/(jc.kdp_1mo/100.0)) end ) as kdp_1mo,
				(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 else
					-log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo,
				(case jc.kdp_1yr when 0 then -11.5129 when 100 then 11.5129 else
					-log((1.0-jc.kdp_1yr/100.0)/(jc.kdp_1yr/100.0))  end) as kdp_1yr,				
				(case jc.kdp_5yr when 0 then -11.5129 when 100 then 11.5129 else
					-log((1.0-jc.kdp_5yr/100.0)/(jc.kdp_5yr/100.0))  end) as kdp_5yr
			from rrf2.dbo.kdp_jc5  jc
				where jc.gvkey='",
					gvkey,"' order by jc.data_date",sep="")
		kdp_values <- fq(query)	
		kdp_values$year_mo <- as.integer(kdp_values$year_mo)
		kdp_values$kdp_1mo <- as.numeric(kdp_values$kdp_1mo)
		kdp_values$kdp_3mo <- as.numeric(kdp_values$kdp_3mo)
		kdp_values$kdp_1yr <- as.numeric(kdp_values$kdp_1yr)
		kdp_values$kdp_5yr <- as.numeric(kdp_values$kdp_5yr)
return(kdp_values[,-1])	#drop gvkey column
}
getStockId <- function(tic="IBM") {		# takes tic or gvkey
	whereclause <- ifelse(!is.numeric(tic),paste("where s.tic='",tic,"'",sep=""),paste("where s.gvkey=",tic,sep=""))
	query <- paste("select s.gvkey, s.iid, s.tic, s.cusip, s.sedol, s.isin, c.conm, s.exchg, s.excntry, g.gdesc as sector, c.loc, c.priusa, c.prican, c.prirow 
		from market.dbo.security s
		left join market.dbo.company c on c.gvkey=s.gvkey
		left join market.dbo.gsector g on g.gsector=c.gsector ",
		whereclause,sep="")
	res <- oq(query)
	if(nrow(res)>0)  {
	res <- res[order(res$iid),] 
	res$primary_loc <- ifelse(!is.na(res$priusa),'USA',
					ifelse(!is.na(res$prican),'CAN','ROW'))
	res <- res[1,c('gvkey', 'iid', 'tic', 'conm','sector', 'exchg', 'loc', 'primary_loc','cusip','sedol','isin')]
	} else {
	res <- as.data.frame(matrix(nrow=0,ncol=11))
		names(res) <- c('gvkey', 'iid', 'tic', 'conm','sector', 'exchg', 'loc', 'primary_loc','cusip','sedol','isin')
	}
return(res)
}
getIdx <- function(d=NA,gvkeyx=3,idx=NA) {
	d <- ifelse(is.na(d),format(Sys.time(),"%Y%m%d"),d)
	gvkeyx <- ifelse(is.na(idx),gvkeyx,
		ifelse(idx=='500',3,
		ifelse(idx=='400',24248,
		ifelse(idx=='600',30824,
		ifelse(idx=='1500',31855,
		ifelse(idx=='100',664,
		ifelse(idx=='DJIA',5,
		ifelse(idx=='NASD',208,
		ifelse(idx=='ADR',161991,
		)))))))))

	query <- paste("select s.gvkey, s.iid, s.tic, c.conm 
			from compustat.dbo.idxcst_his i
			join compustat.dbo.security s on i.gvkey=s.gvkey and s.iid=i.iid 
			join compustat.dbo.company c on s.gvkey=c.gvkey
			where cast(i.gvkeyx as integer)=",gvkeyx,
			" and i.[from] <='",d,"' and (i.thru >='",d,"' or thru is null)", sep="")
	idx <- tq(query)
	conm <- tq(paste("select conm from compustat.dbo.idx_index where cast(gvkeyx as integer) = ",gvkeyx,sep=""))[[1]]
	idx <- list(conm,idx)
	names(idx) <- c('index_name','constituents')
return(idx)
}
getTotalRet <- function(gvkeys,iids,fromdate,todate,tics=F,as.return_factor=T) {

	rets <- NULL
	
	if(tics[1]!=F) {
		ids <- as.data.frame(matrix(nrow=length(tics),ncol=2))
			names(ids) <- c('gvkey','iid')
		for(x in 1:length(tics)) ids[x,] <- getStockId(tics[x])[,c('gvkey','iid')]
	gvkeys <- ids$gvkey
	iids <- ids$iid
	}

	for(x in 1:length(gvkeys)) {
		query <- paste("select p.gvkey, p.iid, p.datadate, (p.prccd/p.ajexdi) as prccd, 
			(case when d.div is null then 0 else d.div/p.ajexdi end) as div,
			(case when d.cheqv is null then 0 else d.cheqv/p.ajexdi end) as cheqv
		from market.dbo.security_daily p
			left join tahiti.compustat.dbo.sec_divid d on d.gvkey=p.gvkey 
				and d.iid=p.iid and d.datadate=p.datadate
		where p.gvkey='",
			gvkeys[x], 
			"' and p.iid='",
			iids[x],
			"' and (p.datadate between ",
			fromdate,
			" and ",
			todate,") 
		order by p.datadate", sep="")
	ds <- oq(query)
		if(nrow(ds)>1) {
			ds$prccd <- as.numeric(ds$prccd)
			ds$div <- as.numeric(ds$div)
			ds$cheqv <- as.numeric(ds$cheqv)
			p0 <- ds$prccd[1]
			p1 <- ds$prccd[nrow(ds)] + (sum(ds$div) + sum(ds$cheqv))
		rets[x] <- (p1/p0)
		} else {
		rets[x] <- NA
		}
	}
if(!(as.return_factor)) rets <- rets-1	#----- then return as percent in decimals with +/- values
rets
}

getVolMetrics <- function(gvkeys,iids,todate=tq(paste("select max(datadate) from compustat.dbo.sec_pvol_view where gvkey in('
					", paste(getIdx(idx='100')[[2]]$gvkey,sep="",collapse="','"),"')",sep=""))) {
	vol <- NULL
	vol130 <- NULL
	vol190 <- NULL

	for(x in 1:length(gvkeys)) {
		query <- paste("select top 90 datadate, (cshtrd*ajexdi) as cshtrd from compustat.dbo.sec_pvol_view where gvkey='",gvkeys[x],"' 
						and iid='",iids[x],"' and datadate<='",todate,"' order by datadate desc",sep="")
		ds <- tq(query)
			ds$cshtrd <- as.numeric(ds$cshtrd)
		
		if(max(ds$datadate) < todate) {
			vol[x] <- NA
			vol130[x] <- NA
			vol190[x] <- NA
			next
		}
		vol[x] <- ds$cshtrd[1]
		if(nrow(ds)==90) {
			vol130[x] <- vol[x]/mean(ds$cshtrd[1:30])
			vol190[x] <- vol[x]/mean(ds$cshtrd[1:90])
			next	
		}
		if(nrow(ds)>=30) {
			vol130[x] <- vol[x]/mean(ds$cshtrd[1:30])
			vol190[x] <- NA		
		}
	}
res <- as.data.frame(cbind(gvkeys,iids,round(as.numeric(vol*.001),0),round(as.numeric(vol130),2),round(as.numeric(vol190),2)))
	names(res) <- c('gvkey','iid','volume','vol1to30','vol1to90')
return(res)
}
	
today <- function() {
	return(as.integer(format(Sys.Date(),"%Y%m%d")))
}
addnYear <- function(dt=today(),n=1) {	# takes a positive or negative number
	new_year <- as.integer(substr(dt,1,4)) + n
	new_dt <- as.integer(paste(new_year,substr(dt,5,8),sep=""))
new_dt
}
removeWhiteSpace <- function(str='    this is a test    ') {
	return(sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", str, perl=TRUE))
}
getFredMetaData <- function(set,fname) {	#----	hand this function the top 40 lines of a FRED file
		#-----	do it like this:	readLines(fname,n=40)
		#-----set <- readLines(paste(mydir,"\\FRED2_txt_2\\data\\",sub('.csv','.txt',thesefiles[x]),sep=""),n=40)

	for(y in 1:length(set)) {
	if(set[y]=='') break
	set[y] <- gsub("'","",set[y])
	set[y] <- gsub(",","",set[y])
	set[y] <- gsub("#","",set[y])
	thing <- strsplit(as.character(set[y]),':')[[1]]
		if(thing[1]=='Title') series_title <- removeWhiteSpace(paste(thing[2:length(thing)],sep="",collapse=""))
		if(thing[1]=='Series ID') series_id <- removeWhiteSpace(paste(thing[2:length(thing)],sep="",collapse=""))
		if(thing[1]=='Source') series_source <- removeWhiteSpace(paste(thing[2:length(thing)],sep="",collapse=""))
		if(thing[1]=='Release') series_release <- removeWhiteSpace(paste(thing[2:length(thing)],sep="",collapse=""))
		if(thing[1]=='Seasonal Adjustment') series_adjustment <- removeWhiteSpace(paste(thing[2:length(thing)],sep="",collapse=""))
		if(thing[1]=='Frequency') series_freq <- removeWhiteSpace(paste(thing[2:length(thing)],sep="",collapse=""))
		if(thing[1]=='Units') series_units <- removeWhiteSpace(paste(thing[2:length(thing)],sep="",collapse=""))
		if(thing[1]=='Date Range') series_date_range <- removeWhiteSpace(paste(thing[2:length(thing)],sep="",collapse=""))
		if(thing[1]=='Last Updated') series_updated <-removeWhiteSpace(paste(thing[2:length(thing)],sep="",collapse=""))
		if(thing[1]=='Notes') {
		str <- paste(removeWhiteSpace(thing),sep="",collapse=" ")
			for(z in (y+1):length(set)) {
				if(substr(set[z],1,4)=='DATE') break
				str <- append(str,removeWhiteSpace(paste(set[z],sep="",collapse=""))) 
			}
		note_str <- gsub('"','',gsub("#","",gsub(",","",gsub("'","",paste(str,sep="",collapse=" ")))))
		}
	}
	if(exists("series_date_range")) {
		if(grep('to',series_date_range)) series_date_range <- as.integer(substr(strsplit(gsub('-','',series_date_range)," to ")[[1]][2],1,8))
			if(as.integer(substr(series_date_range,1,6)>201112)) {
			query <- paste("insert into fred.dbo.fred_series_info_tmp values('",
					paste(series_id,series_title,series_source,series_release,series_adjustment,
						series_freq,series_units,series_date_range,series_updated,fname,note_str,sep="','"),
				"')",sep="")
			query <- gsub("''",'NULL',query)
			fq(query)
		}
	}
}

makeFredTable <- function(col_names,table_name) {
	if(col_names[1] != 'datadate') return ("\n\t1st column must be datadate\n")
	col_names <- col_names[-1]
	fq(paste("CREATE TABLE ",table_name,"(DATADATE VARCHAR(8) NOT NULL,",
			paste(col_names,'FLOAT, ',sep=" ",collapse=" ")," PRIMARY KEY(DATADATE))",sep=""))
}

makeFredDataFrame <- function(these_sets) {	#ds with $series_filename and $series_id from fred_series_info table
	for(z in 1:nrow(these_sets)) {
		fname <- these_sets$series_filename[z]
		test <- readLines(fname,n=250)
		r=0
		for(a in 1:length(test)) if(r==0) r <- ifelse(substr(test[a],1,4)=='DATE',a,r)
	set <- read.table(fname,header=F,skip=(r))
		names(set) <- c('datadate',these_sets$series_id[z])
			set$datadate <- as.integer(gsub("-","",set$datadate))
			set[,these_sets$series_id[z]] <- as.numeric(set[,these_sets$series_id[z]]) 
	if(z==1)	sset <- set
	if(z!=1)	sset <- merge(sset,set,by='datadate',all.x=T,all.y=T)
	}
return(sset)
}

getFredFactors <- function(fred_table='us_doc_cb_m3_inv_orders',fred_cols=NULL) { 	#-----	if fred_cols is null then get everything
	if(is.null(fred_cols))  { 
		query <- paste("select * from fred.dbo.",fred_table," order by datadate",sep="")
	} else {
		query <- paste("select datadate, ",paste(fred_cols,sep="",collapse=", "),
				" from fred.dbo.",fred_table," order by datadate",sep="")
	}
	fred_factors <- fq(query)						#----- first column is always 'datadate'
	for(x in 2:ncol(fred_factors)) fred_factors[,x] <- as.numeric(fred_factors[,x])
		fred_factors$datadate <- substr(fred_factors$datadate,1,6)
		names(fred_factors) <- ifelse(names(fred_factors)=='datadate','year_mo',names(fred_factors))

	write.table(fred_factors,file=paste(mydir,"/",fred_table,today(),".csv",sep=""),sep=",",quote=F,row.names=F)
return(fred_factors)
}

getGvkeys <- function(quantiles=3,quantile=1,src='kdp_ts')  {
	if(src=='kdp_ts') {
		gvkeys <- oq(paste("select distinct gvkey from kdp_ts.dbo.kdp_jc5 order by gvkey"))[[1]]
	} else {
		gvkeys <- oq("select s.gvkey, s.iid from marketview.dbo.security s join marketview.dbo.company c on 
			c.gvkey=s.gvkey and c.priusa=s.iid and s.exchg in(14,12,11) 
			and len(s.iid)=2 and s.iid<90 and s.secstat='A' and c.loc='USA'
			order by gvkey")
		gvkeys <- paste(gvkeys$gvkey,"-",gvkeys$iid,sep="")
	}
	n <- length(gvkeys)
	qfac <- 1/quantiles
	max_qfac <- ifelse(quantile==1,qfac,qfac*quantile)
	if(quantile==1) return(gvkeys[gvkeys<gvkeys[n*max_qfac]])
	min_qfac <- qfac*(quantile-1)
	if(quantile==quantiles) return(gvkeys[gvkeys>=gvkeys[n*min_qfac]]) 
return(gvkeys[gvkeys>=gvkeys[n*min_qfac] & gvkeys<gvkeys[n*max_qfac]])
}

#addnMonth <- 
#usefull vectors of common tics
djia_30 <- getIdx(idx='DJIA')[[2]]$tic
sp_500 <- getIdx(idx='500')[[2]]$tic
sp_400 <- getIdx(idx='400')[[2]]$tic
sp_600 <- getIdx(idx='600')[[2]]$tic

holidays <- c(20140101,20140120,20140217,20140418,20140526,20140704,20140901,20141127,20141225,20150101,20150119,20150216,20150403,20150525,20150703,20150907,20151126,20151225)

mktdates <- as.integer(tq(paste("select distinct datadate from compustat.dbo.sec_dprc 
		where gvkey in('", paste(getIdx(idx='100')[[2]]$gvkey,sep="",collapse="','"),
			"') order by datadate",sep=""))[[1]])
mktdates <- mktdates[!is.element(mktdates,holidays)]

getmktdate <- function(dt=20121020,fwd=F) {	#-----	return closest market day forward or backward or if dt is a mkt day return dt
	if(is.element(dt,mktdates)) return(dt)
	if(!fwd) return(max(mktdates[mktdates<dt]))
	if(fwd) return(min(mktdates[mktdates>dt]))
}

subnmktdates <- function(dt=20120101,n=-1) {
	if(!is.element(dt,mktdates)) return('dt must be a valid market date!')
	if(n<0) thesedates <- mktdates[mktdates<=dt]
	if(n>0) thesedates <- mktdates[mktdates>dt]
	if(n<0) return(thesedates[(length(thesedates)+n)])
	if(n>0) return(thesedates[n])
}

strip_single_quotes <- function(filename,keepfile=FALSE) { 
   set <- gsub("'", "", readLines(filename)) 
   if(keepfile)  filename <- sub("(^.+?)(\\.[^.]*)?$", "\\1_clean\\2", filename)
   write.table(set,file=filename,sep=",",quote=F,row.names=F,col.names=T) 
}

getCountryInfo <- function(ccode)  {
		co_stuff <- (tq(paste("select co.* from compustat.dbo.r_country co where co.isocntrycd='",ccode,"'" ,sep="")))
	if(nrow(co_stuff)==0) return(writeLines("\nmust input valid iso3 country code\n"))
		idx_stuff <- tq(paste("select i.gvkeyx, i.conm, i.indexgeo from compustat.dbo.idx_index i where i.indexgeo='",ccode,"'",sep=""))

	retset <- list(co_stuff,idx_stuff)
	names(retset) <- c('country_codes','indexes')
return(retset)
}

getCountryCompanies <- function(ccode='COL') {
	these_companies <- oq(paste("select distinct s.gvkey, c.prirow, sec.tic, sec.sedol, sec.isin, c.conm,  c.loc, co.isocntrydesc, exc.exchgdesc
			from market.dbo.security_daily s, 
				market.dbo.company c,
				market.dbo.security sec,
				tahiti.compustat.dbo.r_ex_codes exc,
				tahiti.compustat.dbo.r_country co
			where s.gvkey=c.gvkey and s.gvkey=sec.gvkey 
			and c.loc in('",ccode,"')
			and exc.exchgcd=sec.exchg 
			and co.isocntrycd in('",ccode,"')
			and secstat='A' and substring(s.datadate,1,4)='2013' and substring(s.datadate,5,2)='07'",sep=""))
return(these_companies)
}

getCoinfo <- function(this='EBAY') {
oq(paste("select gvkey, tic, conm, country from kris.dbo.company_ticker where conm like('%",this,"%')",sep=""))
}

titleCase <- function(s, strict = FALSE) {
	cap <- function(s) paste(toupper(substring(s,1,1)),
		{s <- substring(s,2); if(strict) tolower(s) else s},sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

load_idxcst_his <- function(fileName='D:/compustat/data/f_idxcst_his.1.xml') {
	library(XML)
	timestamp()
		doc <- xmlTreeParse(fileName)
		dataset_date 	<- xmlRoot(doc)[[2]][[2]][[1]]
		data_group 	<- xmlRoot(doc)[[2]][[2]][[2]][[1]]$value

		if(data_group == 'idxcst_his') {

		dg_data		<- xmlRoot(doc)[[2]][[2]][[2]][-1]

		ds <- data.frame(matrix(nrow=0,ncol=5))
			names(ds) <- c('gvkey','iid','gvkeyx','fromdate','thru')

		dsss <- lapply(c(1:length(dg_data)),function(x) {
			cat(x,"..")
				dss <- ds
				thisgvkey <- dg_data[[x]]
				gvkey <- thisgvkey[[1]]$value
					iid_data <- xmlElementsByTagName(thisgvkey,'IID')
					for(y in 1:length(iid_data)) {
					thisiid <- iid_data[[y]]
					iid <- thisiid[[1]]$value
					gvkeyx_data <- xmlElementsByTagName(thisiid,'GVKEYX')
						for(z in 1:length(gvkeyx_data)) {
						thisgvkeyx <- gvkeyx_data[[z]]
						gvkeyx <- thisgvkeyx[[1]]$value
						fromdate_data <- xmlElementsByTagName(thisgvkeyx,'FROMDATE')
							for(a in 1:length(fromdate_data)) {
							thisfromdate <- fromdate_data[[a]]
							from <- thisfromdate[[1]]$value
							thru <- ifelse(is.null(thisfromdate$children$rectype$children$THRU$children$text$value),
									NA,thisfromdate$children$rectype$children$THRU$children$text$value)
				dss[(nrow(dss)+1),] <- cbind(gvkey,iid, gvkeyx,from,thru)
							}
						}
					}
			tahitiWriteTable(dss,'compustat.idxcst_his_test')
			})
		}
	timestamp()
}

getRRF3mf <- function(dt=today()) {
	rf_vals <- tq("select rf_code, convert(varchar(6),data_date,112) as year_mo, rf_value from samoa.mfs.dbo.rf_values_ccar")
	datez <- unique(as.numeric(rf_vals$year_mo))
		datez <- datez[order(datez,decreasing=F)]
	colz <- c('year_mo',as.character(unique(rf_vals$rf_code)))
	mfacs <- data.frame(matrix(NA,ncol=1,nrow=length(datez)))
			names(mfacs) <- colz[1]
			mfacs$year_mo <- datez
			for(x in 2:length(colz)) {
				mfacs <- merge(mfacs,rf_vals[rf_vals$rf_code==colz[x],c('year_mo','rf_value')],all.x=T,all.y=F,by.x='year_mo',by.y='year_mo')
					names(mfacs)[x] <- colz[x]
					mfacs[,x] <- as.numeric(mfacs[,x])
				}
	addthese <- c('UNEMP_1YD','UNEMP_6MD','UNEMP_2YD','PRIME_1YD','TR3M_1YD','TR5Y_1YD','TR10Y_1YD','MORT_1YD')
	colz <- c(colz,addthese)
		mfacs$UNEMP_1YD <- mfacs$UNEMP - lagn(mfacs$UNEMP,12)
		mfacs$UNEMP_6MD <- mfacs$UNEMP - lagn(mfacs$UNEMP,6)
		mfacs$UNEMP_2YD <- mfacs$UNEMP - lagn(mfacs$UNEMP,24)
		mfacs$PRIME_1YD <- mfacs$PRIME - lagn(mfacs$PRIME,12)
		mfacs$TR3M_1YD <- mfacs$TREAS3MO - lagn(mfacs$TREAS3MO,12)
		mfacs$TR5Y_1YD <- mfacs$TREAS5YR - lagn(mfacs$TREAS5YR,12)
		mfacs$TR10Y_1YD <- mfacs$TREAS10Y - lagn(mfacs$TREAS10Y,12)
		mfacs$MORT_1YD <- mfacs$MORT - lagn(mfacs$MORT,12)

	mfacs <- mfacs[,names(mfacs)[!is.element(names(mfacs),c('CREIDX','DJIDX','BBBCORP','HOUSEIDX'))]]	
	write.table(mfacs,file=paste("./tmp/mfacs_",dt,".csv",sep=""),sep=",",quote=T,row.names=F,append=F)
	return(mfacs)
}

dostepwise <- function(lset = lset,lside=kdp_cols[y]) {
	flag <- ifelse(sum((abs(lset$coefficients[,3][-1])<2))<2,FALSE,as.logical(sum((abs(lset$coefficients[,3][-1])<2))))
	tscrs <- abs(lset$coefficients[,3][-1])
	while(flag) {
		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(row.names(lset$coefficients)[-1][order(tscrs)][-1],collapse=" + "),sep="")),testset))
		flag <- ifelse(sum((abs(lset$coefficients[,3][-1])<2))<2,FALSE,as.logical(sum((abs(lset$coefficients[,3][-1])<2))))
        tscrs <- abs(lset$coefficients[,3][-1])
	}
return(lset)
}

trythisone <- function(frmla,testset,fwf,fwrsq) {
	wfbk <- fwf
	lset <- summary(lm(frmla,testset))
		if(is.na(lset$adj.r.squared) & !is.null(wfbk)) {
			lset <- summary(lm(as.formula(paste(lside,paste(wfbk,sep="",collapse=" + "),sep=" ~ ")),testset))
			return(list(lset,row.names(lset$coefficients)[-1],as.numeric(lset$adj.r.squared)))
		}
		if(is.na(lset$adj.r.squared) & is.null(wfbk))  {
			return(list(NULL,wfbk,fwrsq))
		}
		if(all(abs(lset$coefficients[,3][-1])>2) & as.numeric(lset$adj.r.squared) > fwrsq) {
			return(list(lset,row.names(lset$coefficients)[-1],lset$adj.r.squared))
		}
		if(!all(abs(lset$coefficients[,3][-1])>2)){
			lset <- dostepwise(lset,lside)
			if(!is.na(lset$adj.r.squared)) {
				return(list(lset,row.names(lset$coefficients)[-1],as.numeric(lset$adj.r.squared)))
			}
		}
		if(!is.null(wfbk)) {
			lset <- summary(lm(as.formula(paste(lside,paste(wfbk,sep="",collapse=" + "),sep=" ~ ")),testset))
			return(list(lset,row.names(lset$coefficients)[-1],as.numeric(lset$adj.r.squared)))
		} else {
			return(list(NULL,wfbk,fwrsq))
		}
}

runthese <- function(trythese,wf) {
	if(length(trythese)!=0) {
		for(xx in 1:length(trythese)){
			if(length(wf)==lmtr) {
				lmtr_flag = TRUE
				lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
				wf_holder <- wf
				wf <- row.names(lset$coefficients)[-1][order(abs(lset$coefficients[-1,3]))][-1]
			}
			mf <- trythese[xx]
			if(length(unique(testset[!is.na(testset[,mf]),mf]))==1 | all(is.na(testset[,mf]))) next
			frmla <- as.formula(paste(lside," ~ ",paste(append(wf,mf),sep="", collapse=" + "),sep=""))
			wf_wrsq <- trythisone(frmla,testset,wf,wrsq)
			if(all(abs(wf_wrsq[[1]]$coefficients[,3][-1])>2) & wf_wrsq[[3]]>wrsq) {
				wf <- wf_wrsq[[2]]
				wrsq <- wf_wrsq[[3]]
			} else {
				if(lmtr_flag) wf <- wf_holder
			}
		}
	}
	if(!is.null(wf)){
		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
		return(list(lset,row.names(lset$coefficients)[-1],as.numeric(lset$adj.r.squared)))
	} else {
	return(list(NULL,wf,wrsq))
	}
}	

e_notation_to_decimal <- function(vec=c(1.359088e-04,2.285336e-04,4.991319e-13,5.153306e-13,2.052615e-03,5.470723e-04,1.189199e-02,1.048679e-07)) {
these_strs <- strsplit(as.character(vec),'e')
	for(zz in 1:length(these_strs)) {
		if(length(these_strs[zz][[1]])>1) {
			if(as.integer(these_strs[zz][[1]][2])< -5) {
				these_strs[zz] <- as.character(0)
			} else {
				these_strs[zz] <- ifelse(as.integer(these_strs[zz][[1]][2])<0,
					as.character(substr(paste("0.",paste(rep(0,abs(as.integer(these_strs[zz][[1]][2]))),sep="",collapse=""),substring(gsub('\\.','',these_strs[zz][[1]][1]),1,6),sep=""),1,8)),
					as.character(paste(substr(gsub('\\.','',these_strs[zz][[1]][1]),1,(1+(abs(as.integer(these_strs[zz][[1]][2]))))),".",substr(gsub('\\.','',these_strs[zz][[1]][1]),(2+(abs(as.integer(these_strs[zz][[1]][2])))),(7+(abs(as.integer(these_strs[zz][[1]][2]))))),sep="")))
			}
		} else {
				these_strs[zz] <- as.character(these_strs[zz][[1]])
		}	
	}
	return(unlist(these_strs))
}


company_description <- function(tic='GIII') {
	stock_id <- getStockId(tic)
	busdesc <- tq(paste("select busdesc from compustat.dbo.company where gvkey='",stock_id$gvkey,"'",sep=""))
return(list(stock_id,busdesc))
}