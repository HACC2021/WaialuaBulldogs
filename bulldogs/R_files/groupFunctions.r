# group functions

# allows you to get an index on any historical date
getSPIndex <- function(date=NA, index="500", gvkeyx=NA) {

	if(is.na(date)) {
		date <- system("date +%F", intern=T)
		date <- dashedToYYYYMMDD(date)	
	}

	if(is.na(gvkeyx)) {
		if(index=="500") 		gvkeyx <- 3
		else if(index=="400") 	gvkeyx <- 24248
		else if(index=="600") 	gvkeyx <- 30824
		else if(index=="1500") 	gvkeyx <- 31855
		else if(index=="100") 	gvkeyx <- 664
		# can add all sorts of others, look in idx_index table:
		# mysql -h malthus -B -e 'select distinct h.gvkeyx, i.conm from compustat.idx_index i,
		#                         compustat.idxcst_his h where i.gvkeyx=h.gvkeyx' > indexes.csv
		# to get the whole available list.
		# but if you just want prices, those are in idx_daily and you can get that list here:
		# 
		# mysql -h malthus -B -e 'select distinct d.gvkeyx, i.conm from compustat.idx_index i,
		#                         compustat.idx_daily d where i.gvkeyx=d.gvkeyx
		#						  and datadate=20070816' > pindexes.csv

	}	

	query <- paste(	"select s.tic, s.gvkey, s.iid, s.cusip, ibtic ",
					"from compustat.idxcst_his i, compustat.security s ",
					"where gvkeyx=", gvkeyx, " ",
					"and s.gvkey=i.gvkey ",
					"and s.iid=i.iid ",
					"and (fromdt <='", date, "' ",
 					"and (thru >='", date, "' or thru is null) ) ", sep="")

	companies <- mq(query)
	return(companies)
}

# consolodate all these getGics functions, please!
getGicsSector <- function(date=20050405, gsector=45) {

	# our historical gics data only goes back to 19990630
	date <- ifelse(date < 19990630, 19990630, date)

	query <- paste(	"select s.tic, s.gvkey, s.iid, s.cusip, s.ibtic ",
				   	"from compustat.security s, compustat.co_hgic g, ",
					"compustat.company c ",
				   	"where s.gvkey=g.gvkey ",
				   	"and s.exchg in (11,12,14) ",
				   	"and g.gsector=", gsector, " ",
					"and c.gvkey=s.gvkey ",
					"and s.iid=c.priusa ",
					"and (indfrom <= '", date, "' ",
					"and (indthru >= '", date, "' or indthru is null) ) ", sep="")

	companies <- mq(query)	
	if(nrow(companies)>0) companies$cusip8 <- substr(companies$cusip, 1, 8)
	
	return(companies)

} 

getGicsIndustry <- function(date=20050405, gind=452030) {

	# our historical gics data only goes back to 19990630
	date <- ifelse(date < 19990630, 19990630, date)

	query <- paste(	"select s.tic, s.gvkey, s.iid, s.cusip, s.ibtic ",
				   	"from compustat.security s, compustat.co_hgic g, ",
					"compustat.company c ",
				   	"where s.gvkey=g.gvkey ",
				   	"and s.exchg in (11,12,14) ",
				   	"and g.gind=", gind, " ",
					"and c.gvkey=s.gvkey ",
					"and s.iid=c.priusa ",
					"and (indfrom <= '", date, "' ",
					"and (indthru >= '", date, "' or indthru is null) ) ", sep="")

	companies <- mq(query)	
	if(nrow(companies)>0) companies$cusip8 <- substr(companies$cusip, 1, 8)
	
	return(companies)

} 

getGicsSubIndustry <- function(date=20050405, gsubind=25301010) {

	# our historical gics data only goes back to 19990630
	date <- ifelse(date < 19990630, 19990630, date)

	query <- paste(	"select s.tic, s.gvkey, s.iid, s.cusip, s.ibtic ",
				   	"from compustat.security s, compustat.co_hgic g, ",
					"compustat.company c ",
				   	"where s.gvkey=g.gvkey ",
				   	"and s.exchg in (11,12,14) ",
				   	"and g.gsubind=", gsubind, " ",
					"and c.gvkey=s.gvkey ",
					"and s.iid=c.priusa ",
					"and (indfrom <= '", date, "' ",
					"and (indthru >= '", date, "' or indthru is null) ) ", sep="")

	companies <- mq(query)	
	if(nrow(companies)>0) companies$cusip8 <- substr(companies$cusip, 1, 8)
	
	return(companies)

} 

getAllGicsHistory <- function(date) {

	date <- ifelse(date < 19990630, 19990630, date)

	query <- paste(	"select gvkey, gsubind ",
				   	"from compustat.co_hgic g ",
				   	"where (indfrom <= '", date, "' ",
					"and (indthru >= '", date, "' or indthru is null) ) ", sep="")

	companies <- mq(query)
	return(companies)

}

getHistoricalIbg <- function(ibcode, date) {

	if(is.na(ibcode))  { return(999999) }

	query <- paste("select * from (select sector from ibeshist3 ",
					"where code=", ibcode, " and date_ <= to_date(", date,",'YYYYMMDD') ", 
					" order by date_ desc) where rownum=1", sep="")
	res <- sq(query)
	
	if(nrow(res)==1) {
		return(as.integer(res$sector[1]))
	}

	return(as.integer(NA))

}

getAllIbgHistory <- function(date) {
	query <- paste("select code, to_char(date_, 'YYYYMMDD') as odate, sector from ibeshist3")
	hist <- sq(query)
	names(hist) <- c("ibcode", "date", "ibg")
	return(hist)

}


getAllGicsHistoryEli <- function(date) {

	date <- ifelse(date < 19990630, 19990630, date)

	query <- paste(	"select gvkey, date_format(indfrom, '%Y%m%d') as indfrom, gsector, ggroup ",
				   	"from compustat.co_hgic g ",
				   	"where (indfrom <= '", date, "' ",
					"and (indthru >= '", date, "' or indthru is null) ) ", sep="")

	ds <- mq(query)

	# now, we need to remove any duplicates
	ds <- ds[order(ds$gvkey, ds$indfrom),]
	ds$fwdkey <- c(ds$gvkey[2:nrow(ds)], NA)
	ds <- ds[!(ds$gvkey==ds$fwdkey),]
	ds <- ds[!is.na(ds$gvkey),]

	ds <- ds[,c("gvkey", "gsector", "ggroup")]
	
	return(ds)

}


# companies is a 4 column data frame in this order tic, gvkey, iid, and cusip
computeGroupPE <- function(date=20050405, companies, period=1) {
	
	date <- as.numeric(date)
	codata <- data.frame(matrix(ncol=3, nrow=nrow(companies)))
	names(codata) <- c("market_cap", "earnings", "price")

	# now for each company we need market cap and projected p/e ratio for the current fiscal year
	for(i in 1:nrow(companies)) {

		#cat(paste("querying", i, companies$tic[i], "\n"))

		query <- paste("select prccd, cshoc, adrrc, ajexdi ",
			           "from compustat.sec_dprc ",
			           "where gvkey=", companies$gvkey[i], " ",
				   	   "and iid='", companies$iid[i], "' ",
			           "and datadate=", date, " ", sep="")
		stockdata <- mq(query)

		if(nrow(stockdata) > 0) {

			adrfactor <- ifelse(is.na(stockdata$adrrc),1,stockdata$adrrc)
		
			stockdata$prccd <- stockdata$prccd / stockdata$ajexdi
			stockdata$cshoc <- stockdata$cshoc * stockdata$ajexdi / adrfactor
			
			# get shares from quarterly table instead
			if(is.na(stockdata$cshoc)) {
				query <- paste(	"select cshoq, adrrq, ajexq ",
                 				"from compustat.co_qtrfund ",
				 				"where gvkey=", companies$gvkey[i], " ",
				 				"and datadate>=", subnyear(date, 1), " ",
			     	 			"and datadate<=", date, " ",
				 				"order by datadate desc", sep="")

				qtrdata <- mq(query)
				if(nrow(qtrdata)>0) qtrdata <- qtrdata[!is.na(qtrdata$cshoq),]
				if(nrow(qtrdata)>0) {
			 		adrfactor <- ifelse(is.na(qtrdata$adrrq[1]),1,qtrdata$adrrq[1])	
					stockdata$cshoc <- qtrdata$cshoq[1] * qtrdata$ajexq[1] *1E6 / adrfactor
				}
			}
		
			# and holy crap, if we still don't have shares then go 
			# to the annual table (happens in early history circa 1989)
			if(is.na(stockdata$cshoc)) {
            	query <- paste("select csho, adrr, ajex ",
                               "from compustat.co_annfund ",
                               "where gvkey=", companies$gvkey[i], " ",
                               "and datadate>=", subnyear(date, 1), " ",
                               "and datadate<=", date, " ",
                               "order by datadate desc", sep="")

                anndata <- mq(query)

				if(nrow(anndata)>0) anndata <- anndata[!is.na(anndata$csho),]
                if(nrow(anndata)>0) {
                	adrfactor <- ifelse(is.na(anndata$adrr[1]),1,anndata$adrr[1])
                    stockdata$cshoc <- anndata$csho[1] * anndata$ajex[1] * 1E6 / adrfactor
                }
            }

			ibesdata <- getIbesEpsEst(companies[i,], date, date, period) 
	
			codata$market_cap[i] <- stockdata$prccd * stockdata$cshoc
			codata$earnings[i] <- ifelse(nrow(ibesdata)>0, ibesdata$eps, NA)
			codata$price[i] <- stockdata$prccd
		}
	}

	codata <- codata[!is.na(codata$market_cap+codata$earnings),]
	if(nrow(codata) > 0) {
		sumcap <- sum(codata$market_cap)
		w <- codata$market_cap / sumcap
		wprice <- sum(w * codata$price)	
		wearn <- sum(w * codata$earnings)
		wppe <- wprice / wearn
		#wppe <- sum((codata$market_cap / sumcap) * codata$ppe, na.rm=T)
		wppe <- ifelse(is.infinite(wppe), NA, wppe)
		wppe <- ifelse(is.nan(wppe), NA, wppe)
	} else {
		wppe <- NA
		wearn <- NA
	}
	#return(codata)
	return(list(wppe, wearn))

}

# gets top n stocks by market cap on the given date using PIT data
getTopX <- function(date, n=1000) {

	date <- getBusinessDayBackwards(date)

	if(date >= 19980401) {  # then it's relatively easy since cshoc in daily table!
		query <- paste("select p.gvkey, p.iid, s.tic, conml, prccd, cshoc, ajexdi, adrrc ",
#					   "from backtest.sec_dprc p, backtest.security s, backtest.company c ", tmp change!
					   "from compustat.sec_dprc p, compustat.security s, compustat.company c ",
					   "where datadate=", date, " ",
					   "and cshoc is not null ",
					   "and prccd is not null ",
					   "and p.curcdd='USD' ",
					   "and s.gvkey=p.gvkey ",
					   "and s.iid=p.iid ",
					   "and c.gvkey=p.gvkey ",
					   "and s.tpci in ('0', 'F') ",
					   "and p.iid not in ('01C', '90C', '02C', '03C') ",
					   "and exchg in (11, 12, 14)",
					   "and secstat='A'",
				   	   "and s.iid=c.priusa",
					   #"and (select exchg from backtest.co_pointidhist ",
					   #"	where gvkey=p.gvkey ", 
					   #"	and changedate <= p.datadate ",
					   #"	order by changedate desc limit 1) in (1,2,3) ",
					   #"and p.gvkey=6066 ",
					   sep="")

		res <- mq(query)
		res$adrrc <- ifelse(is.na(res$adrrc), 1, res$adrrc)
		res$cap <- res$prccd * res$cshoc / res$adrrc
	
	} else {  # otherwise this is a serious pain in the a

		fromdate <- subnyear(date, 1)

		query <- paste("select p.gvkey, p.iid, prccd, pit.cshoq, ajexdi, pit.ajexq ",
					   "from backtest.sec_dprc p, ",
					   "backtest.co_pointhstkeys keyt, backtest.co_pointfund pit ",
					   "where keyt.gvkey=pit.gvkey ",
					   "and keyt.datadate=pit.datadate ",
					   "and keyt.pointdate < ", date, " ",
					   "and keyt.pointdate > ", fromdate, " ",
					   "and p.gvkey=keyt.gvkey ",
					   "and p.datadate=", date, " ",
					   sep="")

		res <- mq(query)

		res$price <- res$prccd / res$ajexdi
		res <- res[!is.na(res$shares) & !is.na(res$price),]
		res$cap <- res$price * res$shares
	}

	res <- res[,c("gvkey", "iid", "tic", "conml", "cap")]

	# consolodate company dupes into best iid
	fres <- res[1,]
	fres <- fres[-1,]
	fc <- 1
	keys <- unique(res$gvkey)

	for(i in 1:length(keys)) {
		rows <- res[res$gvkey==keys[i],]
		rows <- rows[order(rows$iid),]
		fres[fc,] <- rows[1,]
		fc <- fc + 1
	}

	fres <- fres[order(fres$cap, decreasing=T),]
	if(nrow(fres)>n) {
		fres <- fres[1:n,]
	}

	return(fres)
}

