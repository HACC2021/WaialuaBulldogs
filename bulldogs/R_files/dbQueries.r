# various useful querry functions

getStockId <- function(tic) {

        # go get an infocode for this ticker
        query <- paste("select infocode as dsinfocode, dsmnem from ds2ctryqtinfo ",
                                   "where statuscode='A' ",
                                   "and (dsmnem like '@", tic, "' or dsmnem like 'U:", tic, "') ", sep="")
        res <- sq(query)

        if(nrow(res)==0) { return(data.frame()) }

        query <- paste("select s.seccode, cusip, sedol, isin from secmap m, secmstr s where ventype=34 ",
                                    "and m.seccode=s.seccode and type_=1 ",
                                        "and vencode=", res$dsinfocode[1], sep="")
        id <- sq(query)

        if(nrow(id)==0) { return(data.frame()) }

        query <- paste("select ventype, vencode from secmap ",
                                   "where seccode=", id$seccode, " ",
                                   "and ventype in (1, 7, 28, 34) and rank=1 ", sep="")
        res3 <- sq(query)

        if(nrow(res3) > 0) {
                id$tic <- tic
                id$wscode <- ifelse(nrow(res3[res3$ventype==7,]) > 0, res3[res3$ventype==7, "vencode"], NA)
                id$ibcode <- ifelse(nrow(res3[res3$ventype==1,]) > 0, res3[res3$ventype==1, "vencode"], NA)
                id$dsinfocode <- res$dsinfocode[1]

                if(is.na(id$wscode)) {
                        id$wscode <- ifelse(nrow(res[res$ventype==28,]) > 0, res[res$ventype==28, "vencode"], NA)
                }

        } else {
                return(data.frame())
        }

        return(id)

}



getStockIdFromSeccode <- function(seccode) {


        query <- paste("select ventype, vencode from secmap ",
                                   "where seccode=", seccode, " ",
                                   "and ventype in (1, 7, 28) and rank=1 ", sep="")
        res3 <- sq(query)

        if(nrow(res3) > 0) {
                id$tic <- tic
                id$wscode <- ifelse(nrow(res3[res3$ventype==7,]) > 0, res3[res3$ventype==7, "vencode"], NA)
                id$ibcode <- ifelse(nrow(res3[res3$ventype==1,]) > 0, res3[res3$ventype==1, "vencode"], NA)
                id$dsinfocode <- res$dsinfocode

                if(is.na(id$wscode)) {
                        id$wscode <- ifelse(nrow(res[res$ventype==28,]) > 0, res[res$ventype==28, "vencode"], NA)
                }

        } else {
                return(data.frame())
        }

        return(id)
}


getValiddates <- function() {

	query <- paste(	"select distinct to_char(marketdate, 'YYYYMMDD') as odate ",
					"from ds2primqtprc where infocode in (46244, 46238, 46176, 39988) ",
					"order by odate", sep="")
	
	validdates <- sq(query)

	validdates <- as.integer(validdates[order(validdates$odate),])
	return(validdates)
}

# if date is not a business day, return the next business day
# you must have a validdates object in memory for this to work!!
getBusinessDay <- function(date=c(20040101, 20030304)) {

	newdate <- NA
	for(i in 1:length(date)) {
		newdate[i] <- validdates[validdates>=date[i]][1]
	}
	return(newdate)
}

# return the next business day
getNextBusinessDay <- function(date=c(20040101, 20030304)) {

	newdate <- NA
	for(i in 1:length(date)) {
		newdate[i] <- validdates[validdates>date[i]][1]
	}
	return(newdate)
}

# if date is not a business day, return the prior business day
# you must have a validdates object in memory for this to work!!
getBusinessDayBackwards <- function(date=c(20040101, 20030304)) {

	newdate <- NA
	for(i in 1:length(date)) {
		lvd <- validdates[validdates<=date[i]]
		newdate[i] <- lvd[length(lvd)]
	}
	return(newdate)
}

# return the prior business day
# you must have a validdates object in memory for this to work!!
getPriorBusinessDay <- function(date=c(20040101, 20030304)) {

	newdate <- NA
	for(i in 1:length(date)) {
		lvd <- validdates[validdates<date[i]]
		newdate[i] <- lvd[length(lvd)]
	}
	return(newdate)
}


# gets Ibes data from the flash tables for annual or LTG periods
getIbesFlashEpsEst <- function(sid, period, lagged=F) {

	if(is.na(sid$ibcode)) { return (NA) }
	qitem <- ifelse(lagged, "mean4wk", "mean")

	ptype <- 1
	if(period=="LTG") { 
		ptype <- 4
		period <- -1
	}

	query <- paste("select ", qitem, " as eps, meanflash as feps ",
			  	   "from ibqsuml1 ",
			  	   "where code=", sid$ibcode, " ",
				   "and periodtype=", ptype, " ",
				   "and measure=8 ",
				   "and forecast='", period+1, "' ",
				   "order by perioddate desc", sep="")
	ibesdata <- sq(query)
		
	if(nrow(ibesdata)==0) { return (NA) }
	ibesdata <- ibesdata[1,]
	if(!lagged && !is.na(ibesdata$feps) && ibesdata$feps != -9999 && ibesdata$feps != -99999) {
		ibesdata$eps <- ibesdata$feps
	}
	
	return(ibesdata$eps)
}

# gets Ibes data from the flash tables for quarterly periods
getIbesFlashEpsEstQtr <- function(sid, period, lagged=F) {

	ibfps <- c('6', '7', '8', '9', 'N', 'O', 'P', 'Q')
	period <- ibfps[period+1]

	if(is.na(sid$ibcode)) { return (NA) }
	qitem <- ifelse(lagged, "mean4wk", "mean")

	query <- paste("select ", qitem, " as eps, meanflash as feps ",
			  	   "from ibqsuml1 ",
			  	   "where code=", sid$ibcode, " ",
				   "and periodtype=2 ",
				   "and measure=8 ",
				   "and forecast='", period, "' ",
				   "order by perioddate desc", sep="")
	ibesdata <- sq(query)
		
	if(nrow(ibesdata)==0) { return (NA) }
	ibesdata <- ibesdata[1,]
	if(!lagged && !is.na(ibesdata$feps) && ibesdata$feps != -9999 && ibesdata$feps != -99999) {
		ibesdata$eps <- ibesdata$feps
	}
	
	return(ibesdata$eps)

}

getQuarterlyData <- function(sid, items=c(3101, 5202, 5301), mnemonics=c("lctq", "epsfxq", "cshoq"),
							 fromdate, todate, smooth=c(F,T,F), NAQtrs=0) {
	
	fromyear <- trunc(fromdate / 10000)
	toyear <- trunc(todate / 10000) + 1

	ndcols <- ""
	gdcols <- NA
	gotDso <- F

	for(i in 1:length(items)) {

		query <- paste("select to_char(date_, 'YYYYMMDD') as datex, ",
					   "freq, year_ as year, seq as fqtr, ",
					   "value_ as value from wsndata where ",
					   "code=", sid$wscode, " and item=", items[i], " ",
					   "and freq in ('@', 'Q', 'S', 'F') ",
					   "and year_ >= ", fromyear, " ",
					   "and year_ <= ", toyear, " ",
					   "order by year, fqtr, freq desc ", sep="")
		ds <- sq(query)

		if(nrow(ds)==0) {
			if(i==length(items) && !gotDso) {
				return(data.frame())
			} else {
				ndcols <- c(ndcols, mnemonics[i])
			}

		} else {
		
			if(all(ds$freq=='Q' | ds$freq=='@')) { # pure quarterly
				restates <- ds[ds$freq=='@',]
				nres <- nrow(restates)
				if(nres > 0) {
					for(j in 1:nres) {
						ds <- ds[!((ds$year == restates$year[j]) &
								  (ds$fqtr  == restates$fqtr[j]) &
								  (ds$freq == 'Q')),] 
					}
				}

				# put in the absent quarters
				minyear <- min(ds$year)
				maxyear <- max(ds$year)
				dssemi <- ds[1,c("year", "fqtr")]
				rindex <- 2
				for(j in minyear:maxyear) {
					for(k in 1:4) {
						dssemi[rindex,] <- dssemi[1,]			
						dssemi$year[rindex] <- j
						dssemi$fqtr[rindex] <- k
						rindex <- rindex + 1
					}
				}
				dssemi <- dssemi[-1,]
				n <- nrow(ds)
				dssemi <- dssemi[!(dssemi$year==maxyear & dssemi$fqtr > ds$fqtr[n]),]
				ds <- merge(ds, dssemi, all.x=T, all.y=T)

			} else if(all(ds$freq=='S' | ds$freq=='F')) { # pure semi-annual
				restates <- ds[ds$freq=='F',]
				nres <- nrow(restates)
				if(nres > 0) {
					for(j in 1:nres) {
						ds <- ds[!((ds$year == restates$year[j]) &
								  (ds$fqtr  == restates$fqtr[j]) &
								  (ds$freq == 'S')),] 
					}
				}

				# put in the absent quarters
				minyear <- min(ds$year)
				maxyear <- max(ds$year)
				dssemi <- ds[1,c("year", "fqtr")]
				rindex <- 2
				for(j in minyear:maxyear) {
					for(k in 1:4) {
						dssemi[rindex,] <- dssemi[1,]			
						dssemi$year[rindex] <- j
						dssemi$fqtr[rindex] <- k
						rindex <- rindex + 1
					}
				}
				dssemi <- dssemi[-1,]

				n <- nrow(ds)
				ds$fqtr <- ifelse(ds$fqtr==2, rep(4, n), rep(2, n))
				dssemi <- dssemi[!(dssemi$year==maxyear & dssemi$fqtr > ds$fqtr[n]),]
				ds <- merge(ds, dssemi, all.x=T, all.y=T)
	
				if(smooth[i]) {
					prevals <- c(ds$value[2:nrow(ds)], NA)
					ds$value <- ifelse(is.na(ds$value), prevals, ds$value)
					ds$value <- ds$value / 2				
				} else { # carrydown
					curval <- ds$value[1]
					for(j in 2:nrow(ds)) {
						if(is.na(ds$value[j])) {
							ds$value[j] <- curval
						} else {
							curval <- ds$value[j]
						}
					}
				}

			
			} else {
				cat ("WARNING! Unhandled frequency case!\n")
				return(data.frame())
			}

			ds <- ds[,c("year", "fqtr", "datex", "freq", "value")]	
			names(ds) <- c("year", "fqtr", "date", "freq", mnemonics[i])
			ds <- ds[,c("date", "year", "fqtr", mnemonics[i])]

			if(gotDso) {
				gdcols <- c(gdcols, mnemonics[i])
				dso <- merge(dso, ds, by=c("year", "fqtr"), all.x=T, all.y=T)
				dso$date.x <- ifelse(is.na(dso$date.x) & !is.na(dso$date.y), dso$date.y, dso$date.x)
				dso <- dso[,c("date.x", "year", "fqtr", gdcols)]
				names(dso)[1] <- "date"			
			} else {
				gdcols <- mnemonics[i]
				dso <- ds
				gotDso <- T
			}				
		}
	}

	# missing anything?
	if(length(ndcols) > 1) {
		for(i in 2:length(ndcols)) {
			dso[,ndcols[i]] <- NA
		}
	}

	# put in an often-useful quarters-back vector
	dso <- dso[order(dso$year, dso$fqtr),]
	a <- dso$fqtr
	b <- lagn(a, 1)
	chg <- ifelse(a!=b, 1, 0)
	chg <- ifelse(is.na(chg), 0, chg)
	if(length(chg)>=2) {
		chg <- c(chg[2:length(chg)], 0)
	}

	dso$qtrsback <- rev(cumsum(rev(chg)))

	return(dso)
}

getCurrentQuarterlyData <- function(sid, items=c(5006, 5009, 5145, 5190, 5255),
									mnemonics=c("price", "pdate", "div12", "dvrate", "epsf12")) {
	
	ndcols <- ""
	gdcols <- NA
	gotDso <- F

	for(i in 1:length(items)) {

		# what table is this item in?
		query <- paste("select table_ from wsitem where number_=", items[i], sep="")
		tbl <- sq(query)
		tblc <- as.character(tbl$table_[1])

		query <- paste("select value_ as value from ws", tblc, "data where ",
					   "code=", sid$wscode, " and item=", items[i], " ",
					   "and freq='C' ", sep="")
		ds <- sq(query)

		if(nrow(ds)==0) {
			if(i==length(items) && !gotDso) {
				return(data.frame())
			} else {
				ndcols <- c(ndcols, mnemonics[i])
			}

		} else {

			if(gotDso) {
				gdcols <- c(gdcols, mnemonics[i])
				names(ds) <- mnemonics[i]
				dso <- merge(dso, ds, all.x=T, all.y=T)
			} else {
				gdcols <- mnemonics[i]
				names(ds) <- mnemonics[i]
				dso <- ds
				gotDso <- T
			}				
		}
	}

	# missing anything?
	if(length(ndcols) > 1) {
		for(i in 2:length(ndcols)) {
			dso[,ndcols[i]] <- NA
		}
	}

	return(dso)
}



getYtdData <- function(sid, items=c(4860, 4601), mnemonics=c("oancfy", "capxy"),
					   fromdate, todate) {
	
	fromyear <- trunc(fromdate / 10000)
	toyear <- trunc(todate / 10000) + 1

	ndcols <- ""
	gotDso <- F

	for(i in 1:length(items)) {

		query <- paste("select to_char(date_, 'YYYYMMDD') as datex, ",
					   "freq, year_ as year, seq as fqtr, ",
					   "value_ as value from wsndata where ",
					   "code=", sid$wscode, "  and item=", items[i], " ",
					   "and freq in ('R','E') ",
					   "and year_ >= ", fromyear, " ",
					   "and year_ <= ", toyear, " ",
					   "order by year, fqtr, freq desc ", sep="")
		ds <- sq(query)

		if(nrow(ds)==0) {
			if(i==length(items) && !gotDso) {
				return(data.frame())
			} else {
				ndcols <- c(ndcols, mnemonics[i])
			}
		}
		
		restates <- ds[ds$freq=='E',]
		nres <- nrow(restates)
		if(nres > 0) {
			for(j in 1:nres) {
				ds <- ds[!((ds$year == restates$year[j]) &
						  (ds$fqtr  == restates$fqtr[j]) &
						  (ds$freq == 'R')),] 
			}
		}

		# put in the absent quarters
		minyear <- min(ds$year)
		maxyear <- max(ds$year)
		dssemi <- ds[1,c("year", "fqtr")]
		rindex <- 2
		for(j in minyear:maxyear) {
			for(k in 1:4) {
				dssemi[rindex,] <- dssemi[1,]
				dssemi$year[rindex] <- j
				dssemi$fqtr[rindex] <- k
				rindex <- rindex + 1
			}
		}
		dssemi <- dssemi[-1,]
		n <- nrow(ds)
		dssemi <- dssemi[!(dssemi$year==maxyear & dssemi$fqtr > ds$fqtr[n]),]
		ds <- merge(ds, dssemi, all.x=T, all.y=T)

		names(ds) <- c("year", "fqtr", "date", "freq", mnemonics[i])
		ds <- ds[,c("date", "year", "fqtr", mnemonics[i])]
		
		if(gotDso) {
			dso <- merge(dso, ds, by=c("year", "fqtr"), all.x=T, all.y=T)
			dso$date.x <- ifelse(is.na(dso$date.x) & !is.na(dso$date.y), dso$date.y, dso$date.x)
			dso <- dso[,c("date.x", "year", "fqtr", mnemonics[1:i])]
			names(dso)[1] <- "date"			
		} else {
			dso <- ds
			gotDso <- T
		}				

	}

	# missing anything?
	if(length(ndcols) > 1) {
		for(i in 2:length(ndcols)) {
			dso[,ndcols[i]] <- NA
		}
	}

	# put in an often-useful quarters-back vector
	dso <- dso[order(dso$year, dso$fqtr),]
	a <- dso$fqtr
	b <- lagn(a, 1)
	chg <- ifelse(a!=b, 1, 0)
	chg <- ifelse(is.na(chg), 0, chg)
	if(length(chg)>=2) {
		chg <- c(chg[2:length(chg)], 0)
	}

	dso$qtrsback <- rev(cumsum(rev(chg)))

	return(dso)
}


getDailyData <- function(sid, fromdate, todate, getShares=F, getDivs=F, smooth=F) {

	query <- paste("select to_char(marketdate,'YYYYMMDD') as mktdate, ",
					"open_, high, low, close_, volume ",
					"from ds2primqtprc where infocode=", sid$dsinfocode, " ",
					"and marketdate >= to_date(", fromdate,",'YYYYMMDD') ",
					"and marketdate <= to_date(", todate,",'YYYYMMDD')", sep="")
	res <- sq(query)

	np <- nrow(res)
	if(np < 1) { return(data.frame()) }

	names(res) <- c("date", "open", "high", "low", "close", "volume")

	if(getShares)	{

		query <- paste("select to_char(eventdate,'YYYYMMDD') as evndate, ",
					   	"numshrs as shares from ds2numshares ",
						"where infocode=", sid$dsinfocode, " ",
						"and eventdate >= to_date(", subnyear(fromdate, 10),",'YYYYMMDD') ",
						"and eventdate <= to_date(", todate, ",'YYYYMMDD')", sep="")
		numShares <- sq(query)
		ns <- nrow(numShares)

		if(ns < 1) {
			# have to get shares somewhere else, let's try quarterly data
			nis <- getQuarterlyData(sid, c(5301), c("cshoc"), subnyear(fromdate,10), todate, F)
			nis <- nis[!is.na(nis$cshoc),]
		 	nis <- nis[nrow(nis),]
			if(nrow(nis)==1) {
				res$shares <- nis$cshoc
			} else {			
				res$shares <- NA
			}
		} else {
			res <- merge(res, numShares, by.x="date", by.y="evndate", all.x=T, all.y=T)

			if(nrow(res) > 1) {
				for(i in 2:nrow(res)) {
					if(is.na(res$shares[i])) {
						res$shares[i] <- res$shares[i-1]
					}
				}
			}
			res$shares <- res$shares * 1000
		}
	}

	if(getDivs) {
		query <- paste("select to_char(effectivedate,'YYYYMMDD') as edate, divrate ",
						"from ds2div ",
						"where infocode= ", sid$dsinfocode, " ",
						"and effectivedate >= to_date(",fromdate,",'YYYYMMDD') ",
						"and effectivedate <= to_date(",todate,",'YYYYMMDD')", sep="")
		div <- sq(query)

		if(nrow(div) < 1) {
			res$div <- NA
		} else {
			# sum dividends with the same date
			divd <- div
			divd$divrate <- NA
			divd <- unique(divd)
			for(i in 1:nrow(divd)) {
				divd$divrate[i] <- sum(div[div$edate==divd$edate[i], "divrate"], na.rm=T)
			}

			names(divd) <- c("date", "div")
			res <- merge(res, divd, by="date", all.x=T, all.y=T)
		}
	}

	# get adjustment factor merged in
	query <- paste("select to_char(adjdate,'YYYYMMDD') as adate, cumadjfactor as adjfac ",
					"from ds2adj where infocode=", sid$dsinfocode, " ",
					"and adjtype=2", sep="")
	da <- sq(query)
	
	if(nrow(da)>0) {
		names(da) <- c("date", "adjfac")
		res <- merge(res, da, all.x=T, all.y=T)
	
		lastadj <- res$adjfac[1]
		for(i in 2:nrow(res)) {
			if(is.na(res$adjfac[i])) {
				res$adjfac[i] <- lastadj
			} else {
				lastadj <- res$adjfac[i]
			}
		}

	} else {
		res$adjfac <- 1
	}

	res <- res[res$date >= fromdate,]

	if(smooth) {
		# make sure we have all dates in there in the middle range
		firstdate <- res$date[1]
		lastdate <- res$date[nrow(res)]
		vo <- validdates[(validdates >= firstdate) & (validdates <= todate)]
		if(length(vo[vo > lastdate]) > 10) {
			enddate <- lastdate
		} else {
			enddate <- todate
		}
		dsx <- data.frame(cbind(vo, rep(1, length(vo))))
		names(dsx) <- c("date", "dummy")
		res <- merge(res, dsx, all.x=T, all.y=T)
		res$dummy <- NULL

		for(i in 2:nrow(res)) {
			res$close[i] <- ifelse(is.na(res$close[i]), res$close[i-1], res$close[i])
			res$adjfac[i] <- ifelse(is.na(res$adjfac[i]), res$adjfac[i-1], res$adjfac[i])
		}
	}

	res$date <- as.integer(res$date)
	return(res)

}

# get fully adjusted prices with CRSP method
getPricesCRSP <- function(sid, fromdate, todate, smooth=T) {
		
	query <- paste("select to_char(marketdate,'YYYYMMDD'), close_, volume ",
					"from ds2primqtprc where infocode=", sid$dsinfocode, " ",
					"and marketdate >= to_date(", fromdate,",'YYYYMMDD') ",
					"and marketdate <= to_date(", todate,",'YYYYMMDD')", sep="")
	ds <- sq(query)

	if(nrow(ds) < 1) { return(data.frame()) }
	names(ds) <- c("date", "price", "volume")

	# get adjustment factor merged in
	query <- paste("select to_char(adjdate,'YYYYMMDD') as adate, cumadjfactor as adjfac ",
					"from ds2adj where infocode=", sid$dsinfocode, " ",
					"and adjtype=2", sep="")
	da <- sq(query)

	if(nrow(da)>0) {
		names(da) <- c("date", "adjfac")
		ds <- merge(ds, da, all.x=T, all.y=T)
	
		lastadj <- ds$adjfac[1]
		for(i in 2:nrow(ds)) {
			if(is.na(ds$adjfac[i])) {
				ds$adjfac[i] <- lastadj
			} else {
				lastadj <- ds$adjfac[i]
			}
		}

	} else {
		ds$adjfac <- 1
	}

	ds <- ds[ds$date >= fromdate,]
	ds$price <- ds$price * ds$adjfac
	ds$volume <- ds$volume / ds$adjfac

	query <- paste("select to_char(effectivedate,'YYYYMMDD') as divdate, divrate ",
					"from ds2div ",
					"where infocode= ", sid$dsinfocode, " ",
					"and effectivedate >= to_date(",fromdate,",'YYYYMMDD') ",
					"and effectivedate <= to_date(",todate,",'YYYYMMDD')", sep="")
	div <- sq(query)

	if(nrow(div) < 1) {
		ds$div <- NA
	} else {
		# sum dividends with the same date
		divd <- div
		divd$divrate <- NA
		divd <- unique(divd)
		for(i in 1:nrow(divd)) {
			divd$divrate[i] <- sum(div[div$divdate==divd$divdate[i], "divrate"], na.rm=T)
		}

		names(divd) <- c("date", "div")
		ds <- merge(ds, divd, by="date", all.x=T, all.y=T)
	}
	ds$div <- ifelse(is.na(ds$div), 0, ds$div)
	ds$div <- ds$div * ds$adjfac
 
	# smooth
	if(smooth) {
		# make sure we have all dates in there in the middle range
		firstdate <- ds$date[1]
		lastdate <- ds$date[nrow(ds)]
		vo <- validdates[(validdates >= firstdate) & (validdates <= todate)]
		if(length(vo[vo > lastdate]) > 10) {
			enddate <- lastdate
		} else {
			enddate <- todate
		}
		dsx <- data.frame(cbind(vo, rep(1, length(vo))))
		names(dsx) <- c("date", "dummy")
		ds <- merge(ds, dsx, all.x=T, all.y=T)
		ds$dummy <- NULL

		for(i in 2:nrow(ds)) {
			ds$price[i] <- ifelse(is.na(ds$price[i]), ds$price[i-1], ds$price[i])
		}
	}

	# now adjust out the dividends
	n <- nrow(ds)
	ds$div <- c(ds$div[2:n], 0)
	ds$divm <- 1 - (ds$div/ds$price)
	ds$divm <- rev(cumprod(rev(ds$divm)))
	
	ds <- ds[,c("date", "price", "volume")]
	ds$date <- as.integer(ds$date)

	return(ds)
}

# write a whole dataset to pre-existing table on malthus
# append=F first truncates the table
# the names of ds need to correspond exactly to the names of the table you're
# attempting to write into
malthusWriteTable <- function(ds, dblocation, append=T) {

	strs <- unlist(strsplit(dblocation, "\\."))
	dbname <- strs[1]
	tablename <- strs[2]

	query <- paste("use", dbname)
	res <- mq(query)

	if(!append) {
		query <- paste("truncate table", tablename)
		res <- mq(query)
	}

	sqlSave(chm, ds, tablename, append=T, rownames=F, safer=T, fast=T)

	invisible()

}

# write a whole dataset to pre-existing table on any mysql DB
# append=F first truncates the table
# the names of ds need to correspond exactly to the names of the table you're
# attempting to write into
dbWriteTable <- function(dbh, ds, dblocation, append=T, ifast=T) {

	strs <- unlist(strsplit(dblocation, "\\."))
	dbname <- strs[1]
	tablename <- strs[2]

	query <- paste("use", dbname)
	res <- sqlQuery(dbh, query, as.is=T)

	if(!append) {
		query <- paste("truncate table", tablename)
		res <- sqlQuery(dbh, query, as.is=T)
	}

	sqlSave(dbh, ds, tablename, append=T, rownames=F, safer=T, fast=ifast)

	invisible()

}

# get stocks to run for pretty much everything
getFsysUniverse <- function() {

	query <- paste("select s.seccode, id, name, cusip, sedol, vencode as dsinfocode, rank ",
				   "from secmstr s, secmap m ",
				   "where type_=1 and vendors like '%Q%' and vendors like '%W%' ",
				   "and s.seccode=m.seccode and ventype=34", sep="")
	ds <- sq(query)

	query <- paste("select infocode as dsinfocode, dsseccode, dsmnem from ds2ctryqtinfo ",
				   "where statuscode='A' and typecode in ('ADR', 'EQ')")
	dsa <- sq(query)
	ds <- merge(ds, dsa, by=c("dsinfocode"), all.x=F, all.y=F)

	query <- paste("select infocode as dsinfocode, exchintcode from ds2primqtprc ",
					"where exchintcode in (12, 135, 140, 145, 244, 252, 254, 255) ",
					"and marketdate=to_date(", validdates[length(validdates)],
					", 'YYYYMMDD') ", sep="")
	dsa <- sq(query)
	ds <- merge(ds, dsa, by=c("dsinfocode"), all.x=F, all.y=F)
	
	ds <- ds[,c("seccode", "dsinfocode", "cusip", "sedol", "dsmnem", "dsseccode", "rank")]
	ds <- ds[!is.na(ds$dsmnem),]
	ds <- unique(ds)
	
	ds$tic <- as.character(ds$dsmnem)

	# clean up the tickers
	for(i in 1:nrow(ds)) {
		if(substr(ds$tic[i], 1, 1)=='@') {
			ds$tic[i] = substr(ds$tic[i], 2, nchar(ds$tic[i]))	
		} else {
			ds$tic[i] = substr(ds$tic[i], 3, nchar(ds$tic[i]))	
		}
	}

	# merge in other stuff we need
	query <- paste("select seccode, vencode as wscode from secmap ",
				   "where ventype=7 and rank=1")
	dsa <- sq(query)

	query <- paste("select seccode, vencode as wscodeb from secmap ",
				   "where ventype=28 and rank=1")
	dsb <- sq(query)
	
	dsa <- merge(dsa, dsb, all.x=T, all.y=T)
	dsa$wscode <- ifelse(is.na(dsa$wscode), dsa$wscodeb, dsa$wscode)
	
	dsa <- dsa[,c("seccode", "wscode")]
	ds <- merge(ds, dsa, all.x=F, all.y=F)

	query <- paste("select seccode, vencode as ibcode from secmap ",
				   "where ventype=1 and rank=1")
	dsa <- sq(query)
	ds <- merge(ds, dsa, all.x=T, all.y=F)

	# clean up
	ds <- ds[,c("seccode", "cusip", "sedol", "tic", "wscode", "ibcode", "dsinfocode", "dsseccode")]
	ds$cusip <- as.character(ds$cusip)
	ds$sedol <- as.character(ds$sedol)

	return(ds)
}

# get ranks, scores from snapshot, caprank or urank, and urank_groupu
# if date is NA go to the snapshot table
# if date is a date, go to snapshot_history table
getIndicatorScores <- function(capgroup,
			       sitems=c("price_nom", "mktcap", "cfgr1"),
			       citems=c("mktcap", "cfgr1"),
			       gitems=c(""), date=NA) {

	ls <- ifelse(sitems[1]!=c(""),length(sitems),0)
	lc <- ifelse(citems[1]!=c(""),length(citems),0)
	lg <- ifelse(gitems[1]!=c(""),length(gitems),0)

	dbtable <- ifelse(capgroup==0, "urank", "caprank")
	cstring <- ifelse(capgroup==0, "",
					  paste("and s.capgroup=", capgroup, " ", sep="") )

	if(ls>0) {
		scolumns <- paste(sitems,sep="",collapse=", s.")
		scolumns <- paste("s.",scolumns,sep="")
	}
	if(lc>0) {
		ccolumns <- paste(citems, sep="", collapse=paste("_", dbtable, ", c.", sep=""))
		ccolumns <- paste("c.", ccolumns, "_", dbtable, sep="")
	}
	if(lg>0) {
		gcolumns <- paste(gitems,sep="",collapse="_urank_groupu, g.")
		gcolumns <- paste("g.",gcolumns,"_urank_groupu",sep="")
	}

	if(ls>0 && lc>0 && lg>0) {
		columns <- paste(scolumns,", ",ccolumns,", ",gcolumns," ",sep="")
	} else
	if(ls+lc==0) {
		columns <- paste(gcolumns," ",sep="")
	} else
	if(ls+lg==0) {
		columns <- paste(ccolumns," ",sep="")
	} else
	if(lc+lg==0) {
		columns <- paste(scolumns," ",sep="")
	} else
	if(ls==0) {
		columns <- paste(ccolumns,", ",gcolumns," ",sep="")
	} else
	if(lc==0) {
		columns <- paste(scolumns,", ",gcolumns," ",sep="")
	} else
	if(lg==0) {
		columns <- paste(scolumns,", ",ccolumns," ",sep="")
	} else {
		columns <- paste("price ",sep="")
	}

	if(is.na(date)) {

		query <- paste( "select s.seccode, s.tic, ",
						columns,
						"from fsys.snapshot s, fsys.", dbtable, " c, ",
						"fsys.groupranku g, fsys.groupmem m ",
						"where c.seccode=s.seccode ",
						"and m.seccode=s.seccode ",
						"and g.giccode=m.ind ",
						cstring,
						sep="")
	} else {

		# get the date to use
		query <- paste("select date_format(date, '%Y%m%d') as date ",
					   "from fsys.snapshot_history ",
					   "where seccode=36799 ",
					   "and date <=", date, " ",
					   "order by date desc limit 1", sep="")
		qdate <- as.integer(mq(query)[1,1])

		if(lg > 0) {
			query <- paste( "select s.seccode, s.tic, ",
						columns,
						"from fsys.snapshot_history s, fsys.", dbtable, "_history c, ",
						"fsys.groupranku_history g, fsys.groupmem m ",
						"where s.seccode=c.seccode ",
						"and s.seccode=m.seccode ",
						"and m.ind=g.giccode ",
						"and s.date=", qdate, " ",
						"and c.date=s.date and g.date=s.date ",
						cstring,
						sep="")
		} else {
			query <- paste( "select s.seccode, s.tic, ",
						columns,
						"from fsys.snapshot_history s, fsys.", dbtable, "_history c ",
						"where s.seccode=c.seccode ",
						"and s.date=", qdate, " ",
						"and c.date=s.date ",
						cstring,
						sep="")
			
		}
	}
	ds <- mq(query)

	return(ds)
}

# get ranks, scores from caprank or snapshot for just one stock
getIndicatorScoresSid <- function(sid,
			       sitems=c("price", "mktcap", "cfgr1"),
			       citems=c("mktcap", "cfgr1"),
			       gitems=c("mktcap", "cfgr1")) {

	ls <- ifelse(sitems[1]!=c(""),length(sitems),0)
	lc <- ifelse(citems[1]!=c(""),length(citems),0)
	lg <- ifelse(gitems[1]!=c(""),length(gitems),0)

	if(ls>0){
		scolumns <- paste(sitems,sep="",collapse=", s.")
		scolumns <- paste("s.",scolumns,sep="")
	}
	if(lc>0){
		ccolumns <- paste(citems,sep="",collapse="_caprank, c.")
		ccolumns <- paste("c.",ccolumns,"_caprank",sep="")
	}
	if(lg>0){
		gcolumns <- paste(gitems,sep="",collapse="_urank_groupu, g.")
		gcolumns <- paste("g.",gcolumns,"_urank_groupu",sep="")
	}

	if(ls>0 && lc>0 && lg>0) {
		columns <- paste(scolumns,", ",ccolumns,", ",gcolumns," ",sep="")
	} else
	if(ls+lc==0) {
		columns <- paste(gcolumns," ",sep="")
	} else
	if(ls+lg==0) {
		columns <- paste(ccolumns," ",sep="")
	} else
	if(lc+lg==0) {
		columns <- paste(scolumns," ",sep="")
	} else
	if(ls==0) {
		columns <- paste(ccolumns,", ",gcolumns," ",sep="")
	} else
	if(lc==0) {
		columns <- paste(scolumns,", ",gcolumns," ",sep="")
	} else
	if(lg==0) {
		columns <- paste(scolumns,", ",ccolumns," ",sep="")
	} else {
		columns <- paste("price ",sep="")
	}

	query <- paste( "select s.seccode, s.tic, ",
					columns,
					"from fsys.snapshot s, fsys.caprank c, ",
					"fsys.groupranku g, fsys.groupmem m ",
					"where s.seccode=c.seccode ",
					"and s.seccode=m.seccode ",
					"and m.ind=g.giccode ",
					"and s.seccode=", sid$seccode, " ",
					sep="")
	ds <- mq(query)

	return(ds)
}

# subtract business days from YYYYMMDD format
subnbusdays <- function(date=20070827, n=1) {

	N <- length(validdates[validdates <= date])	
	day <- validdates[N-n]
	return(day)
}

getLastPitDate <- function() {

	query <- paste("select max(date_format(pointdate, '%Y%m%d')) as date ",
 				   "from compustat.co_pointhstkeys ",
				   "where seccode in (6066, 12141, 5047)", sep="")

	res <- mq(query)
	return(as.integer(res$date))

}

getLastIbesDate <- function() {

	query <- paste("select max(date_format(ibstper, '%Y%m%d')) as date ",
		  	   "from compustat.ibmsumstat ",
		  	   "where ibtic in ('IBM', 'MSFT', 'GE') ",
			   "and ibfp='QTR' ",
			   "and ibmeas='EPS' ", sep="")

	res <- mq(query)	
	return(as.integer(res$date))

}

getLastDailyDate <- function() {

	query <- paste("select max(date_format(datadate, '%Y%m%d')) as date ",
 				   "from compustat.sec_dprc ",
				   "where seccode in (6066, 12141, 5047)", sep="")

	res <- mq(query)
	return(as.integer(res$date))

}

getMonthlyDates <- function() {

	query <- paste("select date_format(datadate, '%Y%m%d') as date ",
				   "from compustat.co_mthly ",
				   "where seccode=6066 and datadate >= 19831201 ",
				   "order by datadate ", sep="")
	mo <- mq(query)
	mo <- as.integer(mo[,1])

	return(mo)
}
