# general filtering utilities

# return series lagged n 
lagn <- function(d, n) {
	if(length(d) <= n) return(rep(NA, length(d)))
	return(c(rep(NA, n), d[1:(length(d)-n)])) 
}

# h is data frame with ticker and shares (must not have prices!)
# p is data frame with ticker and prices (any other stuff ok too)
portval <- function(p, h) {
    s <- merge(p,h,by="ticker", all.x=F, all.y=T)
    val <- sum(s$price * s$shares)
    val
}

# lowest rank for highest x
# warning --- outdated, do not use
# also, NA's always have highest rank
normrank <- function(x) {
    r <- rank(x)
    r / max(r)
}

# distribute x as a bounded normal-like rank on 0 to 1
# give this x with all elements in [0,1], and it will give you
# a [0,1] ranking, distributed close to normally, with the percent
# of ranks between -1 and +1 16.7% range (1/6th) being about equal
# to what you ask for in pctmid 
#
# note that the [0,1] limits mean that the distribution is truncated
# which results in slightly larger tails.  the population of scores
# between 1 and 2 standard deviations out is a bit light, but close.
# for the default 6 sigma range, the mid bins hold 11.2% instead of
# 13.6% for example
#
# pctmid must be in the range of 40 to 90, 60 is standard
# at 40 a uniform distribution is returned (but use uniformRank for that)
# at 90 a (probably unusable) super-kertotic distribution is returned
#
# when you start shifting the midpoint around, the pctmid figures
# remain about right for a non-truncated normal distribution
#
# napos is where you want the NAs to go on [0,1]
#
# trunc=T will create the same distribution as with trunc=F, but will
# redistribute it to the usual right-half of the trunc=F case
#
normalRank <- function(x, midpoint=0.5, pctmid=60, napos=0.5, truncdist=F, inverse=F) {

	lx <- length(x)
	dfo <- cbind((1:lx), x)
	df <- dfo[!is.na(dfo[,2]),]
	t <- df[,1]
	x <- df[,2]

	if(inverse) x <- x * -1
	
	if(truncdist) {
		x <- c(rep(-1E10, length(x)), x)
	}
	
	r <- rank(x) - 1
	n <- length(r) - 1
	p <- r / n

	shift <- -0.5 + (0.5-midpoint)/5
	pctmid <- ifelse(pctmid < 40, 40, pctmid)
	pctmid <- ifelse(pctmid > 100, 100, pctmid)

	mult <- pi*((((pctmid-1)/2)-19.5)/30)^(1/4)
	mult <- ifelse(mult < 0.05, 0.05, mult) # limit mult to [.05, ~(pi-0.05)]
	mult <- ifelse(mult > 3.1, 3.1, mult)
	y <- tan(mult*(p+shift))
	y <- y - min(y)
	y <- y / max(y)
	
	# do a left truncation if called for:
	if(truncdist) {
		#yrep <- rep(2, length(y))
		#y <- rep(y, yrep)
		yn <- length(y)
		midpt <- yn / 2
		y <- y[(midpt+1):yn]
		#yn <- length(y)

		# now restretch to [0,1]
		y <- y - min(y)
		y <- y / max(y)
	} 

    dfm <- merge(dfo, cbind(t, y), by=1, all.x=TRUE)
    y <- dfm[,3]

	y <- ifelse(is.na(y), napos, y)

	y
}

gaussianKernel <- function(x) {
	y <- (1/sqrt(2*pi))*exp((-1/2)*x^2)
	y
}

# napos can be "lo", "mid", "hi", or "na"
# distributes on [0,1] (inclusive) with lowest x having 0, highest having 1 etc.
# and all spacing information is destroyed
uniformRank <- function(x, napos="mid") {

	if(length(x)==1) {
		if(is.na(x[1])) {
			if(napos=="lo") return (0)
			if(napos=="mid") return (0.5)
			if(napos=="hi") return (1)
			return(NA)
		}
		return(1)
	}

	lx <- length(x)
	dfo <- cbind((1:lx), x)
	df <- dfo[!is.na(dfo[,2]),]
	t <- df[,1]
	x <- df[,2]

	r <- (rank(x) - 1) / (length(x) - 1)

	dfm <- merge(dfo, cbind(t, r), by=1, all.x=TRUE)
    y <- dfm[,3]
	
	if(napos=="lo") {
		y <- ifelse(is.na(y), 0, y)
	} else if (napos=="na") {
		y <- ifelse(is.na(y), NA, y)
	} else if (napos=="hi") {
		y <- ifelse(is.na(y), 1, y)
	} else {
		y <- ifelse(is.na(y), 0.5, y)
	}

	y

}

# n point moving average on x
# assumes most recent date is last element of x
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


# n point moving variance of x
# assumes most recent date is last element of x
mvar <- function(x, n, na.rm=TRUE) {

	lx <- length(x)

    if(na.rm==TRUE) {
        dfo <- cbind((1:lx), x)
        df <- dfo[!is.na(dfo[,2]),]
        t <- df[,1]
        x <- df[,2]
    }

	if(length(x) < n) { return(rep(NA, lx)) }

    csx <- cumsum(x)
    csxlag <- c(rep(NA, (n-1)), 0, csx[0:(length(csx)-n)])
    mav <- (csx - csxlag) / n
	var <- rep(NA, length(mav))
	
	for(i in n:length(mav)) {
		var[i] <- (1/(n-1)) * sum((x[(i-n+1):i] - mav[i])^2)
	}
	
    if(na.rm==TRUE) {
        dfm <- merge(dfo, cbind(t, var), by=1, all.x=TRUE)
        var <- dfm[,3]
    }

    return(var)
}


# variable period ema computation
ema <- function(x, period, na.rm=T) {
	
	a <- 2/(period+1)
	am1 <- 1 - a
	lastgood <- x[1]
	for (t in 2:(length(x))) {
		if(is.na(x[t-1]) && !is.na(x[t])) {
			if(!is.na(lastgood)) {
				x[t] <- lastgood
			}
			# else don't change x[t]
		}
		else {
			x[t] <- am1*x[t-1] + a*x[t]
			lastgood <- x[t]
		} 
	}
	x
}


# rolling summation
RollingSum <- function(x, n, na.rm=TRUE, na.subzero=FALSE) {

	 olx <- length(x)
     if (length(x) < n) { 
          rolsum <- rep(NA, olx) 
     }

     else {
          if(na.rm==TRUE) {
               a <- 1:length(x)
               dfo <- cbind(a,x)
               df <- dfo[!is.na(dfo[,2]),]
               a <- df[,1]
               x <- df[,2]
			   if(length(x) < n) {
			      return(rep(NA, olx))
			   }		
          } else {
               if(na.subzero==TRUE) x <- ifelse(is.na(x), 0, x)
          }
     
          csx <- cumsum(x)
          csxlag <- lagn(csx, n)
		  csxlag[n] <- 0  # otherwise this will be NA, but is really ok as zero
          
          if(na.rm==TRUE) {
             dfm <- merge(dfo, cbind(a, csx, csxlag), by=1, all.x=TRUE)
             csx <- dfm$csx
             csxlag <- dfm$csxlag
          }
          rolsum <- csx - csxlag
     }
     rolsum
}


# take log of points to compute an annual growth rate with regression
# ppy is periods per year of the x, y data
computeExpGrowthRate <- function(y, x, ppy, na.rm=TRUE) {

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

# take log of points to compute an annual growth rate with regression
# ppy is periods per year of the x, y data
# then computes SD of residuals and returns that in original non-exponentiated units
# if you want a 5-year figure, pass in 10 years of data (first 5 years are startup NAs)
computeExpGrowthRateStability <- function(y, x, n, ppy, na.rm=TRUE) {

	# NA the negative or zero y's
	fy <- ifelse(y <= 0, NA, y) 
	numy <- sum(!is.na(fy))
	if(numy >= 3) {
		e <- MovingUnivariateRegression(log(fy), x, n, na.rm, returnVal="residuals")
		sde <- sd(e, na.rm=T)
		sde <- exp(sde*ppy) - 1				
		return(sde)
	}
	return(NA)
	
}

# efficient way to compute rolling regression residual series
# see notes for more information
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

    if (length(y) < n) { 
        return(rep(NA, olen)) 
	} 
	else {    
        
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
        	dfm <- merge(dfo, cbind(mergexr, x, movresid, movbeta, movalpha),
						 by=1, all.x=TRUE)
			xs <- dfm$x
        	movresid <- dfm$movresid
			movbeta <- dfm$movbeta
			movalpha <- dfm$movalpha
    	}
    }
	
	if(returnVal=="alphas") return(movalpha)
	if(returnVal=="betas") return(movbeta)
	if(returnVal=="percentChange") return(xs*movbeta / movalpha) 
					#return((movbeta / (movalpha + xs*movbeta)))
	return(movresid) # default is to return residuals

}


# same as MovingUnivariateRegression except that n is set to the maximum
# available length for the date given 
MovingUnivariateMaxNRegression <- function(y, x, MinN=12, na.rm=TRUE) {
    
    if(na.rm==TRUE) {
        dfo <- cbind(x,y)
        df <- dfo[!is.na(dfo[,2]),]
        x <- df[,1]
        y <- df[,2]
    }

    if (length(y) < MinN) { 
        movresid <- rep(NA, length(y)) 
    }
    else {    
        
        xy <- x * y
        movsumxy <- cumsum(xy)
    
        xx <- x * x
        movsumxx <- cumsum(xx)

		movsumx <- cumsum(x)
        movsumy <- cumsum(y)

		# cumulative n, but make first MinN NA's
        n <- c(rep(NA,MinN-1), MinN:length(x))

        movxbar <- movsumx / n
        movybar <- movsumy / n

        movbeta <- (movsumxy - n*movxbar*movybar) / (movsumxx - n*(movxbar^2))
        movalpha <- movybar - movbeta*movxbar
   
        movresid <- movbeta*x + movybar - movbeta*movxbar - y
    }

    if(na.rm==TRUE) {
        dfm <- merge(dfo, cbind(x, movresid), by=1, all.x=TRUE)
        movresid <- dfm$movresid
    }
    
    # you could return other things if you wanted.  be sure to change
    # or add to the na.rm block above if you do this
    return(movresid)
}


# chop each security out of the dataset and run price-trend regression
# return the residual at each time period 
SecurityRegress <- function(d, periodx, MinPtsForReg=12) {
    
    residx <- rep(NA, nrow(d))
    
    for(s in unique(d$ticker)) {
        sd <- d[d$ticker==s,]
        obs <- nrow(sd)
        sd <- sd[(obs-periodx:obs),]
        resid(lm(sd$price ~ 1:nrow(ds)))
    }

}


# row-wise regression last-period residual returner
rowregres <- function (d, periodx, MinPtsForReg=12) {

    # residuals from trend regression over various periods
    residx <- rep(NA, nrow(d))
    
    subp <- d[,1:(min(periodx, ncol(d)))]
    maxt <- ncol(subp)
    t <- 1:maxt
    
    for (i in 1:nrow(subp)) {
	if (sum(as.numeric(!is.na(subp[i,]))) > MinPtsForReg) {
            tmp <- resid(lm(rev(as.numeric(subp[i,])) ~ t))
            residx[i] <- tmp[length(tmp)]
        }
    }
    residx
}


# convert X200405 date format to c(year, month)
dateLabelToDatecode <- function (dt) {
    yr <- substr(dt,2,5)
    m <- substr(dt,6,7)
    c(as.numeric(yr), as.numeric(m))
}

YYYYMMDDtoDate <- function(date) {
	date <- as.character(date)
	y <- substr(date, 1,4)
	m <- substr(date, 5,6)
	d <- substr(date, 7,8)
	paste(m, d, y, sep="/")
}

# convert X200405 date format to YYYYMMDD format
dateLabelToYYYYMMDD <- function (dt) {
    yr <- substr(dt,2,5)
    m <- substr(dt,6,7)
    d <- "01"
    as.numeric(paste(yr, m, d, sep=""))
}


# convert slash-separated dates to YYYYMMDD format
dateToYYYYMMDD <- function (dt=c("1/2/3")) {
    res <- NA
    resi <- 1
    for(i in 1:length(dt)) {
		if(!is.na(dt[resi])) {
	        a <- strsplit(dt[resi], "/")
    	    yr <- as.numeric(a[[1]][3])
    	    if(yr < 10) yr <- yr + 2000
    	    m <- as.numeric(a[[1]][1])
    	    d <- as.numeric(a[[1]][2])
    	    res[resi] <- yr * 10000 + m * 100 + d
		} else {
			res[resi] <- NA			
		}

    	resi <- resi + 1
    }
    res
}

# convert dash-separated dates to YYYYMMDD format
dashedToYYYYMMDD <- function (dt=c("2005-09-14")) {
    res <- NA
    resi <- 1
    for(i in 1:length(dt)) {
		if(!is.na(dt[resi])) {
	        a <- strsplit(dt[resi], "-")
	        yr <- as.numeric(a[[1]][1])
	        if(yr < 10) yr <- yr + 2000
	        m <- as.numeric(a[[1]][2])
	        d <- as.numeric(a[[1]][3])
	        res[resi] <- yr * 10000 + m * 100 + d
	    } else {
			res[resi] <- NA			
		}
		resi <- resi + 1
	}
    res
}

slashedToDashed <- function(dt=c("1/2/2003")) {
	dates <- as.character(dateToYYYYMMDD(as.character(dt)))
	for(i in 1:length(dt)) {
		date <- as.character(dates[i])
		y <- substr(date, 1,4)
		m <- substr(date, 5,6)
		d <- substr(date, 7,8)
		dates[i] <- paste(y, m, d, sep="-")
	}
	dates
}

YYYYMMDDToDashed <- function(dates=c(20060530)) {

	for(i in 1:length(dates)) {
		date <- as.character(dates[i])
		y <- substr(date, 1,4)
		m <- substr(date, 5,6)
		d <- substr(date, 7,8)
		dates[i] <- paste(y, m, d, sep="-")
	}
	dates
}

# one-period transfer function
transone <- function(x, period) {
	a <- 2/period
	am1 <- 1 - a
	y <- NA 
	for (t in 2:(length(x))) {
		 y[t] <- am1*x[t-1] + a*x[t] 
	}
	y
}


# MACD 
MACD <- function(x, buyperiod=c(8,17,9), sellperiod=c(12,25,9)) {
		
		# buy signal
		sta <- ema(x,buyperiod[1])
		lta <- ema(x,buyperiod[2])
		buydiff <- sta - lta
		buydiffema <- ema(buydiff, buyperiod[3])
									
		# sell signal
		sta <- ema(x,sellperiod[1])
		lta <- ema(x,sellperiod[2])
		selldiff <- sta - lta
		selldiffema <- ema(selldiff, buyperiod[3])
		
		#list("buydiff"=buydiff,   "buydiffema"=buydiffema,
		#	 "selldiff"=selldiff, "selldiffema"=selldiffema)
		ret <- as.data.frame(cbind(buydiff, buydiffema, selldiff, selldiffema))
		ret
}

# buy/sell signal from MACD
MACDsig <- function(x, startN, buyperiod=c(8,17,9), sellperiod=c(12,25,9)) {
	macdOut <- MACD(x, buyperiod, sellperiod)
	
	buy <- (macdOut$buydiff > macdOut$buydiffema)
	sell <- (macdOut$selldiff < macdOut$selldiffema)
	n <- length(x)
	sig <- rep(NA, startN)
	
	for(i in (startN+1):n) {
		sig[i] <- ifelse(buy[i-1]==FALSE && buy[i]==TRUE, TRUE, sig[i-1])
		sig[i] <- ifelse(sell[i-1]==FALSE && sell[i]==TRUE, FALSE, sig[i]) 				
	}	
	ret <- as.data.frame(cbind(sig, macdOut$buydiff, macdOut$buydiffema))
	ret$sig <- ifelse(ret$sig == 1, TRUE, FALSE)
	names(ret) <- c("sig", "buydiff", "buydiffema")
	ret
}

# subtract a year from any date format
subnyear <- function(date="1/2/3", n=1) {
	
	intflag <- F

	# transform YYYYMMDD to 1/2/3 format
	if(is.numeric(date[1])) {
		intflag <- T
		date <- YYYYMMDDtoDate(date)
	}

	if(substr(date, 2,2)=="/") {
		month <- substr(date, 1,1)
		daystart <- 3
	} else {
		month <- substr(date, 1,2)
		daystart <- 4
	}
	
	if(substr(date, daystart+1, daystart+1)=="/") {
		day <- substr(date, daystart, daystart)
		yrstart <- daystart+2
	} else {
		day <- substr(date, daystart, daystart+1)
		yrstart <- daystart+3
	}
	
	year <- as.integer(substr(date, yrstart, 10))
	
	# convert to 4 digit year.  we assume < 20 means it's a 2k+ date
	year <- ifelse(year < 20,  year+2000, year)
	year <- ifelse(year < 100, year+1900, year) 

 	# now do the subtraction
	year <- year - n
	
	newdate <- paste(month, day, year, sep="/")

	if(intflag) {
		newdate <- dateToYYYYMMDD(newdate)
	}

	return(newdate)
}


getYear <- function(date=20030401) {
	date <- as.integer(date)
	yr <- as.integer(substring(date, 1, 4))
	return(yr)
}


# you call also call this with negative numbers for subtractions
# if you add or subtract more than 12 months, the results will be screwy due to not having
# enough stuff in the year-wrapping logic
addnmonths <- function(date="1/2/3", n=1) {

	newdate <- NA
	intflag <- F

	# transform YYYYMMDD to 1/2/3 format
	if(is.numeric(date[1])) {
		intflag <- T
		date <- YYYYMMDDtoDate(date)
	}

	for(i in 1:length(date)) {
	
		if(substr(date[i], 2,2)=="/") {
			month <- substr(date[i], 1,1)
			daystart <- 3
		} else {
			month <- substr(date[i], 1,2)
			daystart <- 4
		}
	
		if(substr(date[i], daystart+1, daystart+1)=="/") {
			day <- substr(date[i], daystart, daystart)
			yrstart <- daystart+2
		} else {
			day <- substr(date[i], daystart, daystart+1)
			yrstart <- daystart+3
		}
		
		year <- as.integer(substr(date[i], yrstart, 10))	
		month <- as.integer(month)
		month <- month + n
	
		if(month>=13) {
			month <- month - 12
			year <- as.integer(substr(date[i], yrstart, 10))
			year <- year + 1
			if(year==100) year <- 2000
		}	

		if(month<=0) {
			month <- 12 + month
			year <- as.integer(substr(date[i], yrstart, 10))
			year <- year - 1
			if(year==-1) year <- 1999
		}
	
		if(year < 10) year <- paste("0", year, sep="")
	
		# if you pass in an invalid calendar date[i] in a query, the query will fail
		# so you have to avoid creating them
		if(month==2) {
			if(day > 28) day <- 28
		} 
		else if(month==4 || month==6 || month==9 || month==11) {
			if(day > 30) day <- 30
		}

		newdate[i] <- paste(month, day, year, sep="/")
		
	}

	if(intflag) {
		newdate <- dateToYYYYMMDD(newdate)
	}

	return(newdate)
}

addnweeks <- function(date=YYYYMMDD,n=1) {

        leapyears <- seq(1980, 2096, 4)
                n <- n*7
                Y <- as.numeric(substr(date,1,4))
                leapyear <- ifelse(is.element(Y,leapyears),T,F)
                M <- as.numeric(substr(date,5,6))
                D <- as.numeric(substr(date,7,8))
                D <- D + n

        while(D > 28) {

                if(is.element(M,c(4,6,9,11))) {
                        if(D > 30) {
                                M <- M+1
                                D <- D-30
                        } else {
                                Y <- Y
                                M <- ifelse(M < 10,paste("0",M,sep=""),M)
                                D <- ifelse(D < 10,paste("0",D,sep=""),D)
                                res <- as.integer(paste(Y,M,D,sep=""))
                return(res)
                        }
                }

                if(is.element(M,c(2))) {
                        if(leapyear) {
                                if(D > 29) {
                                        M <- M+1
                                        D <- D-29
                                }
                        } else {
                                if(D > 28) {
                                        M <- M+1
                                        D <- D-28
                                } else {
                                Y <- Y
                                M <- ifelse(M < 10,paste("0",M,sep=""),M)
                                D <- ifelse(D < 10,paste("0",D,sep=""),D)
                                res <- as.integer(paste(Y,M,D,sep=""))
                return(res)
                                }
                        }
                }
                if(is.element(M,c(1,3,5,7,8,10,12))) {
                        if(D > 31) {
                                M <- M+1
                                D <- D-31
                                        if(M > 12) {
                                        M <- 1
                                        Y <- Y + 1
                                        leapyear <- ifelse(is.element(Y,leapyears),T,F)
                                        }
                        } else {
                                Y <- Y
                                M <- ifelse(M < 10,paste("0",M,sep=""),M)
                                D <- ifelse(D < 10,paste("0",D,sep=""),D)
                                res <- as.integer(paste(Y,M,D,sep=""))
                return(res)
                        }
                }
        }
        Y <- Y
        M <- ifelse(M < 10,paste("0",M,sep=""),M)
        D <- ifelse(D < 10,paste("0",D,sep=""),D)
        res <- as.integer(paste(Y,M,D,sep=""))
return(res)
}

bopm <- function(date) {
	beginningOfPriorMonth(date)
}


beginningOfPriorMonth <- function(date="1/2/3") {

	intflag <- F

	# transform YYYYMMDD to 1/2/3 format
	if(is.numeric(date[1])) {
		intflag <- T
		date <- YYYYMMDDtoDate(date)
	}
	
	if(substr(date, 2,2)=="/") {
		month <- substr(date, 1,1)
		daystart <- 3
	} else {
		month <- substr(date, 1,2)
		daystart <- 4
	}
	
	if(substr(date, daystart+1, daystart+1)=="/") {
		day <- substr(date, daystart, daystart)
		yrstart <- daystart+2
	} else {
		day <- substr(date, daystart, daystart+1)
		yrstart <- daystart+3
	}

	month <- as.integer(month)
	year <- as.integer(substr(date, yrstart, 10))
	
	# convert to 4 digit year.  we assume < 20 means it's a 2k+ date
	year <- ifelse(year < 20,  year+2000, year)
	year <- ifelse(year < 100, year+1900, year) 

 	# now do the subtraction
	month <- month -1
	if(month==0) {
		month <- 12
		year <- year - 1
	}
		
	newdate <- paste(month, "1", year, sep="/")

	if(intflag) {
		newdate <- dateToYYYYMMDD(newdate)
	}

	return(newdate)
}

bom <- function(date="1/2/3") {

	intflag <- F

	# transform YYYYMMDD to 1/2/3 format
	if(is.numeric(date[1])) {
		intflag <- T
		date <- YYYYMMDDtoDate(date)
	}
	
	if(substr(date, 2,2)=="/") {
		month <- substr(date, 1,1)
		daystart <- 3
	} else {
		month <- substr(date, 1,2)
		daystart <- 4
	}
	
	if(substr(date, daystart+1, daystart+1)=="/") {
		day <- substr(date, daystart, daystart)
		yrstart <- daystart+2
	} else {
		day <- substr(date, daystart, daystart+1)
		yrstart <- daystart+3
	}

	month <- as.integer(month)
	year <- as.integer(substr(date, yrstart, 10))
	
	# convert to 4 digit year.  we assume < 20 means it's a 2k+ date
	year <- ifelse(year < 20,  year+2000, year)
	year <- ifelse(year < 100, year+1900, year) 
	
	newdate <- paste(month, "1", year, sep="/")

	if(intflag) {
		newdate <- dateToYYYYMMDD(newdate)
	}

	return(newdate)
}



# merge less frequent with more frequent data
# be sure to run mset back a bit farther than fullset for the best results
interpolationMerge <- function(fullsetdates, mdates, mvals) {

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


# merge less frequent with more frequent data when dates are not in common
# note that fdates and sdates need to be valid business-day dates: don't use
# this with month-end dates without first shifting to a business day
interpolationMergeNJ <- function(fdates, sdates, svals) {

	mindate <- min(fdates)
	maxdate <- max(fdates)
	superdates <- data.frame(validdates[validdates <= maxdate & validdates >= mindate])
	names(superdates) <- "date"

	superdates$svals <- interpolationMerge(superdates[,1], sdates, svals)

	# now decimate the series
	fdates <- data.frame(fdates)
	names(fdates) <- c("date")
	fset <- merge(superdates, fdates, all.x=F, all.y=T)
	
	return(fset$svals)

}

# merge two asynchronous time series, each of which may contain duplicate dates
# each set gets merged by field "date"
# returns a series with multiple dates preserved and with the most current information
# from the other dataset in each row
asynchronousMerge <- function(a, b) {
	
	m <- merge(a, b, all.x=T, all.y=T)	
	nc <- ncol(m)

	colvals <- m[1:2,] # trick to keep types right
	for(c in 1:nc) { colvals[2,c] <- NA }

	for(r in 1:nrow(m)) {
		for(c in 1:nc) {
			if(is.na(m[r,c])) {
				m[r,c] <- colvals[2,c]
			}
			else {
				colvals[2,c] <- m[r,c]
			}
		}
	}
	return (m)
}


# merge a pit set with asynchronous data
# pointdate must be the first column of b
asynchronousPitMerge <- function(pitset, b) {
	
	# for each row in b that is not a pointdate in pitset, we need to expand
	# the entire block 

	mindate <- min(pitset$pointdate)
	a <- b[b$pointdate <= mindate,]
	a <- a[nrow(a),]
	b <- b[b$pointdate > mindate,]

	if(nrow(b)==0) return(data.frame())

	opitset <- pitset

	for(r in 1:nrow(b)) {  # nrow(b)
		ps <- opitset[opitset$pointdate <= b$pointdate[r],]
		ps <- ps[ps$pointdate==ps$pointdate[nrow(ps)],]
		if(ps$pointdate[1]!=b$pointdate[r]) {
			ps$pointdate <- b$pointdate[r]
			pitset <- rbind(pitset, ps)
		}		
	}
	pitset <- pitset[order(pitset$pointdate, pitset$datadate),]
	row.names(pitset) <- 1:nrow(pitset) # auto-names get very long

	m <- merge(pitset, rbind(a,b), all.x=T, all.y=T)
	
	colvals <- rep(NA, ncol(m))
	for(r in 1:nrow(m)) {
		for(c in 2:ncol(m)) {
			if(is.na(m[r,c])) {
				m[r,c] <- colvals[c]
			}
			else {
				colvals[c] <- m[r,c]
			}
		}
	}
	
	if(nrow(a) > 0) {
		if(a$pointdate < mindate) m <- m[2:nrow(m),]
	}

	return (m)

}


formatFrame <- function(df, digits="X01234") {

	for(i in 1:ncol(df)) {
		d <- substr(digits, i, i)
		if(d != 'X') {
			d <- as.integer(d)
			df[,i] <- as.character(format(round(df[,i], d), nsmall=d))		 
		}
	}

	return(df)

}


# returns a data frame with no rows	
getDataFrame <- function(names=c("col1", "col2"), oneRow=F) {
	a <- data.frame(t(rep(NA,length(names))))
	names(a) <- names
	if(!oneRow) {
		a <- a[-1,]
	}
	return(a)
}


# used to make monthly pit data into a quarterly series by taking
# the last row that has a particular datacqtr only
# this only works with a ds that contains a datacqtr column, of course!
decimateToQuarters <- function(ds) {
	nds <- ds[1,]
	nds <- nds[-1,]

	curqtr <- ds[1,"datacqtr"]
	ndsrows <- nrow(ds)
	n <- 1
	while(n <= ndsrows) {
		if(ds[n,"datacqtr"]!=curqtr) {
			nds <- rbind(nds, ds[n-1,])
			curqtr <- ds[n, "datacqtr"]
		}
		n <- n + 1				
	}
	if(ds[ndsrows,"datacqtr"]!=ds[ndsrows-1,"datacqtr"]) {
		nds <- rbind(nds, ds[ndsrows,])
	}				
	return(nds)	
}


##
# pass in vectors of gvkey and iid, plus dates, and this returns
# a vector of portfolio returns, one per date in which the
# portfolio is not reset to equal weights at each period.
#
# returns a time series of returns and a final weight vector
dailySeriesRetcomp <- function(seccodes=c(46244, 61501),
				    		   startdate=20100607, enddate=20100806,
							   w=c(0.5, 0.5)) {

	days <- validdates[validdates >= startdate & validdates <= enddate]
	n <- length(days)-1
	pret <- NA	
	w <- ifelse(is.na(w), 0, w)

	for(i in 1:n) {
		
		rraw <- totalRetcomp(gvkeys, days[i], days[i+1])
		
		wn <- w * ifelse(is.na(rraw), 1, rraw)
		wn <- wn / sum(wn, na.rm=T)

		pret[i] <- sum(w * ifelse(is.na(rraw), 1, rraw))

		w <- wn

	}
	
	return(list(r=pret, w=wn))

}



##
# Computes total returns (including dividends)
# and gives this number as a gross return
#
# note: these dates must be market dates
#
totalRetcomp <- function(seccodes=c(36799, 29629), startdate=20100607, enddate=20100806,
						ignoreShares=F) {

	returns <- NA
	vop <- validdates[validdates < startdate]
	n <- length(vop)
	start_lagwk <- vop[n-50]
	vop <- validdates[validdates < enddate]
	n <- length(vop)
	end_lagwk <- vop[n-50]

	s2 <- data.frame(cbind(seccodes, 1:length(seccodes)))
	names(s2) <- c("seccode", "origorder")
	s2 <- merge(s2, mergeable_dsinfocodes, all.x=T, all.y=F)
	s2 <- s2[order(s2$origorder),]
	dsinfocodes <- s2$dsinfocode

	for(i in 1:length(dsinfocodes)) {
		
		if(is.na(dsinfocodes[i])) {
			returns[i] <- NA
			next
		}

		query <- paste("select to_char(marketdate,'YYYYMMDD') as mktdate, ",
						"open_, high, low, close_, volume ",
						"from ds2primqtprc where infocode=", dsinfocodes[i], " ",
						"and marketdate > to_date(", start_lagwk, ",'YYYYMMDD') ",
						"and marketdate <= to_date(", startdate,  ",'YYYYMMDD') ",
						 sep="")
		dsb <- sq(query)
		dsb <- dsb[!is.na(dsb$close_),]
		
		query <- paste("select to_char(marketdate,'YYYYMMDD') as mktdate, ",
						"open_, high, low, close_, volume ",
						"from ds2primqtprc where infocode=", dsinfocodes[i], " ",
						"and marketdate >  to_date(", end_lagwk, ", 'YYYYMMDD') ", 
						"and marketdate <= to_date(", enddate,",'YYYYMMDD')",
						 sep="")
		dse <- sq(query)
		dse <- dse[!is.na(dse$close_),]

		if(nrow(dsb) > 0 && nrow(dse) > 0) {
			
			ds <- rbind(dsb[nrow(dsb),], dse[nrow(dse),])			

			names(ds) <- c("date", "open", "high", "low", "close", "volume")

			query <- paste("select to_char(effectivedate,'YYYYMMDD') as edate, divrate ",
						"from ds2div ",
						"where infocode= ", dsinfocodes[i], " ",
						"and effectivedate >= to_date(",startdate,",'YYYYMMDD') ",
						"and effectivedate < to_date(",enddate,",'YYYYMMDD')", sep="")
			div <- sq(query)

			if(nrow(div)>0) {
				# sum dividends with the same date
				divd <- div
				divd$divrate <- NA
				divd <- unique(divd)
				for(j in 1:nrow(divd)) {
					divd$divrate[j] <- sum(div[div$edate==divd$edate[j], "divrate"], na.rm=T)
				}

				names(divd) <- c("date", "divrate")
				ds <- merge(ds, divd, all.x=T, all.y=T, by=c("date"))
			} else {
				ds$divrate <- 0
			}
			
			# merge in adjfactors as necessary
			query <- paste("select to_char(adjdate,'YYYYMMDD') as adate, ",
						   "cumadjfactor as adjfac ",
						   "from ds2adj where infocode=", dsinfocodes[i], " ",
						   "and adjtype=2", sep="")
			da <- sq(query)
	
			if(nrow(da)>0) {
				names(da) <- c("date", "adjfac")
				ds <- merge(ds, da, all.x=T, all.y=T, by=c("date"))
	
				for(j in 2:nrow(ds)) {
					ds$adjfac[j] <- ifelse(is.na(ds$adjfac[j]), ds$adjfac[j-1], ds$adjfac[j])
				}

			} else {
				ds$adjfac <- 1
			}			

			ds$close <- ds$close * ds$adjfac
			ds$divrate <- ds$divrate * ds$adjfac
			n <- nrow(ds)

			div <- sum(ds$divrate, na.rm=T)
			closes <- ds[!is.na(ds$close), "close"]
			p0 <- closes[1]
			p1 <- closes[2]

			returns[i] <- (p1 + div) / p0 
		} 
		else {
			returns[i] <- NA
		}
		
	}
	
	returns

}




# note: these dates must be market dates
# you should divide the result by the number of years between startdate and enddate
# mode can be total, rated, or norm
# total is everything, including shares and special dividends included
# rated is ft interactives estimate of dividends for next year based on last year
# norm is regular dividends only, no shares or special divs included
# rated will fall back to norm
yieldcomp <- function(gvkeys=c(6066, 100080), iids=c('01', '90'),
				    startdate='2006-03-21', enddate='2007-03-21', tickers=NA,
					mode="total") {

	startdate <- ifelse(is.numeric(startdate), YYYYMMDDToDashed(startdate), startdate)
	enddate   <- ifelse(is.numeric(enddate), YYYYMMDDToDashed(enddate), enddate)

	startdate <- ifelse(startdate=='2004-09-06', '2004-09-07', startdate)
	enddate <- ifelse(enddate=='2004-09-06', '2004-09-07', enddate)
 
	yields <- NA

	# get rid of tickers if that's what they gave us:
	if(!is.na(tickers)) {
		sids <- getDataFrame(c("tic", "gvkey", "iid", "cusip"))
		for(tic in tickers) {
			sids <- rbind(sids, getStockId(tic))
		}
	} else {
		sids <- data.frame(cbind(gvkeys, iids))
		names(sids) <- c("gvkey", "iid")
	}
	
	n <- nrow(sids)
	for(i in 1:n) {
			
		query <- paste("select datadate, prccd, ajexdi ",
					   "from compustat.sec_dprc ",
					   "where gvkey=", sids$gvkey[i], " ",
					   "and iid='", sids$iid[i], "' ",
					   "and datadate='", enddate, "' ", sep="")
		
		res <- mq(query)

		if(nrow(res)==1) {

			div <- 0
			# now get the dividends

			query <- paste(	"select d.datadate, divx, cheqv, ajexdi, ",
							"divd, divop, divsp, dvrated ",
				   			"from compustat.sec_divid d, compustat.sec_dprc p ",
				   			"where d.gvkey=p.gvkey ",
							"and d.iid = p.iid ",
							"and d.datadate = p.datadate ",
							"and d.gvkey=", sids$gvkey[i], " ",
							"and d.iid='", sids$iid[i], "' ",
				    		"and d.datadate >= '", startdate, "' ",
							"and d.datadate < '", enddate, "' ", sep="")

			divs <- mq(query)

			if(nrow(divs)>0) {

				# try to do the best we can with the mode they want...
				# data is funky, fallback on std method when necesary
				fallback <- F

				if(mode=='rated') {
					lr <- nrow(divs)
					div <- divs$dvrated[lr]/divs$ajexdi[lr]
					fallback <- ifelse(is.na(div), T, fallback)
				} else if(mode=='norm' || fallback) {
					if(!all(is.na(c(divs$divd, divs$divop)))) {
						div  <- sum(divs$divd/divs$ajexdi, na.rm=T) +
								sum(divs$divop/divs$ajexdi, na.rm=T)
					} else {
						fallback <- T
					}
				} else {
					fallback <- T
				}
					
				if(fallback) {				
					div <- 	sum(divs$divx/divs$ajexdi, na.rm=T) +
							sum(divs$cheqv/divs$ajexdi, na.rm=T)
				}
			}
			yields[i] <- div / res$prccd
		} 
		else {
			yields[i] <- NA
		}
	}

	yields
}

# takes two sets... before and after.  returns surviving percent of before set in after set
survcomp <- function(before, after) {
	lost <- setdiff(before, after)
	surv <- 1 - (length(lost) / length(before))
	return(surv)
}


# Crazy %K factor
percentK <- function(sid=NA, asof=20051205, n=2, tic=NA) {

	if(is.na(sid) && !is.na(tic)) { sid <- getStockId(tic) }

	asof <- getBusinessDayBackwards(asof)
	
	dates <- data.frame(cbind(validdates, 1:length(validdates)))
	names(dates) <- c("date", "idx")
	dates$date <- as.integer(dates$date)
	idx <- dates[dates$date==asof, "idx"]	
	fromdate <- dates$date[idx-n+1]

	ds <- getDailyData(sid, c("prccd", "prchd", "prcld", "ajexdi"), fromdate, asof) 
	if(nrow(ds) != n) return(NA)

	ds$prccd <- ds$prccd / ds$ajexdi
	ds$prchd <- ds$prchd / ds$ajexdi
	ds$prcld <- ds$prcld / ds$ajexdi

	nr <- nrow(ds)
	recent_close <- ds$prccd[nr]
	lowest_low <- min(ds$prcld, na.rm=T)
	highest_high <- max(ds$prchd, na.rm=T)
	
	percent_k <- (recent_close - lowest_low) / (highest_high - lowest_low)

	return(percent_k)
	
}


# MA deviation percent
MAdeviation <- function(sid, asof=20051205, n=10, tic=NA) {

	if(!is.na(tic)) { sid <- getStockId(tic) }

	asof <- getBusinessDayBackwards(asof)

	dates <- data.frame(cbind(validdates, 1:length(validdates)))
	names(dates) <- c("date", "idx")
	dates$date <- as.integer(dates$date)
	idx <- dates[dates$date==asof, "idx"]	
	fromdate <- dates$date[idx-(n*2)] # back to remove MA startup

	ds <- getDailyData(sid, c("prccd", "ajexdi"), fromdate, asof) 
	if(nrow(ds)!=(n*2)+1) return(NA)

	ds$prccd <- ds$prccd / ds$ajexdi

	ds$ma10 <- ma(ds$prccd, n)
	
	nr <- nrow(ds)	
	percent_deviation <- (ds$prccd[nr] / ds$ma10[nr])
	
	return(percent_deviation)

}

getLatestNonNAData <- function(ds, cols){
        for(i in 1:length(cols)){
                ds <- ds[!is.na(ds[cols[i]]),]
        }
        return(ds[nrow(ds),])
}

processNonNADataCodes <- function(ds, item_dc, smooth, NAQtrs, Pit=F) {

	item <- substr(item_dc, 1, (nchar(item_dc)-3))

	if(Pit){
		if(any(names(ds)=="pointdate")) {
			for(k in unique(ds$pointdate)) {
				ds[ds$pointdate==k,] <-
					processPitNonNADataCodes(ds[ds$pointdate==k,], item_dc, smooth, NAQtrs)
			}
		}else{ # for Pit Unrestated
			ds <- processPitNonNADataCodes(ds, item_dc, smooth, NAQtrs)
        }

    } else {	

		if(all(is.na(ds[,item_dc]))){
			return(ds)
		}

		# An NA datacode on the first row for fqtrs {2,4} can be regular/semianual/Anual,
		# there is no way to know which one,so we NA the its data item.			
		if( (ds[1,"fqtr"]==2) || (ds[1,"fqtr"]==4) ){
			ds[1, item] <- NA
		}

		ds <- adjustDataCode(ds, item_dc)

		if(smooth) {				
			ds <- smoothDataItem(ds,item_dc)
		}

		if(NAQtrs){
			ds <- adjustForDCTransitions(ds, item_dc, NAQtrs)
		}			

	}
	
	return(ds)
}

smoothDataItem <- function(ds,item_dc){
	
    item<-substr(item_dc,1,nchar(item_dc)-3)
	row.names(ds)<-1:nrow(ds)
	for(datacode in 2:3){
		idx<-NA
		if(datacode ==2){
			dscond <- ds[!is.na(ds[,item_dc]) & (ds[,item_dc]== 2) & (ds$fqtr==2 | ds$fqtr==4),]
			if(nrow(dscond))
				idx<-as.integer(row.names( dscond ))
			smFactor<-2	
		}else{
			dscond<-ds[!is.na(ds[,item_dc]) & (ds[,item_dc]== 3) & (ds$fqtr==4),]
			if(nrow(dscond))
				idx<-as.integer(row.names( dscond )) 
			smFactor<-4	
		}
		idx<-idx[!is.na(idx)]
		if(length(idx) >0){
			for(i in 1:length(idx) ){
				lastIdx<-idx[i]
				ds[lastIdx,item]<-ds[lastIdx,item]/smFactor
				for(k in 1:(smFactor-1)){
					if((lastIdx-k)>0 && (ds[lastIdx-k,"fqtr"]== (ds[lastIdx,"fqtr"] - k)) ){
						ds[lastIdx-k,item]<-ds[lastIdx,item]
					}
				}
			}
		}
	}
	return(ds)
}



adjustDataCode <- function(ds, item_dc) {

	item <- substr(item_dc, 1, nchar(item_dc)-3)
	row.names(ds) <- 1:nrow(ds)

	for(datacode in 2:3) {

		idx <- NA
		if(nrow(ds[!is.na(ds[, item_dc]) & ds[, item_dc]==datacode,])) {
			idx <- as.integer(row.names(ds[!is.na(ds[, item_dc]) & ds[, item_dc]==datacode, ]))
		}

		idx <- idx[!is.na(idx)]
		if(length(idx) > 0) {
			for(i in 1:length(idx) ) {
				j <- idx[i]

				#check that the next quarter is actually current quarter +1 because sometimes some qtrs are missing 
				if((j+1) <= nrow(ds) && ds[j+1, "fqtr"]==(ds[j, "fqtr"]+1)) {		
					lastIdx <- j + 1
					ds[lastIdx, item_dc] <- datacode
				}
			}
		}
	}

	return(ds)
}	 



adjustForDCTransitions <- function(ds,item_dc,numQ,DEBUG=0){

	item<-substr(item_dc,1,nchar(item_dc)-3)
	ds[is.na(ds[,item_dc]),item_dc]<-0
	row.names(ds)<-1:nrow(ds)
	idx<-NA
# We check for a datacode change from the 4th qtr and the 1st qtr of next year that has another dc value and if any, we adjust for that.
	idx<-as.integer(row.names( ds[ ds$fqtr==4 ,]  ))
	if(length(idx) >0){
		for(i in 1:length(idx) ){
			k<- idx[i]+1
			if( (k<= nrow(ds)) && (ds$fqtr[k] == 1) && (ds[idx[i],item_dc] != ds[k,item_dc]) ){
				if(DEBUG)
					print(ds[idx[i],])
				if(DEBUG)
					print(ds[idx[i]+1,])
				for(j in 1:numQ){
					if(idx[i]-j >= 0)
						ds[idx[i]-j+1,item]<-NA
				}
			}
		}
	}

# We check for a datacode change from the 2th qtr  and the 3rd qtr that has another dc value and if any, we adjust for that.
	idx<-as.integer(row.names( ds[ ds$fqtr==2 ,]  ))
	if(length(idx) >0){
		for(i in 1:length(idx) ){
			k<- idx[i]+1
			if((k<= nrow(ds)) && (ds$fqtr[k] == 3) && (ds[idx[i],item_dc] != ds[k,item_dc]) ){
				if(DEBUG)
					print(ds[idx[i],])
				if(DEBUG)
					print(ds[idx[i]+1,])
				for(j in 1:2){
					if(idx[i]-j >= 0)
						ds[idx[i]-j+1,item]<-NA
				}
			}
		}
	}

	ds[ds[,item_dc]==0,item_dc]<-NA
			
	return(ds)
}	

processPitNonNADataCodes<-function(ds, item_dc, smooth, NAQtrs){

 	item <- substr(item_dc, 1, nchar(item_dc)-3)

	if(all(is.na(ds[,item_dc]))){
		return(ds)
	}

	if(smooth){				
		if( !(ds$fqtr[1] %% 2) && !is.na(ds[1,item_dc]) ){
				ds[1,item]<-ifelse(ds[1,item_dc]==2,ds[1,item]/2,ds[1,item]/4)
		}
		ds<-smoothDataItem(ds,item_dc)
	}

	if(NAQtrs){
		ds<-adjustForDCTransitions(ds,item_dc,NAQtrs)
	}			
	  return(ds)
}	

#Since there is no datacode for oancfq, we have to check it ourselves. cf=oancfq-lagn(oancfq,1) and then we check for all NA CFs and try to estimate their value using the data from the previous and future quarters.
smoothCashFlow<-function(ds){
	row.names(ds)<-1:nrow(ds)
	idx<-as.integer(row.names(ds[is.na(ds$cf),]))

	# if there is no NA cashflow to estimate
	if(!length(idx))
		return(ds)
	for( i in 1:length(idx)){
		k<-idx[i]
		ds$cf[k]<-computeCashFlow(ds,k)
	}
	return(ds)
}

computeCashFlow<-function(ds,idx){
	fqtr<-ds$fqtr[idx]
	cf<-NA
	if(fqtr==4 && is.na(ds$oancfq[idx]))
		return(NA)
		
	oa<-replicate(4,NA)
	for(i in 1:4){
		if(idx-fqtr+i <1)
			next
		if(idx-fqtr+i > nrow(ds))
			break
		if(ds$fqtr[idx-fqtr+i]==i)
			oa[i]<-ds[idx-fqtr+i,"oancfq"]
	}
	if(all(is.na(oa[1:3])) && !is.na(oa[4])){
		cf<- oa[4]/4
		return(cf)
	}
	if(!is.na(oa[2]) && !is.na(oa[4]) && is.na(oa[1]) && is.na(oa[3]) ){
		cf<-ifelse(fqtr<=2,oa[2]/2,oa[4]/2)
		return(cf)
	}

	if(all(is.na(oa)))
		return(NA)
	if(all(is.na(oa[1:fqtr]))){
		for(k in (fqtr+1):4){
			if(fqtr+1 > 4)
				break
			if(!is.na(oa[k])){
				cf<-oa[k]/k
				return(cf)
			}
		}
	}

	if(!is.na(oa[fqtr])){
		if(all(is.na(oa[1:(fqtr-1)]))){
			cf<-oa[fqtr]/fqtr
			return(cf)
		}
		for(k in 2:3){
			if(fqtr-k < 1)
				break
			if(is.na(oa[fqtr-k]))
				next
			cf<-(oa[fqtr]-oa[fqtr-k])/k
			return(cf)
		}
	}else{
		fqtru<-NA
		for(k in (fqtr+1):4){
			if(k > 4)
				break
			if(!is.na(oa[k])){
				fqtru<-k	
				break
			}
		}
	
		if(!is.na(fqtru)){
			for(k in (fqtr-1):1){
				if(k < 1)
					break
				if(!is.na(oa[k])){
					fqtrd<-k	
					break
				}
			}
			if(!is.na(fqtrd))
				cf<-(oa[fqtru]-oa[fqtrd])/(fqtru-fqtrd)
			else
				cf<-oa[fqtru]/fqtru
		}		
	}
	return(cf)

}


# needs ds with cshoq, ajexq, adrrq
# returns adjusted and pulled-down cshoq (as it's often annual)
cshoqAdjuster <- function(ds) {

	if(nrow(ds) < 2) return(ds$cshoq)

	# it turns out, after talking with S&P, that cshoq should not be adjusted by adrrq,
	# whereas cshoc (from daily table) should be

	ds$cshoq <- ds$cshoq * ds$ajexq
	for(i in 2:nrow(ds)) {
		ds$cshoq[i] <- ifelse(is.na(ds$cshoq[i]), ds$cshoq[i-1], ds$cshoq[i])
	}		

	# we're going to invalidate anything with shares less than 1M, as it causes really
	# strange numbers in ratios
	ds$cshoq <- ifelse(ds$cshoq < 1, NA, ds$cshoq)

	return(ds$cshoq)
}


##
# - Takes in any series of cash flow items in cumulative 
# 	form and returns the spot cash flows.
# - Deals properly with NA cash flows from semi-annual and annual reporters
#   by smoothing the cash flow out over the various quarters
#
deaccumulateCashFlows <- function(cf, fqtr) {

	ds <- data.frame(cbind(cf, fqtr))
	
	# first of all, we're not going to deal with any lead-in partial years
	ds$idx <- 1:nrow(ds)
	fourthQtrRows = ds[ds$fqtr==4, "idx"]
	if(length(fourthQtrRows)==0) return (rep(NA, nrow(ds)))

	fr <- fourthQtrRows[1]
	numRowsCut <- ifelse(fr < 4, fr, max(0, fr-4))
	if(numRowsCut==nrow(ds)) return (rep(NA, nrow(ds)))

	# handle some special cases:
	numRowsCut <- ifelse(ds$fqtr[numRowsCut+1]==4, numRowsCut+1, numRowsCut)
	if(ds$fqtr[numRowsCut+1]!=1) { # something weird going on, probably bad data.
		return (rep(NA, nrow(ds)))
	}

	# do the lead-in cut
	ds <- ds[(numRowsCut+1):nrow(ds),]

	ds$scf <- NA 			# we will fill this in with smoothed cf

	for(i in 1:nrow(ds)) {

		if(ds$fqtr[i]==1) {
			ds$scf[i] <- ds$cf[i]
		}

		## second quarter adjustments
		if(ds$fqtr[i]==2) {

			if(ds$fqtr[i-1]==1 && !is.na(ds$cf[i])) { # adj possible then

				if(!is.na(ds$cf[i-1])) { # normal case
					ds$scf[i] <- ds$cf[i] - ds$cf[i-1]
				}
				else { # smooth out
					ds$scf[i-1] <- ds$cf[i]/2
					ds$scf[i]   <- ds$cf[i]/2
				}			
			}
		}

		## third quarter adjustments
		if(ds$fqtr[i]==3) {

			if(ds$fqtr[i-1]==2 && !is.na(ds$cf[i])) { # adj possible then

				if(!is.na(ds$cf[i-1])) { # normal case
					ds$scf[i] <- ds$cf[i] - ds$cf[i-1]
				}
				else if(!is.na(ds$cf[i-2]) && ds$fqtr[i-2]==1) {
					ds$scf[i-1] <- (ds$cf[i]-ds$cf[i-2]) / 2
					ds$scf[i]   <- (ds$cf[i]-ds$cf[i-2]) / 2
				}			
				else { # both prior quarters must be NA
					ds$scf[i-2] <- ds$cf[i] / 3
					ds$scf[i-1] <- ds$cf[i] / 3
					ds$scf[i]   <- ds$cf[i] / 3
				}

			}
			
		}

		## fourth quarter adjustments
		if(ds$fqtr[i]==4) {

			if(ds$fqtr[i-1]==3 && !is.na(ds$cf[i])) { # adj possible then

				if(!is.na(ds$cf[i-1])) { # normal case 					? ? # #
					ds$scf[i] <- ds$cf[i] - ds$cf[i-1]
				}
				else if(!is.na(ds$cf[i-2]) && ds$fqtr[i-2]==2) { ##   	? # NA  #
					ds$scf[i-1] <- (ds$cf[i]-ds$cf[i-2]) / 2
					ds$scf[i]   <- (ds$cf[i]-ds$cf[i-2]) / 2
				} 
				else if (!is.na(ds$cf[i-3]) && ds$fqtr[i-3]==1) { ##     	# NA NA  #
					ds$scf[i-2] <- (ds$cf[i]-ds$cf[i-3]) / 3
					ds$scf[i-1] <- (ds$cf[i]-ds$cf[i-3]) / 3
					ds$scf[i]   <- (ds$cf[i]-ds$cf[i-3]) / 3
				}			
				else { # all three prior quarters must be NA #			NA NA NA #
					ds$scf[i-3] <- ds$cf[i] / 4
					ds$scf[i-2] <- ds$cf[i] / 4
					ds$scf[i-1] <- ds$cf[i] / 4
					ds$scf[i]   <- ds$cf[i] / 4
				}
			}
		}
	}
	
	#return(ds)
	return(c(rep(NA, numRowsCut), ds$scf))
}

#
# useful test vectors for deaccumulateCashFlows
#cf <- c(1,	2,	3,	4,
#		1,	2,	3,	NA,
#		1,	2,	NA,	4,
#		1,	2,	NA, NA,
#		1,	NA,	3,	4,
#		1,	NA,	3,	NA,
#		1,	NA,	NA,	4,
#		1,	NA,	NA,	NA,
#		NA,	2,	3,	4,
#		NA,	2,	3,	NA,
#		NA,	2,	NA,	4,
#		NA,	2,	NA, NA,
#		NA,	NA,	3,	4,
#		NA,	NA,	3,	NA,
#		NA,	NA,	NA,	4,
#		NA,	NA,	NA,	NA)
#
#fqtr <- rep(1:4, 16)
#

today <- function() {
	as.integer(format(Sys.time(), "%Y%m%d"))
}

yesterday <- function() {
	as.integer(system("date -d yesterday +%Y%m%d", intern=T))
}

writecsv <- function(ds, filename="Routput.csv") {
	write.table(ds, file=filename, quote=F, row.names=F, sep=",")
}

# compute cumulative product skipping over NA's
cumprodna <- function(x) {

	dfo <- data.frame(cbind(x,1:length(x)))
	names(dfo) <- c("x", "y")
	df <- dfo[!is.na(dfo$x),]
	df$x <- cumprod(df$x)
	names(df) <- c("xc", "y")
	df <- merge(df, dfo, by="y", all.x=T, all.y=T)
	return(df$xc)
}

robusto <- function(x, topchop=2.5, botchop=2.5) {

	if(length(x) <= 8) { return(x) } # screw it

	if(!is.na(topchop)) {
		topLim <- as.numeric(quantile(x, probs=((100-topchop)/100), na.rm=T))
		x <- ifelse(x < topLim, x, NA)
	}
	
	if(!is.na(botchop)) {
		botLim <- as.numeric(quantile(x, probs=(botchop/100), na.rm=T))	
		x <- ifelse(x > botLim, x, NA)
	}
	
	return(x)

}

robustMean <- function(x, w, topchop=2.5, botchop=2.5) {
	x <- robusto(x, topchop, botchop)
	w <- ifelse(is.na(x), NA, w)
	m <- weighted.mean(x, w, na.rm=T)
	return(m)
}

robustSum <- function(x, w, p=NA, topchop=2.5, botchop=2.5) {
	x <- robusto(x, topchop, botchop)
	w <- ifelse(is.na(x), NA, w)
	w <- w / sum(w, na.rm=T)
	if(length(p)>1) {  # woppy mode
		w <- w / p
	}
	s <- sum(w*x, na.rm=T)
	return(s)	
}

# give me sum(a*w)/sum(b*w)
robustDivide <- function(a, b, w, topchopa=1.25, botchopa=1.25, topchopb=1.25, botchopb=1.25) {
	a <- robusto(a, topchopa, botchopa)
	b <- robusto(b, topchopb, botchopb)
	a <- ifelse(is.na(b), NA, a)
	b <- ifelse(is.na(a), NA, b)
	w <- ifelse(is.na(a) | is.na(b), NA, w)
	w <- w / sum(w, na.rm=T)
	r <- sum(w*a, na.rm=T) / sum(w*b, na.rm=T)
	return(r)
}

