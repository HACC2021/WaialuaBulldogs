library(RODBC)
library(DBI)
library(gdata)
library(VIF)
library(fmsb)
library(gvlma)
library(lmtest)
library(car)
library(sandwich)
library(fBasics)
library(Rmisc)
library(frm)

Sys.setenv(R_HISTSIZE=999999)


if(!exists("tahiti")) {
	dbdb <- 'TAHITI'
	dbuser <- 'sa'
	dbpw <- 'Seaside364'
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

if(!exists("fiji")) {
	dbdb <- 'FIJI'
	dbuser <- 'sa'
	dbpw <- 'kamakura'
	dbConnectFiji <- function() {
		res <- odbcConnect(dbdb,dbuser,dbpw,interpretDot=F)
	if(res==-1)  { 
		paste("failed to connect to FIJI") 
	} else {
		assign('fiji',res,env=.GlobalEnv)
		invisible()
	}
	}
	dbConnectFiji()
}


sendQuery <- function(query="select name from sys.databases",dbh=fiji,as.is=T) {
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

fq <- function(query="select name from sys.databases order by name",dbh=fiji,as.is=T) {
	sendQuery(query,dbh,as.is)
}


kamaWriteTable <- function(ds, dblocation, append=T, dbh=fiji) {
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

fijiWriteTable <- function(ds,dblocation,append=T,dbh=fiji) {
	kamaWriteTable(ds,dblocation,append,dbh)
}


fix_collinearity <- function(facs)	{
	lset <- summary(lm(as.formula(paste(lside," ~ ",paste(facs,sep="",collapse="+"),sep="")),testset))
	if(as.logical(sum(as.logical(lset$aliased[-1])))) {

		gfac_holder <- row.names(lset$coefficients)[-1]
		bfacs <- facs[!is.element(facs,gfac_holder)]

			if(length(bfacs)==1) {
				lset <- summary(lm(as.formula(paste(lside,"~",paste(append(gfac_holder,bfacs),sep="",collapse="+"),sep="")),testset))
				bfacs <- gfac_holder[!is.element(gfac_holder,row.names(lset$coefficients)[-1])]
				gfacs <- row.names(lset$coefficients)[-1]
				if(length(bfacs)==1) bfacs <- bfacs[-1]
			}
			if(length(bfacs)>1){
				gfacs <- gfac_holder
				for(zz in 1:length(bfacs)) {
					lset <- summary(lm(as.formula(paste(lside,"~",paste(append(gfacs,bfacs[zz]),sep="",collapse="+"),sep="")),testset))
					if(!as.logical(sum(as.logical(lset$aliased[-1])))) gfacs <- row.names(lset$coefficients[-1,])
				}
				lset <- summary(lm(as.formula(paste(lside,"~",paste(gfacs,sep="",collapse="+"),sep="")),testset))
				if(as.logical(sum(as.logical(lset$aliased[-1])))) lset <- summary(lm(as.formula(paste(lside,"~",paste(gfac_holder,sep="",collapse="+"),sep="")),testset))
				gfacs <- row.names(lset$coefficients[-1,])
			}
		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(gfacs,sep="",collapse="+"),sep="")),testset))
	}
	y <- 1
	while(all(is.na(lset$coefficients[-1,3]))) {
		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(gfacs[1:(length(gfacs)-y)],sep="",collapse="+"),sep="")),testset))
		y <- y+1
	}
	gfacs <- row.names(lset$coefficients[-1,])
	return(gfacs)
}

bgtest <- function (formula, order = 1, order.by = NULL, type = c("Chisq","F"), data = list(), fill = 0)  {
    dname <- paste(deparse(substitute(formula)))
    if (!inherits(formula, "formula")) {
        X <- if (is.matrix(formula$x)) 
            formula$x
        else model.matrix(terms(formula), model.frame(formula))
        y <- if (is.vector(formula$y)) 
            formula$y
        else model.response(model.frame(formula))
    }
    else {
        mf <- model.frame(formula, data = data)
        y <- model.response(mf)
        X <- model.matrix(formula, data = data)
    }
    if (!is.null(order.by)) {
        if (inherits(order.by, "formula")) {
            z <- model.matrix(order.by, data = data)
            z <- as.vector(z[, ncol(z)])
        }
        else {
            z <- order.by
        }
        X <- as.matrix(X[order(z), ])
        y <- y[order(z)]
    }
    n <- nrow(X)
    k <- ncol(X)
    order <- 1:order
    m <- length(order)
    resi <- lm.fit(X, y)$residuals
    Z <- sapply(order, function(x) c(rep(fill, length.out = x), 
        resi[1:(n - x)]))
    if (any(na <- !complete.cases(Z))) {
        X <- X[!na, , drop = FALSE]
        Z <- Z[!na, , drop = FALSE]
        y <- y[!na]
        resi <- resi[!na]
        n <- nrow(X)
    }
    auxfit <- lm.fit(cbind(X, Z), resi)
    cf <- auxfit$coefficients
    vc <- try(chol2inv(auxfit$qr$qr) * sum(auxfit$residuals^2)/auxfit$df.residual)
#----- ADDED BY TED SPRADLIN 2015.12.03
if(is(vc,'try-error')) return(vc)
	
    names(cf) <- colnames(vc) <- rownames(vc) <- c(colnames(X), 
        paste("lag(resid)", order, sep = "_"))
    switch(match.arg(type), Chisq = {
        bg <- n * sum(auxfit$fitted^2)/sum(resi^2)
        p.val <- pchisq(bg, m, lower.tail = FALSE)
        df <- m
        names(df) <- "df"
    }, F = {
        uresi <- auxfit$residuals
        bg <- ((sum(resi^2) - sum(uresi^2))/m)/(sum(uresi^2)/(n - 
            k - m))
        df <- c(m, n - k - m)
        names(df) <- c("df1", "df2")
        p.val <- pf(bg, df1 = df[1], df2 = df[2], lower.tail = FALSE)
    })
    names(bg) <- "LM test"
    RVAL <- list(statistic = bg, parameter = df, method = paste("Breusch-Godfrey test for serial correlation of order up to", 
        max(order)), p.value = p.val, data.name = dname, coefficients = cf, 
        vcov = vc)
    class(RVAL) <- c("bgtest", "htest")
    return(RVAL)
}

get_kdpjc6 <- function(gvkey='006066') {
	query <- paste("select jc.gvkey, substring(convert(char,jc.data_date,112),1,6) as yearmo,
				(case jc.kdp_1mo when 0 then -11.5129 when 100 then 11.5129 else -log((1-jc.kdp_1mo/100.0)/(jc.kdp_1mo/100.0)) end ) as kdp_1mo,
				(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo,
				(case jc.kdp_1yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_1yr/100.0)/(jc.kdp_1yr/100.0))  end) as kdp_1yr,
				(case jc.kdp_5yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_5yr/100.0)/(jc.kdp_5yr/100.0))  end) as kdp_5yr
			from OKINAWA.kris.dbo.kdp_jc6 jc join OKINAWA.kris.dbo.month_end_dates d on d.gvkey=jc.gvkey and d.data_date=jc.data_date
			where jc.gvkey='",
			gvkey,"' order by yearmo",sep="")
	kdp_values <- tq(query)	
		kdp_values$kdp_1mo <- as.numeric(kdp_values$kdp_1mo)
		kdp_values$kdp_3mo <- as.numeric(kdp_values$kdp_3mo)
		kdp_values$kdp_1yr <- as.numeric(kdp_values$kdp_1yr)
		kdp_values$kdp_5yr <- as.numeric(kdp_values$kdp_5yr)
	return(kdp_values[,-1])	#drop gvkey column
}
