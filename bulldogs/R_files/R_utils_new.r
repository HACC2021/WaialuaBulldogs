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
	gfacs <- row.names(lset$coefficients)[-1]
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

frm_call <- function(lside=lside,fax=colz,abs_t=T ) {
			if(length(fax)<1) {
				writeLog(reg_id,gvkey,NA,'E','fail_frm_call')	
	return(list(NA,NA,NA,NA))
			}
			thistestset <- testset[,c(lside,fax)]
			keep_rows <- ''
			for(x in 1:nrow(thistestset)) if(!sum(is.na(thistestset[x,2:(ncol(thistestset))]))) keep_rows <- append(keep_rows,x)
			keep_rows <- keep_rows[-1]
			thistestset <- thistestset[keep_rows,]
			if(nrow(thistestset)<12) {
				writeLog(reg_id,gvkey,NA,'W','fail_min_rows_test')
	return(list(NA,NA,NA,NA))
			}

			if(length(fax)>nrow(thistestset)*.5) {
				fax <- fax[1:((nrow(thistestset)*.5)-1)]
			}
			x=as.data.frame(thistestset[,c(fax)])
				names(x) <- fax
			y=thistestset[,lside]
			frm_res <- try(frm(y=y,x=x,linkfrac='logit',type='1P',intercept=T,table=F,variance=T))
			if(is(frm_res,'try-error')) {
				writeLog(reg_id,gvkey,NA,'E','fail_frm_call')
	return(frm_res)
			} 
			
			intercept 	<-  frm_res$p[1]
			yhat 		<- frm_res$yhat
			num_obs		<- length(yhat)
			ll			<- sum(log(yhat)*y+(1-y)*log(1-yhat))
			ybar		<- mean(y)
			dof			<- length(frm_res$x.names[-1])
			AIC			<- -2*ll + 2*(length(frm_res$x.names))
			BIC			<- -2*ll + log(num_obs)*(length(frm_res$x.names))
			Tscores		<- (frm_res$p/sqrt(diag(frm_res$p.var)))[-1]
			if(abs_t==T) Tscores <- abs(Tscores)
			p_rsq		<- 1-ll/(num_obs*(ybar*log(ybar) + (1-ybar)*log(1-ybar)))
			pd_rsq		<- 1-(sum((y-yhat)^2)/sum((y-ybar)^2))
			pd_adj_rsq <-  1-((1-pd_rsq)*(num_obs-1) /(num_obs-dof-1))
			
			wf <- names(Tscores)
			wrsq <-  pd_adj_rsq

		return(list(Tscores,wf,wrsq,intercept,num_obs))
}

dostepwise <- function(wf_wrsq) {
	flag <- ifelse(sum(wf_wrsq[[1]]<TVAL)<2,FALSE,as.logical(sum(wf_wrsq[[1]]<TVAL)))
	while(flag) {
		mf_cols <- names(wf_wrsq[[1]][order(wf_wrsq[[1]],decreasing=F)])[-1]
		if(length(mf_cols)<1) flag=F
		wf_wrsq <- frm_call(lside,mf_cols)	
		flag <- ifelse(sum(wf_wrsq[[1]]<TVAL)<2,FALSE,as.logical(sum(wf_wrsq[[1]]<TVAL)))
		if(wf_wrsq[[1]]==1) return(wf_wrsq)
	}
return(wf_wrsq)
}

trythisone <- function(frmla,wf,wrsq) {
	wfbk <- wf
	wf_wrsq <- frm_call(lside,frmla)	
		thesefacs	<- wf_wrsq[[2]]
		thisrsq	<- wf_wrsq[[3]]
	
		if(is.na(thisrsq) & !is.null(wfbk)) {
			wf_wrsq <- frm_call(lside,wfbk)	
			return(wf_wrsq)
		}
		if(is.na(thisrsq) & is.null(wfbk))  {
			return(list(NULL,wfbk,wrsq,NULL))
		}
		if(all(wf_wrsq[[1]]>TVAL) & thisrsq > wrsq) {
			return(wf_wrsq)
		}
		if(!all(wf_wrsq[[1]]>TVAL)){
			wf_wrsq <- dostepwise(wf_wrsq)
			if(!is.na(wf_wrsq[[3]])) return(wf_wrsq)
		}
		if(!is.null(wfbk)) {
			wf_wrsq <- frm_call(lside,wfbk)	
			return(wf_wrsq)
		} else {
			return(list(NULL,wfbk,wrsq,NULL))
		}
}

runthese <- function(trythese,wf) {
	if(length(trythese)>1) {
		for(xx in 1:length(trythese)){
			if(length(wf)==lmtr) {
				lmtr_flag <- TRUE
				wf_wrsq <- frm_call(lside)
					wf_holder <- wf
					wf		<- wf_wrsq[[2]]
			}
			mf <- trythese[xx]
			if(length(unique(testset[!is.na(testset[,mf]),mf]))==1 | all(is.na(testset[,mf]))) next
			if(length(wf)==0 | is.null(wf)) { 
				wf <- mf
				next
			}
			frmla <- append(wf,mf)
			wf_wrsq <- trythisone(frmla,wf,wrsq)
				thesefacs <- wf_wrsq[[2]]
				thisrsq	<- wf_wrsq[[3]]

			if(all(wf_wrsq[[1]]>TVAL) & thisrsq>wrsq) {
				wf <- thesefacs
				wrsq <- thisrsq
			} else {
				if(lmtr_flag) wf <- wf_holder
			}
		}
	}
	if(!is.null(wf)){
		wf_wrsq <- frm_call(lside,wf)
		return(wf_wrsq)
	} else {
	return(wf_wrsq)
	}
}

tryharder <- function(thisdeep=2) {
	if(length(facs)<thisdeep) return(NA)
	frmla_pairs <- NULL
	frmla_pairs_best <- NULL
	frmla_pairs_rsqs <- NULL
	frmla_pairs_t <- NULL
	mfacpairs <- combn(facs,thisdeep)
	for(aa in 1:ncol(mfacpairs)) frmla_pairs[aa] <- paste(mfacpairs[1:thisdeep,aa][order(mfacpairs[1:thisdeep,aa])],sep="",collapse="+")
	frmla_pairs <- unique(frmla_pairs)
	for(aa in 1:length(frmla_pairs)) {
		wf_wrsq <- frm_call(lside,strsplit(frmla_pairs[aa],'\\+')[[1]],T)
		if(!is.na(wf_wrsq[[1]])) {
			frmla_pairs_best[aa] <- paste(wf_wrsq[[2]],sep="",collapse="+")
			frmla_pairs_rsqs[aa] <- wf_wrsq[[3]]
			frmla_pairs_t[aa] <- as.logical(sum(wf_wrsq[[1]]>TVAL))
		} else {
			frmla_pairs_best[aa] <- frmla_pairs[aa]
			frmla_pairs_rsqs[aa] <- 0
			frmla_pairs_t[aa] <- F
		}
	}
	frmla_pairs_best <- frmla_pairs_best[frmla_pairs_t]
	frmla_pairs_rsqs <- frmla_pairs_rsqs[frmla_pairs_t]
	this_wf <- frmla_pairs_best[order(frmla_pairs_rsqs,decreasing=T)][1]
return(this_wf)
}

writeLog <- function(reg_id, gvkey, reg_factor=NA, error_type='E', error_code='no_model_inputs') {
	thistime=format(Sys.time(),"%Y-%m-%d %H:%M:%S")
	query=gsub("'NA'","NULL",paste("insert mfsq.dbo.mfs_reg_error values('",
		thistime,"','",reg_id,"','",gvkey,"','",reg_factor,"','",error_type,"','",error_code,"')",sep=''))
	fq(query)
}