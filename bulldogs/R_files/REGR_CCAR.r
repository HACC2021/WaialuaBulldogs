#-----	CCAR CUSTOM

run_ccar_regr <- function() {

dostepwise <<- function(lset = lset,lside=kdp_cols[y]) {
	flag <- ifelse(sum((abs(lset$coefficients[,3][-1])<TVAL))<2,FALSE,as.logical(sum((abs(lset$coefficients[,3][-1])<TVAL))))
	tscrs <- abs(lset$coefficients[,3][-1])
	while(flag) {
		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(row.names(lset$coefficients)[-1][order(tscrs)][-1],collapse=" + "),sep="")),testset))
		flag <- ifelse(sum((abs(lset$coefficients[,3][-1])<TVAL))<2,FALSE,as.logical(sum((abs(lset$coefficients[,3][-1])<TVAL))))
		tscrs <- abs(lset$coefficients[,3][-1])
	}
return(lset)
}

trythisone <<- function(frmla,testset,fwf,fwrsq) {
	wfbk <- fwf
	lset <- summary(lm(frmla,testset))
		if(is.na(lset$adj.r.squared) & !is.null(wfbk)) {
			lset <- summary(lm(as.formula(paste(lside,paste(wfbk,sep="",collapse=" + "),sep=" ~ ")),testset))
			return(list(lset,row.names(lset$coefficients)[-1],as.numeric(lset$adj.r.squared)))
		}
		if(is.na(lset$adj.r.squared) & is.null(wfbk))  {
			return(list(NULL,wfbk,fwrsq))
		}
		if(all(abs(lset$coefficients[,3][-1])>TVAL) & as.numeric(lset$adj.r.squared) > fwrsq) {
			return(list(lset,row.names(lset$coefficients)[-1],lset$adj.r.squared))
		}
		if(!all(abs(lset$coefficients[,3][-1])>TVAL)){
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

runthese <<- function(trythese,wf) {
	if(length(trythese)!=0) {
		for(xx in 1:length(trythese)){
			if(length(wf)==lmtr) {
				lmtr_flag <- TRUE
				lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
				wf_holder <- wf
				wf <- row.names(lset$coefficients)[-1][order(abs(lset$coefficients[-1,3]))][-1]
			}
			mf <- trythese[xx]
			if(length(unique(testset[!is.na(testset[,mf]),mf]))==1 | all(is.na(testset[,mf]))) next
			frmla <- as.formula(paste(lside," ~ ",paste(append(wf,mf),sep="", collapse=" + "),sep=""))
			wf_wrsq <- trythisone(frmla,testset,wf,wrsq)
			if(all(abs(wf_wrsq[[1]]$coefficients[,3][-1])>TVAL) & wf_wrsq[[3]]>wrsq) {
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




	base_formula <- c('VIXM','CREIDX_R1Y_NON','HOUSEIDX_R1Y_NON','DJIDX_R1Y_NON','CPI','BBBSPD','TSLOPE','UNEMP')
	large_co_facs <- c('CPIASIA','CPIEUR','CPIJPN','CPIUK','FXASIA','FXEUR','FXJPN','FXUK','RGDPASIA','RGDPEUR','RGDPJPN','RGDPUK')
	base_plus <- list(c('RGDP','NGDP'), c('UNEMP_D1Y_NON','UNEMP_D6M_NON','UNEMP_D2Y_NON'),c('PRIME','TREAS3MO','TREAS5YR','TREAS10Y','MORT'),
							c('PRIME_D1Y_NON','TREAS3MO_D1Y_NON','TREAS5YR_D1Y_NON','TREAS10Y_D1Y_NON','MORT_D1Y_NON'), c('RDISINC','NDISINC'))

	gids <- fq(paste("select top 1 g.market_cap, k.gsubind, k.class, k.country as loc from MFSQ.dbo.KRIS_COMPANY_TICKER k
		left join MFSQ.dbo.CCAR_LARGE_CAPS g on k.gvkey=g.gvkey where k.gvkey='",gvkey,"'",sep=""))

	if(nrow(gids)==0) {
		gids <- fq(paste("select 1000 as market_cap, s.gic as gsubind, c.class, 'US' as loc, gic.gicdesc as conm 
		from MFSQ.dbo.KRIS_STAT_MAST s join MFSQ.dbo.GICCD gic on gic.giccd=s.gic 
		join MFSQ.dbo.KRIS_COMPANY_TICKER c on c.gvkey=s.stkey where s.stkey='",gvkey,"'",sep=""))
	}
	
	if(nrow(gids)==0) {
		gids <- data.frame(matrix(ncol=5,nrow=1))
			names(gids) <- c('market_cap','gsubind','class','loc','conm')
	}

		gids$market_cap <- ifelse(is.na(gids$market_cap),0,gids$market_cap)
		gids$gsubind <- ifelse(is.na(gids$gsubind),1111,gids$gsubind)
		gids$class <- ifelse(is.na(gids$class),'custom',gids$class)
		gids$loc <- ifelse(is.na(gids$loc),'',gids$loc)
		gids$conm <- ifelse(is.na(gids$conm),gvkey,gids$conm)

	if(gids$class=='SEC' | gids$class=='RAT') {
		fac_list <- list(base_formula,base_plus)
	} else if(gids$class=='PUB') {
		if((as.numeric(gids$market_cap)>15000 & !is.element(substr(gids$gsubind,1,4),c('4030','5510')) & gids$loc=='USA') | gids$loc!='USA') {
			fac_list <- list(base_formula,base_plus,large_co_facs)
		} else {
			fac_list <- list(base_formula,base_plus)
		}
	} else if(gids$class=='SOV') {
		fac_list <- list(large_co_facs,base_plus,base_formula)
	} else {
		fac_list <- list(base_formula,base_plus)
	}

	lmtr_flag = FALSE
	wf = NULL
	wrsq = 0

#-----THE WHEEL OF FORTUNE
	lwf = wf
	qqq = 1
	TRYS=10
		while(qqq) {
		#-----DOMESTIC MACRO-FACTORS
			trythese <- unlist(fac_list[[1]])
			trythese <- trythese[!is.element(trythese,wf)]
			trythese <- trythese[is.element(trythese,facs)]
			wf_wrsq <- runthese(trythese,wf)
				wf <- wf_wrsq[[2]]
				wrsq <- wf_wrsq[[3]]
		#-----GROUPED MACRO-FACTORS
			for(x in 1:length(fac_list[[2]])) { 
				wfs <- NULL
				wrsqs <- NULL
				wts <- NULL
				trythese <- fac_list[[2]][x][[1]]
				trythese <- trythese[is.element(trythese,facs)]
				if(length(trythese)>0) {
					frmlas <- NULL
					if(length(wf)==lmtr) {
						lmtr_flag = TRUE
						lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
						wf_holder <- wf
						wf <- row.names(lset$coefficients)[-1][order(abs(lset$coefficients[-1,3]))][-1]
					}
					for(thisone in trythese) frmlas <- append(frmlas,as.formula(paste(lside," ~ ",paste(append(wf[!is.element(wf,trythese)],thisone),sep="",collapse=" + "),sep="")))
					for(xx in 1:length(frmlas)) {
						mf <- trythese[xx]
						if(length(unique(testset[,mf]))==1 | all(is.na(testset[,mf]))) {
							wfs[xx] <- NA
							wrsqs[xx]<- NA
							wts[xx] <- FALSE
						} else {
							wf_wrsq <- trythisone(frmlas[[xx]],testset,wf,wrsq)
							wfs[xx] <- list(wf_wrsq[[2]])
							wrsqs[xx]<- ifelse(is.null(wf_wrsq[[3]]),0,wf_wrsq[[3]])
							wts[xx] <- ifelse(is.null(wf_wrsq[[3]]),FALSE,all(abs(wf_wrsq[[1]]$coefficients[,3][-1])>TVAL))
						}
					}
					if(sum(wts)) {
						wfs <- wfs[wts]
						wrsqs <- wrsqs[wts]
						if(wrsqs[order(wrsqs,decreasing=T)][1]>wrsq){
							wf <- wfs[order(wrsqs,decreasing=T)][[1]]
							wrsq <- wrsqs[order(wrsqs,decreasing=T)][1]
						} else {
							if(lmtr_flag) wf <- wf_holder
						}
					}
				}
			}
		#-----INTERNATIONAL & LARGE COMPANY FACS
			if(length(fac_list)==3){
				trythese <- unlist(fac_list[[3]])
				trythese <- trythese[!is.element(trythese,wf)]
				trythese <- trythese[is.element(trythese,facs)]
				if(length(tryhthese)>0){
					wf_wrsq <- runthese(trythese,wf)
					wf <- wf_wrsq[[2]]
					wrsq <- wf_wrsq[[3]]
				}
			}	
		if(qqq>1) if(all(is.element(wf,lwf)) & all(is.element(lwf,wf))) qqq=FALSE
		if(qqq==TRYS) qqq=FALSE	
		if(qqq) qqq=qqq+1
		if(qqq>1) lwf=wf
		}

	if(length(wf)>0) {
		this_wf <- paste(wf,sep="",collapse="+")
	} else { 
		this_wf <- NULL
	}
	names(this_wf) <- paste('algo_id_',algo_id,sep="")

return(this_wf)
}
