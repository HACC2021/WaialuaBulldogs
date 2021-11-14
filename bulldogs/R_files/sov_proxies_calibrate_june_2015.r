mydir <- ifelse(Sys.getenv('COMPUTERNAME') == 'TED-PC','/therocrates','/kamakura_r')
source(paste(mydir,"/kamakura_Rutils.r",sep=""))
timestamp()
#######################################################
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

gvkeys <- oq("select distinct stkey from kdps.dbo.stat_mast where gic is null and rating is null and meas_val=.5 order by stkey")[[1]]

for(yy in 2:3) {

	if(yy==3) {
		base_formula <- c('VIX','CREIDX','HOUSEIDX','DJIDX','CPI','BBBCORP','UNEMP')
		large_co_facs <- c('CPIASIA','CPIEUR','CPIJPN','CPIUK','FXASIA','FXEUR','FXJPN','FXUK','RGDPASIA','RGDPEUR','RGDPJPN','RGDPUK')
		base_plus <- list(c('RGDP','NGDP'), c('UNEMP_1YD','UNEMP_6MD','UNEMP_2YD'),c('TREAS3MO','TREAS10Y','MORT'), 
								c('TR3M_1YD','TR10Y_1YD','MORT_1YD'), c('RDISINC','NDISINC'))
		fac_list <- list(append(base_formula,large_co_facs),base_plus)
	}
	if(yy==2) {
		base_formula <-c('EUR_USD','IDXCHG_C','IDXCHG_E','IDXCHG_G','IDXCHG_J','IDXCHG_U','IDXSTD_C','IDXSTD_E','IDXSTD_G','IDXSTD_J','IDXSTD_U','JPY_USD','OILPRICE','TR10YR_C','TR10YR_E','TR10YR_G','TR10YR_J','TR10YR_U','UNEMPLOY')
		base_plus <- list(c('CSRET1YR','CSRET2YR','CSRET3YR','CSRET5YR','CSSTD2YR'),c('EUR_USD1','EUR_USD2','EUR_USD3'),c('GDP_10YR','GDP_1YR','GDP_2YR','GDP_3YR','GDP_5YR'),c('JPY_USD1','JPY_USD2','JPY_USD3'))
		fac_list <- list(base_formula,base_plus)
	}

	kdp_cols <- c('kdp_1mo','kdp_3mo','kdp_1yr','kdp_5yr')
	mfacs <- tq(paste("exec mfsq.dbo.get_macro_factors ",yy,sep=""))
	names(mfacs)[2:ncol(mfacs)] <- toupper(names(mfacs)[2:ncol(mfacs)])
	for(x in 2:ncol(mfacs)) mfacs[,x] <- as.numeric(mfacs[,x])

	if(yy==3) {
		mfacs$UNEMP_1YD <- mfacs$UNEMP - lagn(mfacs$UNEMP,12)
		mfacs$UNEMP_6MD <- mfacs$UNEMP - lagn(mfacs$UNEMP,6)
		mfacs$UNEMP_2YD <- mfacs$UNEMP - lagn(mfacs$UNEMP,24)
		mfacs$TR3M_1YD <- mfacs$TREAS3MO - lagn(mfacs$TREAS3MO,12)
		mfacs$TR10Y_1YD <- mfacs$TREAS10Y - lagn(mfacs$TREAS10Y,12)
		mfacs$MORT_1YD <- mfacs$MORT - lagn(mfacs$MORT,12)
	}

	for(gvkey in gvkeys) {
	cat(gvkey,"..")

	set <- oq(paste("select convert(varchar(6),jc.data_date,112) as year_mo,
				(case jc.kdp_1mo when 0 then -11.5129 when 100 then 11.5129 else -log((1-jc.kdp_1mo/100.0)/(jc.kdp_1mo/100.0)) end ) as kdp_1mo,
				(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo,
				(case jc.kdp_1yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_1yr/100.0)/(jc.kdp_1yr/100.0))  end) as kdp_1yr,
				(case jc.kdp_5yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_5yr/100.0)/(jc.kdp_5yr/100.0))  end) as kdp_5yr
				from  kdps.dbo.kdp_stat jc join (
						select stkey, convert(varchar(6),data_date,112) as year_mo, 
						max(data_date) as maxdate 
						from kdps.dbo.kdp_stat where stkey='",gvkey,"' and model='JC5' group by stkey, convert(varchar(6),data_date,112)
				) m on jc.stkey=m.stkey and jc.data_date=m.maxdate where jc.stkey='",gvkey,"' and model='JC5' order by year_mo",sep=""))
					
	if(nrow(set)==0) next
			set$year_mo <- as.integer(set$year_mo)
		for(x in 2:ncol(set)) set[,x] <- as.numeric(set[,x])
		testset <- merge(set,mfacs,by.x='year_mo')
		if(nrow(testset)<12) next
		num_obs <- nrow(testset)
		lmtr <- trunc(num_obs*.5)
		
		for(y in 1:(length(kdp_cols))) { 
			lmtr_flag <- FALSE
			term = toupper(substr(kdp_cols[y],5,7))
			lside = kdp_cols[y]
			wf <- NULL
			wrsq <- 0

	#-----THE WHEEL OF FORTUNE
			lwf <- wf
			qqq <- 1
			TRYS=10
				while(qqq) {
				#-----DOMESTIC MACRO-FACTORS
					trythese <- unlist(fac_list[[1]])
					trythese <- trythese[!is.element(trythese,wf)]
					wf_wrsq <- runthese(trythese,wf)
						wf <- wf_wrsq[[2]]
						wrsq <- wf_wrsq[[3]]
				#-----GROUPED MACRO-FACTORS
					for(x in 1:length(fac_list[[2]])) { 
						wfs <- NULL
						wrsqs <- NULL
						wts <- NULL
						trythese <- fac_list[[2]][x][[1]]
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
								wts[xx] <- ifelse(is.null(wf_wrsq[[3]]),FALSE,all(abs(wf_wrsq[[1]]$coefficients[,3][-1])>2))
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

			if(qqq>1) if(all(is.element(wf,lwf)) & all(is.element(lwf,wf))) qqq=FALSE
			if(qqq==TRYS) qqq=FALSE	
			if(qqq) qqq=qqq+1
			if(qqq>1) lwf=wf
				}
if(length(wf)>0) {
lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
gcset <- data.frame(cbind(gvkey,'JC5',yy,term,row.names(lset$coefficients)[-1],lset$coefficients[-1,1],lset$coefficients[-1,2],lset$coefficients[-1,3],lset$coefficients[-1,4]))
for(x in 1:ncol(gcset)) gcset[,x] <- as.character(gcset[,x])
for(x in 1:nrow(gcset)) tq(paste("insert therocrates.dbo.gvkey_coeffs_country_proxies values('",paste(gcset[x,],sep="",collapse="','"),"')",sep=""))
tq(paste("insert therocrates.dbo.GVKEY_TERMS_country_proxies values('",paste(gvkey,'JC5',yy,term,lset$sigma,lset$coefficients[1,1],lset$adj.r.squared,sep="','"),"')",sep=""))
			}
		}
	}
}

#truncate table therocrates.dbo.gvkey_coeffs_country_proxies
#truncate table therocrates.dbo.GVKEY_TERMS_country_proxies
