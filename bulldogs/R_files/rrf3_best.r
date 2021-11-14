mydir <- ifelse(Sys.getenv('COMPUTERNAME') == 'TED-PC','c:/therocrates','d:/kamakura_r')
cat("\n",mydir,"\n")
source(paste(mydir,"/kamakura_Rutils.r",sep=""))
timestamp()

#	cmd.exe /c "R < d:/kamakura_r/rrf3_best.r "12" "1" "PUB"> d:/kamakura_r/tmp/rrf3_best_1.out 2>&1"
args		=	commandArgs()[-(1:2)]
quantiles	=	as.integer(args[1])
quantile	=	as.integer(args[2])
gclass		=	as.character(args[3])

getGvkeys <- function(gclass='PUB',quantiles=1,quantile=1){
	if(gclass=='PUB') gvkeys=oq("select distinct gvkey from kris.dbo.company_ticker where class in('PUB') order by gvkey")[[1]]
	if(gclass=='SEC') gvkeys=tq("select distinct stkey from OKINAWA.kris.dbo.stat_mast where rating is null and len(GIC) in(2,4)")[[1]]	
	if(gclass=='SOV') gvkeys=oq("select distinct gvkey from kris.dbo.company_ticker where class in('SOV') order by gvkey")[[1]]
	if(gclass=='RAT') gvkeys=oq("select distinct gvkey from kris.dbo.company_ticker where class in('RAT') order by gvkey")[[1]]
	n <- length(gvkeys)
        qfac <- 1/quantiles
        max_qfac <- ifelse(quantile==1,qfac,qfac*quantile)
        if(quantile==1) return(gvkeys[gvkeys<gvkeys[n*max_qfac]])
        min_qfac <- qfac*(quantile-1)
        if(quantile==quantiles) return(gvkeys[gvkeys>=gvkeys[n*min_qfac]]) 
return(gvkeys[gvkeys>=gvkeys[n*min_qfac] & gvkeys<gvkeys[n*max_qfac]])
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

base_formula <- c('VIX','CRE1YR','HOUSE1YR','DJ1YR','CPI','BBBSPD','TSLOPE','UNEMP')
large_co_facs <- c('CPIASIA','CPIEUR','CPIJPN','CPIUK','FXASIA','FXEUR','FXJPN','FXUK','RGDPASIA','RGDPEUR','RGDPJPN','RGDPUK')
base_plus <- list(c('RGDP','NGDP'), c('UNEMP_1YD','UNEMP_6MD','UNEMP_2YD'),c('PRIME','TREAS3MO','TREAS5YR','TREAS10Y','MORT'), c('PRIME_1YD','TR3M_1YD','TR5Y_1YD','TR10Y_1YD','MORT_1YD'), c('RDISINC','NDISINC'))
kdp_cols <- c('kdp_1mo','kdp_3mo','kdp_1yr','kdp_5yr')
mfacs <- getRRF3mf()
mfacs <- mfacs[mfacs$year_mo>198912,]	#----- no kdps < January 1990

gvkeys <- getGvkeys(gclass,quantiles,quantile)
gvkeys <- gvkeys[gvkeys>'009328']

gvkeys <- append(c('001003','001004','001009','001011','001013','001014','001017','001018','001019','001021','001028','001029','001033',
			'001034','001036','001037','001038','001039','001043','001045','001050','001054','001055','001056','001062','001065',
			'001072','001073','001075','001076','001078','001081','001082','001084','001086','001094','001095','001096','001097',
			'001098','001099','001103','001104','001108','001109','001111','001115','001117','001121','001125','001126','001127',
			'001128','001137','001138','001141','001148','001151','001154','001155','001161','001163','001164','001165','001166',
			'001171','001173','001177','001178'),gvkeys)

cat("\nlets do these:\t",min(gvkeys),"\tthrough:\t",max(gvkeys),"\n")
for(gvkey in gvkeys) {
cat(gvkey,"..")
	if(gclass=='SEC' | gclass=='RAT') {
		fac_list <- list(base_formula,base_plus)
		gids <- tq(paste("select 1000 as market_cap, s.gic as gsubind, 'US' as loc, gic.gicdesc as conm from OKINAWA.kris.dbo.stat_mast s join compustat.dbo.giccd gic on gic.giccd=s.gic where s.stkey='",gvkey,"'",sep=""))
	} else if(gclass=='PUB') {
		gids <- tq(paste("select top 1 d.market_cap, c.gsubind, c.loc, c.conm from TONGA.marketview.dbo.security_daily d 
			join TONGA.market.dbo.company c on c.gvkey=d.gvkey and d.gvkey='",gvkey,"' where market_cap is not null order by datadate desc",sep=""))
		if(nrow(gids)==0) next
			gids$market_cap <- ifelse(is.na(gids$market_cap),0,gids$market_cap)
			gids$gsubind <- ifelse(is.na(gids$gsubind),1111,gids$gsubind)
		if((as.numeric(gids$market_cap)>15000 & !is.element(substr(gids$gsubind,1,4),c('4030','5510')) & gids$loc=='USA') | gids$loc!='USA') {
		fac_list <- list(base_formula,base_plus,large_co_facs)
		} else {
		fac_list <- list(base_formula,base_plus)
		}
	} else if(gclass=='SOV') { #----- these are SOVS will need to tune it up
		fac_list <- list(large_co_facs,base_plus,base_formula)   #----- because SOV, we try international/large-co facs first
		gids <- sq(paste("select 100 as market_cap, gsubind, right('",gvkey,"',3) as loc, conm from kris.dbo.company_ticker where gvkey='",gvkey,"'",sep=""))
	}

	if(gclass=='SOV') {
	set <- sq(paste("select convert(varchar(6),jc.data_date,112) as year_mo,(case jc.kdp_1mo when 0 then -11.5129 when 100 then 11.5129 else -log((1-jc.kdp_1mo/100.0)/(jc.kdp_1mo/100.0)) end ) as kdp_1mo,
				(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo,(case jc.kdp_1yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_1yr/100.0)/(jc.kdp_1yr/100.0))  end) as kdp_1yr, (case jc.kdp_5yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_5yr/100.0)/(jc.kdp_5yr/100.0))  end) as kdp_5yr
				from kdp_ts.dbo.kdp_sd5 jc join (
					select gvkey, convert(varchar(6),data_date,112) as year_mo, max(data_date) as maxdate from kdp_ts.dbo.kdp_sd5 where gvkey='",gvkey,"' group by gvkey, convert(varchar(6),data_date,112)
				) m on jc.gvkey=m.gvkey and jc.data_date=m.maxdate where jc.gvkey='",gvkey,"' and jc.model='SD5' order by year_mo",sep=""))
	} else if(gclass=='SEC' | gclass=='RAT') {
	set <- sq(paste("select convert(varchar(6),jc.data_date,112) as year_mo,(case jc.kdp_1mo when 0 then -11.5129 when 100 then 11.5129 else -log((1-jc.kdp_1mo/100.0)/(jc.kdp_1mo/100.0)) end ) as kdp_1mo,
				(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo, (case jc.kdp_1yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_1yr/100.0)/(jc.kdp_1yr/100.0))  end) as kdp_1yr, (case jc.kdp_5yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_5yr/100.0)/(jc.kdp_5yr/100.0))  end) as kdp_5yr
				from  kris.dbo.kdp_stat jc join (
					select stkey, convert(varchar(6),data_date,112) as year_mo, max(data_date) as maxdate from kris.dbo.kdp_stat  where stkey='",gvkey,"' and model='JC5' group by stkey, convert(varchar(6),data_date,112)
				) m on jc.stkey=m.stkey and jc.data_date=m.maxdate where jc.stkey='",gvkey,"' and model='JC5' order by year_mo",sep=""))
	} else {
	set <- tq(paste("select convert(varchar(6),jc.data_date,112) as year_mo,
				(case jc.kdp_1mo when 0 then -11.5129 when 100 then 11.5129 else -log((1-jc.kdp_1mo/100.0)/(jc.kdp_1mo/100.0)) end ) as kdp_1mo,(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo,
				(case jc.kdp_1yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_1yr/100.0)/(jc.kdp_1yr/100.0))  end) as kdp_1yr,(case jc.kdp_5yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_5yr/100.0)/(jc.kdp_5yr/100.0))  end) as kdp_5yr
				from rrf3.dbo.kdp_jc5 jc where jc.gvkey='",gvkey,"' and model='JC5' order by year_mo",sep=""))
	}
				
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
			#-----INTERNATIONAL & LARGE COMPANY FACS
				if(length(fac_list)==3){
					trythese <- unlist(fac_list[[3]])
					trythese <- trythese[!is.element(trythese,wf)]
						wf_wrsq <- runthese(trythese,wf)
						wf <- wf_wrsq[[2]]
						wrsq <- wf_wrsq[[3]]
				}
			if(qqq>1) if(all(is.element(wf,lwf)) & all(is.element(lwf,wf))) qqq=FALSE
			if(qqq==TRYS) qqq=FALSE	
			if(qqq) qqq=qqq+1
			if(qqq>1) lwf=wf
			}
		if(length(wf)>0) {
		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
			gcset <- data.frame(cbind(gvkey,term,row.names(lset$coefficients)[-1],lset$coefficients[-1,1],lset$coefficients[-1,2],lset$coefficients[-1,3],lset$coefficients[-1,4]))
			for(x in 1:nrow(gcset)) tq(paste("insert rrf3.dbo.gvkey_coeffs_ccar_",gclass,"_tmp values('",paste(gcset[x,1],gcset[x,2],gcset[x,3],gcset[x,4],gcset[x,5],gcset[x,6],gcset[x,7],sep="','"),"')",sep=''))
			tq(paste("insert rrf3.dbo.gvkey_terms_ccar_",gclass,"_tmp values('",paste(gvkey,term,lset$sigma,lset$coefficients[1,1],lset$adj.r.squared,sep="','"),"')",sep=""))
		}
	}
}
