mydir <- ifelse(Sys.getenv('COMPUTERNAME') == 'TED-PC','c:/therocrates','d:/kamakura_r')
cat("\n",mydir,"\n")
source(paste(mydir,"/kamakura_Rutils.r",sep=""))
timestamp()

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
#write.table("\tnow stepwise",file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
	flag <- as.logical(sum(abs(lset$coefficients[,3][-1])<=2))
	tscrs <- abs(lset$coefficients[,3][-1])
#write.table(paste(row.names(lset$coefficients)[-1][order(tscrs)][-1],collapse=" + "),file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
	while(flag) {
		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(row.names(lset$coefficients)[-1][order(tscrs)][-1],collapse=" + "),sep="")),testset))
#write.table(paste("\t\t",paste(row.names(lset$coefficients)[-1],collapse=" + "),"\tT-score test:  ",all(abs(lset$coefficients[,3][-1])>2),"\trsq:  ",lset$adj.r.squared,sep=""),file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
		flag <- ifelse(sum((abs(lset$coefficients[,3][-1])<2))<2,FALSE,as.logical(sum((abs(lset$coefficients[,3][-1])<2))))
        tscrs <- abs(lset$coefficients[,3][-1])
	}
return(lset)
}

trythisone <- function(frmla,testset,fwf,fwrsq) {
	fwf <- wf
	fwrsq <- wrsq
#write.table(paste("\t\t",paste(frmla[3]),sep=""),file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
	lset <- summary(lm(frmla,testset))
#write.table(paste("\tT-score test:  ",all(abs(lset$coefficients[,3][-1])>2),"\trsq:  ",lset$adj.r.squared,sep=""),file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
	if(all(abs(lset$coefficients[,3][-1])>2) & lset$adj.r.squared > fwrsq) {
			fwf <- row.names(lset$coefficients)[-1]
			fwrsq <- lset$adj.r.squared
		return(list(lset,fwf,fwrsq))
	}
	if(!all(abs(lset$coefficients[,3][-1])>2)){
			twf <- row.names(lset$coefficients)[-1][abs(lset$coefficients[,3])[-1]>2]
			if(length(twf)==0) return(list(lset,fwf,fwrsq))
		lset <- dostepwise(lset,lside)
			if(as.numeric(lset$adj.r.squared) > fwrsq) {
			fwf <- row.names(lset$coefficients)[-1]
			fwrsq <- as.numeric(lset$adj.r.squared) 
			}
		}
	return(list(lset,fwf,fwrsq))
}

base_formula <- c('VIX','CRE1YR','HOUSE1YR','DJ1YR','CPI','BBBSPD','TSLOPE','UNEMP')

large_co_facs <- c('CPIASIA','CPIEUR','CPIJPN','CPIUK','FXASIA','FXEUR','FXJPN','FXUK','RGDPASIA','RGDPEUR','RGDPJPN','RGDPUK')
base_plus <- list(
	c('RGDP','NGDP'),
	c('UNEMP_1YD','UNEMP_6MD','UNEMP_2YD'),
	c('PRIME','TREAS3MO','TREAS5YR','TREAS10Y','MORT'),
	c('PRIME_1YD','TR3M_1YD','TR5Y_1YD','TR10Y_1YD','MORT_1YD'),
	c('RDISINC','NDISINC')
)

kdp_cols <- c('kdp_1mo','kdp_3mo','kdp_1yr','kdp_5yr')
mfacs <- getRRF3mf()
mfacs <- mfacs[mfacs$year_mo>198912,]	#----- no kdps < January 1990

#gclass='PUB'
#gvkeys <- getIdx(idx='1500')[[2]]$gvkey
#gvkeys <- append(gvkeys,c('165264','166230','170527','170841','180635','183920','184009','184263','184392','184500','184571','184714','170771',
#			'184715','184899','185177','185549','185618','186310','186773','186989','187041','187105','187128','187406','188255',
#			'189490','189491','197956','199356','294524','298341','185786','314939','318094','294366','296392','312080','314499','295434',
#			'316293','313121','185258','015422','185664','294290','316025','294028','298238','312485','297053','295362','296421','313118',
#			'019498','294188','170981','311946','020550','187525','185123','185602','184299','296948','311838','297834','312701','170607',
#			'021156','186675','313115','178972','185351','186562','317309','311886','020020','016500','184459','300487','015623','295071',
#			'019777','314853','317792','018321','313179','317083','313796','297154','315896','305393','315989','312725','189058','312429',
#			'293894','312939','318167','186845','315437','294210','295348','312430','311730','296243','294252','311315','316815','016814',
#			'185719','016653','303631','295944','295380','295667','020089','296533','313324','186097','315549','016153','294466','313343',
#			'179686','313678','293866','018246','186279'))
#gvkeys <- unique(gvkeys)

#gclass='SEC'
#gvkeys <- getGvkeys('SEC',1,1)[1:20]

gclass='PUB'
#gclass='SEC'
#gclass='SOV'
gvkeys <- getGvkeys(gclass,700,700)[1:10]
gvkeys <- append(gvkeys,getGvkeys(gclass,700,600)[1:10])
gvkeys <- append(gvkeys,getGvkeys(gclass,700,500)[1:10])
gvkeys <- append(gvkeys,getGvkeys(gclass,700,400)[1:10])
gvkeys <- append(gvkeys,getGvkeys(gclass,700,300)[1:10])
gvkeys <- append(gvkeys,getGvkeys(gclass,700,200)[1:10])
gvkeys <- append(gvkeys,getGvkeys(gclass,700,100)[1:10])
gvkeys <- append(gvkeys,getGvkeys(gclass,700,10)[1:10])

for(gvkey in gvkeys) {
	if(gclass=='SEC') {
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
	} else { #----- these are SOVS will need to tune it up
		fac_list <- list(base_formula,base_plus)
		gids <- oq(paste("select top 1 d.market_cap, c.gsubind, c.loc, c.conm from TONGA.market.dbo.security_daily d 
			join TONGA.market.dbo.company c on c.gvkey=d.gvkey and d.gvkey='",gvkey,"' order by datadate desc",sep=""))
	}

	set <- tq(paste("select convert(varchar(6),jc.data_date,112) as year_mo,
				(case jc.kdp_1mo when 0 then -11.5129 when 100 then 11.5129 else -log((1-jc.kdp_1mo/100.0)/(jc.kdp_1mo/100.0)) end ) as kdp_1mo,(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo,
				(case jc.kdp_1yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_1yr/100.0)/(jc.kdp_1yr/100.0))  end) as kdp_1yr,(case jc.kdp_5yr when 0 then -11.5129 when 100 then 11.5129 else -log((1.0-jc.kdp_5yr/100.0)/(jc.kdp_5yr/100.0))  end) as kdp_5yr
				from rrf3.dbo.kdp_jc5 jc where jc.gvkey='",gvkey,"' and model='JC5' order by year_mo",sep=""))
	if(nrow(set)==0) next
		set$year_mo <- as.integer(set$year_mo)
	for(x in 2:ncol(set)) set[,x] <- as.numeric(set[,x])
	testset <- merge(set,mfacs,by.x='year_mo')
	if(nrow(testset)<12) next
	num_obs <- nrow(testset)
	lmtr <- trunc(num_obs*.5)
	lmtr <- 6
	for(y in 1:(length(kdp_cols))) { 
		lmtr_flag <- FALSE
		term = toupper(substr(kdp_cols[y],5,7))
		lside = kdp_cols[y]
		wf <- NULL
		wrsq <- 0

#-----THE WHEEL OF FORTUNE
		lwf <- wf
		qqq <- 1
write.table(paste(gvkey,"\t",gids$conm,"\t",lside,sep=""),file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
		while(qqq) {
write.table(paste("#####-----ROUND\t",qqq," of 10\t",gvkey,"\t",paste(wf,sep="",collapse=" + "),"\t",wrsq,"\tLMTR = ",lmtr,"\tlength wf =",length(wf),sep=""),file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
		#-----DOMESTIC MACRO-FACTORS
			trythese <- unlist(fac_list[[1]])
			trythese <- trythese[!is.element(trythese,wf)]

			if(length(trythese)!=0) {
				for(xx in 1:length(trythese)){
					mf <- trythese[xx]
#write.table(mf,file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
					if(length(wf)==lmtr) {
						lmtr_flag = TRUE
						lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
						wf_holder <- wf
						wf <- row.names(lset$coefficients)[-1][order(abs(lset$coefficients[-1,3]))][-1]
					}

					frmla <- as.formula(paste(lside," ~ ",paste(append(wf,mf),sep="", collapse=" + "),sep=""))
					wf_wrsq <- trythisone(frmla,testset,wf,wrsq)
						if(all(abs(wf_wrsq[[1]]$coefficients[,3][-1])>2) & wf_wrsq[[3]]>wrsq) {
							wf <- wf_wrsq[[2]]
							wrsq <- wf_wrsq[[3]]
write.table(paste("NEW WF:\t",paste(wf,sep="",collapse=" + "),"\tWRSQ:\t",wrsq,sep=""),file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
						} else {
							if(lmtr_flag) wf <- wf_holder
						}
				}
			}
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
#write.table(mf,file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
					wf_wrsq <- trythisone(frmlas[[xx]],testset,wf,wrsq)
					wfs[xx] <- list(wf_wrsq[[2]])
					wrsqs[xx]<- wf_wrsq[[3]]
					wts[xx] <- all(abs(wf_wrsq[[1]]$coefficients[,3][-1])>2)
				}
				if(sum(wts)) {
					wfs <- wfs[wts]
					wrsqs <- wrsqs[wts]
					if(wrsqs[order(wrsqs,decreasing=T)][1]>wrsq){
						wf <- wfs[order(wrsqs,decreasing=T)][[1]]
						wrsq <- wrsqs[order(wrsqs,decreasing=T)][1]
write.table(paste("NEW WF:\t",paste(wf,sep="",collapse=" + "),"\tWrsq:\t",wrsq,sep=""),file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
					} else {
						if(lmtr_flag) wf <- wf_holder
					}
				}
			}
				
			#-----INTERNATIONAL & LARGE COMPANY FACS
			if(length(fac_list)==3){
				trythese <- unlist(fac_list[[3]])
				trythese <- trythese[!is.element(trythese,wf)]
				if(length(trythese)!=0) {
				for(xx in 1:length(trythese)){
					if(length(wf)==lmtr) {
						lmtr_flag = TRUE
						lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
						wf_holder <- wf
						wf <- row.names(lset$coefficients)[-1][order(abs(lset$coefficients[-1,3]))][-1]
					}
					mf <- trythese[xx]
#write.table(mf,file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
						frmla <- as.formula(paste(lside," ~ ",paste(append(wf,mf),sep="", collapse=" + "),sep=""))
						wf_wrsq <- trythisone(frmla,testset,wf,wrsq)
							if(all(abs(wf_wrsq[[1]]$coefficients[,3][-1])>2) & wf_wrsq[[3]]>wrsq) {
								wf <- wf_wrsq[[2]]
								wrsq <- wf_wrsq[[3]]
write.table(paste("NEW WF:\t",paste(wf,sep="",collapse=" + "),"\tWrsq:\t",wrsq,sep=""),file="./tmp/catit.txt",append=T,col.names=F,row.names=F,quote=F)
							} else {
							if(lmtr_flag) wf <- wf_holder
							}
					}
				}
			}
		if(qqq>1) if(all(is.element(wf,lwf)) & all(is.element(lwf,wf))) qqq=FALSE
		if(qqq==10) qqq=FALSE
		if(qqq) qqq=qqq+1
		if(qqq>1) lwf=wf
		}
#	if(length(wf)>0) {
#		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
#			gcset <- data.frame(cbind(gvkey,term,row.names(lset$coefficients)[-1],lset$coefficients[-1,1],lset$coefficients[-1,2],lset$coefficients[-1,3]))
#			for(x in 1:nrow(gcset)) tq(paste("insert rrf3.dbo.gvkey_coeffs_ccar_20140923 values('",paste(gcset[x,1],gcset[x,2],gcset[x,3],gcset[x,4],gcset[x,5],gcset[x,6],sep="','"),"')",sep=''))
#			tq(paste("insert rrf3.dbo.gvkey_terms_ccar_20140923 values('",paste(gvkey,term,lset$sigma,lset$coefficients[1,1],lset$adj.r.squared,sep="','"),"')",sep=""))
#}  
#for(zz in 2:length(fac_list[[2]])) {
#	if(sum(is.element(wf,fac_list[[2]][zz][[1]]))>1) {
#		cat("\n****************************\n",gvkey,"\t",paste(wf,sep="",collapse=" "),"\t",paste(fac_list[[2]][zz][[1]],sep="",collapse=" "),"\n")
#	}
} 
}
