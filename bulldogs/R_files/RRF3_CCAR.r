
source(paste(mydir,"/kamakura_Rutils.r",sep=""))
timestamp()

#-----	cmd.exe /c "R < D:\kamakura_r/RRF3_CCAR.r "5" "1" > d:/kamakura_r/tmp/ccar_pub_calibrate.out 2>&1"

#	samoa.mfs.dbo.rf_values_ccar
#	select * from stat_mast where rating is null and len(gic) in (2,4) - sectors

#-----	args <- commandArgs()[-(1:2)]
#-----	cat(paste(args,sep="",collapse="\t"))

getGvkeys <- function(quantiles=1,quantile=1){
	gvkeys=oq("select distinct gvkey from kris.dbo.company_ticker where class in('PUB','SOV') order by gvkey")[[1]]
	n <- length(gvkeys)
        qfac <- 1/quantiles
        max_qfac <- ifelse(quantile==1,qfac,qfac*quantile)
        if(quantile==1) return(gvkeys[gvkeys<gvkeys[n*max_qfac]])
        min_qfac <- qfac*(quantile-1)
        if(quantile==quantiles) return(gvkeys[gvkeys>=gvkeys[n*min_qfac]]) 
return(gvkeys[gvkeys>=gvkeys[n*min_qfac] & gvkeys<gvkeys[n*max_qfac]])
}

#----- lets get our universe
#-----		if(length(args)==0) gvkeys <- getGvkeys(1,1)
#-----		if(length(args)==2) {
#-----			quantiles <- as.integer(args[1])
#-----			quantile <- as.integer(args[2])
#-----			gvkeys <- getGvkeys(quantiles,quantile)
#-----		}

#-----	gvkeys <- getGvkeys(400,30)
#gvkeys <- getIdx('DJIA')[[2]]$gvkey
sector_gvkeys <- oq("select distinct stkey from OKINAWA.kris.dbo.stat_mast where rating is null and len(GIC) in(2,4)")[[1]]
cat("\nlets do these:\t",min(gvkeys),"\tthrough:\t",max(gvkeys),"\n")

#-----	these are the kdps to be tested
kdp_cols <- c('kdp_1mo','kdp_3mo','kdp_1yr','kdp_5yr')

#-----	that which we seek to write to the database
ds.gvkey_terms <- data.frame(matrix(nrow=0,ncol=5))
	names(ds.gvkey_terms) <- c('gvkey','term','error_term','intercept','adj_r_sqr')
ds.gvkey_coeffs <- data.frame(matrix(nrow=0,ncol=6))
	names(ds.gvkey_coeffs) <- c('gvkey','term','rf_code','coeff','std_err','t_stat')

#-----	mfacs <- data.frame(matrix(NA,ncol=length(colz),nrow=length(datez)))
#-----	ccar macro factor variables 
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
write.table(mfacs,file="./tmp/mfacs_20140819.csv",sep=",",quote=T,row.names=F,append=F)	

step_one <- list(c('VIX'),c('RGDP','NGDP'),c('UNEMP_1YD','UNEMP_6MD','UNEMP_2YD'),c('CRE1YR'),
		c('HOUSE1YR'),c('DJ1YR'),c('CPI'),c('BBBSPD'),c('TSLOPE'),
		c('PRIME','TREAS3MO','TREAS5YR','TREAS10Y','MORT'),
		c('PRIME_1YD','TR3M_1YD','TR5Y_1YD','TR10Y_1YD','MORT_1YD'),
		c('RDISINC','NDISINC')
)
step_two <-step_one

base_formula <- paste(step_one[[1]],step_one[[4]],step_one[[5]],step_one[[6]],step_one[[7]],step_one[[8]],step_one[[9]],sep=" + ")
large_co_facs <- c('CPIASIA','CPIEUR','CPIJPN','CPIUK','FXASIA','FXEUR','FXJPN','FXUK','RGDPASIA','RGDPEUR','RGDPJPN','RGDPUK')

step_one_formulaz <- NULL
for(s in step_one[[2]]) {
	for(t in step_one[[3]]) {
		for(u in step_one[[10]]) {
			for(v in step_one[[11]]) {
				for(w in step_one[[12]]){
				f <- paste(s,t,u,v,w,sep=" + ")
				step_one_formulaz <- append(step_one_formulaz,f)
				}
			}
		}
	}
}
step_one_formulaz <- paste(base_formula,step_one_formulaz,sep=" + ")
large_co_formulaz <-  paste(step_one_formulaz,paste(large_co_facs,sep="",collapse=" + "),sep=" + ")
#-----	gvkeys <- gvkeys[1:60]

tester <- data.frame(matrix(NA,ncol=6,nrow=0))
	names(tester) <- c('gvkey','conm','term','rsq','wf','coeffs')
gvkeys <- sector_gvkeys	
for(gvkey in gvkeys) {
cat(gvkey,"..")
#gvkey='006066'
#gvkey='004093'
#gvkey='178015'
#gvkey='S00003'
	ds.gt <- ds.gvkey_terms
	ds.gc <- ds.gvkey_coeffs

#----- if substr(gvkey,1,1)==S then step_one_formulas
#----- if SOV then 
if(is.element(gvkey,sector_gvkeys)) {
		these_formulas <- step_one_formulaz
		gids <- tq(paste("select 1000 as market_cap, s.gic as gsubind, 'US' as loc, gic.gicdesc as conm 
 		from OKINAWA.kris.dbo.stat_mast s join compustat.dbo.giccd gic on gic.giccd=s.gic 
		where s.stkey='",gvkey,"'",sep=""))
} else if((as.numeric(gids$market_cap)>15000 & !is.element(substr(gids$gsubind,1,4),c('4030','5510')) & gids$loc=='USA') | gids$loc!='USA') {
		these_formulas <- large_co_formulaz 
		gids <- oq(paste("select top 1 d.market_cap, c.gsubind, c.loc, c.conm 
				from market.dbo.security_daily d 
				join market.dbo.company c on c.gvkey=d.gvkey and d.gvkey='",
				gvkey,"' order by datadate desc",sep=""))
} else {
		these_formulas <- step_one_formulaz
		gids <- oq(paste("select top 1 d.market_cap, c.gsubind, c.loc, c.conm 
				from market.dbo.security_daily d 
				join market.dbo.company c on c.gvkey=d.gvkey and d.gvkey='",
				gvkey,"' order by datadate desc",sep=""))
}		
	kdp_values <- tq(paste("select convert(varchar(6),jc.data_date,112) as year_mo,
				(case jc.kdp_1mo when 0 then -11.5129 when 100 then 11.5129 else
				-log((1-jc.kdp_1mo/100.0)/(jc.kdp_1mo/100.0)) end ) as kdp_1mo,
				(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 else
				-log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo,
				(case jc.kdp_1yr when 0 then -11.5129 when 100 then 11.5129 else
				-log((1.0-jc.kdp_1yr/100.0)/(jc.kdp_1yr/100.0))  end) as kdp_1yr,                               
				(case jc.kdp_5yr when 0 then -11.5129 when 100 then 11.5129 else
				-log((1.0-jc.kdp_5yr/100.0)/(jc.kdp_5yr/100.0))  end) as kdp_5yr
				from rrf3.dbo.kdp_jc5 jc where jc.gvkey='",
				gvkey,"' and model='JC5' order by year_mo",sep=""))
	if(nrow(kdp_values)==0) next
	set <- kdp_values
		set$year_mo <- as.integer(set$year_mo)
		for(x in 2:ncol(set)) set[,x] <- as.numeric(set[,x])
	num_obs <- nrow(set)
###		prelim_vars <- prelim_vars[order(prelim_vars$tstat,decreasing=T),]
###		if(nrow(prelim_vars)>trunc(num_obs*.5)) prelim_vars <- prelim_vars[1:trunc(num_obs*.5),]

	if(num_obs < 12) next
	testset <- merge(set,mfacs,by.x='year_mo',all.x=T)
	if(nrow(testset)<48) next
	
for(y in 1:(length(kdp_cols))) {
	term = toupper(substr(kdp_cols[y],5,7))
	rsqs <- 0
	for(x in 1:length(these_formulas)) {
		coefs <- abs(summary(lm(as.formula(paste(kdp_cols[y]," ~ ",these_formulas[x],sep="")),testset))$coefficients[-1,3])
		if(all(is.nan(coefs))) next
		flag <- as.logical(sum(coefs<2))
		while(flag) {
			if(length(coefs)==1) {
				cat(paste(gvkey," this one failed all of the iterations in the abs(t.values < 2) rule so skipping to next\n",sep=""))
				flag <- (length(coefs)-1)
			} else {
			coefstr <- paste(names(coefs)[order(coefs,decreasing=T)][-(length(coefs))],sep="",collapse=" + ")		
			coefs <- abs(summary(lm(as.formula(paste(kdp_cols[y]," ~ ",coefstr,sep="")),testset))$coefficients[-1,3])
			flag <- as.logical(sum(coefs<2))
			}
		}
	rsq <- as.numeric(summary(lm(as.formula(paste(kdp_cols[y]," ~ ",coefstr,sep="")),testset))$r.squared)
			if(rsq>rsqs) {
				rsqs <- rsq
				wf <- coefstr
			}
	}
	if(!exists('wf')) next
	wf_bk <- wf
	dropthis <- NULL
	for(z in 1:length(step_one)) {
		if(sum(is.element(strsplit(wf," \\+ ")[[1]],step_one[[z]])) > 0) dropthis <- append(dropthis,z*-1)
	}
	step_two <- step_one[dropthis]

	for(z in 1:length(step_two)) {
		thesefacs <- step_two[[z]]
			qrs <- NULL
			for(zz in 1:length(thesefacs)) {
				coefs <- abs(summary(lm(as.formula(paste(kdp_cols[y]," ~ ",wf," + ",thesefacs[zz],sep="")),testset))$coefficients[-1,3])
				flag <- as.logical(sum(coefs<2))
				while(flag) {
					if(length(coefs)==1) {
						flag <- (length(coefs)-1)
					} else {
						coefstr <- paste(names(coefs)[order(coefs,decreasing=T)][-(length(coefs))],sep="",collapse=" + ")		
						coefs <- abs(summary(lm(as.formula(paste(kdp_cols[y]," ~ ",coefstr,sep="")),testset))$coefficients[-1,3])
						flag <- as.logical(sum(coefs<2))
					}
				}		
				rsq2 <- as.numeric(summary(lm(as.formula(paste(kdp_cols[y]," ~ ",coefstr,sep="")),testset))$r.squared)
				if(rsq2>rsqs) {
				rsqs <- rsq2
				wf <- coefstr
				}
			}
	}
if(wf != wf_bk) cat(paste(gvkey,"|",gids$conm,"|new_wf|",wf,"|old_wf|",wf_bk,sep=""),"\n")  
w_coefs <-  list(lm(as.formula(paste(kdp_cols[y]," ~ ",wf,sep="")),testset)$coefficients[-1])
tester[(nrow(tester)+1),] <- cbind(gvkey,gids$conm,term,rsqs,wf, paste(names(w_coefs[[1]]),w_coefs[[1]],sep=" ",collapse="|"))
}}
write.table(tester,file="./tmp/Sectors_RRF3_rsqrs_20140822_tahiti.txt",sep="|",row.names=F)

########################################################
#-----	now we need to get all the stats for our gvkeys
wfset <- read.table("./tmp/Sectors_RRF3_rsqrs_20140822_tahiti.txt",sep="|",quote='"',header=T)
for(x in 1:ncol(wfset)) wfset[,x] <- as.character(wfset[,x])
gvkeys <- unique(wfset$gvkey)
gvkeys <- gvkeys[order(gvkeys)]
for(gvkey in gvkeys) {
	wf_stuff <- wfset[wfset$gvkey == gvkey,c('term','wf')]
		wf_stuff$kdp <- paste('kdp_',tolower(wf_stuff$term),sep="")
	ds.gt <- ds.gvkey_terms
	ds.gc <- ds.gvkey_coeffs
		gids <- tq(paste("select 1000 as market_cap, s.gic as gsubind, 'US' as loc, gic.gicdesc as conm 
					from OKINAWA.kris.dbo.stat_mast s join compustat.dbo.giccd gic on gic.giccd=s.gic where s.stkey='",gvkey,"'",sep=""))

		kdp_values <- tq(paste("select convert(varchar(6),jc.data_date,112) as year_mo,
					(case jc.kdp_1mo when 0 then -11.5129 when 100 then 11.5129 else
					-log((1-jc.kdp_1mo/100.0)/(jc.kdp_1mo/100.0)) end ) as kdp_1mo,
					(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 else
					-log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo,
					(case jc.kdp_1yr when 0 then -11.5129 when 100 then 11.5129 else
					-log((1.0-jc.kdp_1yr/100.0)/(jc.kdp_1yr/100.0))  end) as kdp_1yr,                               
					(case jc.kdp_5yr when 0 then -11.5129 when 100 then 11.5129 else
					-log((1.0-jc.kdp_5yr/100.0)/(jc.kdp_5yr/100.0))  end) as kdp_5yr
					from rrf3.dbo.kdp_jc5 jc where jc.gvkey='",
					gvkey,"' and model='JC5' order by year_mo",sep=""))
		if(nrow(kdp_values)==0) next
		set <- kdp_values
			set$year_mo <- as.integer(set$year_mo)
			for(x in 2:ncol(set)) set[,x] <- as.numeric(set[,x])
		num_obs <- nrow(set)
###		prelim_vars <- prelim_vars[order(prelim_vars$tstat,decreasing=T),]
###		if(nrow(prelim_vars)>trunc(num_obs*.5)) prelim_vars <- prelim_vars[1:trunc(num_obs*.5),]

		if(num_obs < 12) next
		testset <- merge(set,mfacs,by.x='year_mo',all.x=T)
		testset <- testset[testset$year_mo<201400,]
		if(nrow(testset)<48) next

		for(z in 1:nrow(wf_stuff)) {
			lset <- summary(lm(as.formula(paste(wf_stuff$kdp[z]," ~ ",wf_stuff$wf[z],sep="")),testset))
			gcset <- data.frame(cbind(gvkey,wf_stuff$term[z], names(lset$coefficients[-1,1]),lset$coefficients[-1,1],lset$coefficients[-1,2],lset$coefficients[-1,3]))
			for(x in 1:nrow(gcset)) {
				tq(paste("insert rrf3.dbo.gvkey_coeffs_ccar values('",
					paste(gcset[x,1],gcset[x,2],gcset[x,3],gcset[x,4],gcset[x,5],gcset[x,6],sep="','"),
					"')",sep=''))
			}
			tq(paste("insert rrf3.dbo.gvkey_terms_ccar values('",
				paste(gvkey,wf_stuff$term[z],lset$sigma,lset$coefficients[1,1],lset$adj.r.squared,sep="','"),
			"')",sep=""))
		}
}

###################################################################
#-----	this one is for sectors
################################
testset <- read.table("./tmp/Sectors_RRF3_rsqrs_20140822_tahiti.txt",sep="|",quote='"',header=T)
for(x in 1:ncol(testset)) testset[,x] <- as.character(testset[,x])
	testset$rsq	<- as.numeric(testset$rsq)

old_rsqs <- oq(paste("select gvkey, term, adj_r_sqr from mfs.dbo.gvkey_terms_smart where gvkey in('",
	paste(unique(testset$gvkey),sep="",collapse="','"),
	"')",sep=""))

new_output <- merge(testset,old_rsqs,by.x=c('gvkey','term'),by.y=c('gvkey','term'))
new_output <- new_output[,c(1,3,2,4,7,5)]
	new_output$rsq <- as.numeric(new_output$rsq)
	new_output$adj_r_sqr <- as.numeric(new_output$adj_r_sqr)
names(new_output) <- ifelse(names(new_output)=='rsq','new_rsq',
						ifelse(names(new_output)=='adj_r_sqr','old_rsq',names(new_output)))

write.table(new_output,file="./tmp/sector_rsqrs_compare_20140822.txt",sep="|",row.names=F)
###########################################################################
#-----	this one is for publics
################################
old_rsqs <- oq(paste("select t.gvkey, t.term, adj_r_sqr as rsq_rrf2 
	from kris.dbo.company_ticker c join mfs.dbo.gvkey_terms_smart t on t.gvkey=c.gvkey 
		where t.reg_id=3 and model='JC5' and t.gvkey in('",
		paste(unique(tester$gvkey),sep="",collapse="','"),
		"')",sep=""))

new_output <- merge(testoutput,old_rsqs,by.x=c('gvkey','term'),by.y=c('gvkey','term'))
new_output <- new_output[,c(6,1,3,2,5,4,7)]
new_output$rsq <- as.numeric(new_output$rsq)
new_output$rsq_rrf2 <- as.numeric(new_output$rsq_rrf2)
write.table(new_output,file="./tmp/RRF3_rsqrs_20140822.txt",sep="|",row.names=F)
##############################################################################

sumset <- data.frame(matrix(NA,ncol=7,nrow=0))
	names(sumset) <- c('term','mean','min','max','lt_zero','gt_zero','stdev')
terms <- c('1MO','3MO','1YR','5YR')
for(term in terms) {
row <- cbind(term,
	mean(new_output[new_output$term==term,'new_rsq'] -  new_output[new_output$term==term,'old_rsq']),
	min(new_output[new_output$term==term,'new_rsq'] -  new_output[new_output$term==term,'old_rsq']),
	max(new_output[new_output$term==term,'new_rsq'] -  new_output[new_output$term==term,'old_rsq']),
	sum(new_output[new_output$term==term,'new_rsq'] -  new_output[new_output$term==term,'old_rsq']<0),
	sum(new_output[new_output$term==term,'new_rsq'] -  new_output[new_output$term==term,'old_rsq']>0),
	sd(new_output[new_output$term==term,'new_rsq'] -  new_output[new_output$term==term,'old_rsq']))
sumset <- rbind(sumset,row)
}
names(sumset) <- c('term','mean','min','max','lt_zero','gt_zero','stdev')
write.table(sumset,file="./tmp/RRF3_vs_RRF2_sectors_sumset_20140822.txt",sep="|",row.names=F)







 for(x in 1:length(gvkeys)) if(gvkeys[x]=='013498') cat(x,"  !")
302   !> gvkeys[302]
[1] "013498"
























	
	


			qrs <- append(qrs,list(summary(lm(as.formula(paste(kdp_cols[y]," ~ ",wf," + ",thesefacs[zz],sep="")),testset))[c(4,8)]))
			}
			
			



			if(z==1) { 
				bmfstr <- append(bmf,thesefacs[order(qrs,decreasing=T)][1])
			} else { 
				bmfstr <- append(bmfstr,thesefacs[order(qrs,decreasing=T)][1]) 
			}
		tvals <- abs(summary(lm(as.formula(paste(kdp_cols[y]," ~ ",paste(bmfstr,sep="",collapse=" + "),sep="")),testset))$coefficients[,3][-1]) >2
		gstr <- names(tvals)[tvals]
		if(is.null(gstr)) bmfstr <- bmf
		if(!is.null(gstr)) bmfstr <- gstr
cat(gvkey,"\n")
cat(paste(names(tvals),tvals,sep=" = ",collapse="        "),"\n")
#}
#}
}
















	step_one_formulaz <- NULL
	for(s in step_one[[2]]) {
		for(t in step_one[[3]]) {
			for(u in step_one[[10]]) {
				for(v in step_one[[11]]) {
					for(w in step_one[[12]]){
					f <- paste(s,t,u,v,w,sep=" + ")
					step_one_formulaz <- append(step_one_formulaz,f)
					}
				}
			}
		}
	}

	
	rsqs <- append(rsqs,summary(lm(as.formula(paste(kdp_cols[y]," ~ ",these_formulas[x],sep="")),testset))$r.squared)
	}

	mf_cols <- mf_cols[order(rsqs,decreasing=T)]
	step_one_cols <- mf_cols[is.element(mf_cols,unlist(step_one))]
 	tmp_one <- step_one
	bmf <- step_one_cols[1]

	for(z in 1:length(tmp_one)) {
		if(is.element(bmf,tmp_one[[z]])) {
			dropthis <- z*-1
		}
	}
	tmp_one <- tmp_one[dropthis]
	step_one_cols <- step_one_cols[-1]
	sortstr <- NULL
	for(z in 1:length(step_one_cols)) {
		for(zz in 1:length(tmp_one)) if(is.element(step_one_cols[z],tmp_one[[zz]])) sortstr <- append(sortstr,zz) 
	}
	tmp_one <- tmp_one[unique(sortstr)]


	for(step in step_one_cols) {


	cat(paste(step,vs,sep=" + ",collapse=""),"\n")
	}
}

summary(lm(as.formula(paste(kdp_cols[y]," ~ ",step_one_cols,sep="")),testset)

	
for(z in 1:length(step_two)) {
	thesefacs <- step_two[[z]]
		qrs <- NULL
		for(zz in 1:length(thesefacs)) {
			qrs <- append(qrs,list(summary(lm(as.formula(paste(kdp_cols[y]," ~ ",wf," + ",thesefacs[zz],sep="")),testset))[c(4,8)]))
	
}




			if(z==1) { 
				bmfstr <- append(bmf,thesefacs[order(qrs,decreasing=T)][1])
			} else { 
				bmfstr <- append(bmfstr,thesefacs[order(qrs,decreasing=T)][1]) 
			}
		tvals <- abs(summary(lm(as.formula(paste(kdp_cols[y]," ~ ",paste(bmfstr,sep="",collapse=" + "),sep="")),testset))$coefficients[,3][-1]) >2
		gstr <- names(tvals)[tvals]
		if(is.null(gstr)) bmfstr <- bmf
		if(!is.null(gstr)) bmfstr <- gstr
cat(gvkey,"\n")
cat(paste(names(tvals),tvals,sep=" = ",collapse="        "),"\n")
#}
#}
}


			step_one_cols <- step_one_cols[!is.element(step_one_cols,thesefacs)]
			}
		}
tmp_one <- tmp_one[dropthis]

			if(is.element(bmf,tmp_one[[z]])) {
			dropthis <- z*-1
	
	
	
			intercept <- thing$coefficients[[1]]
			coefs <- coefs[-1,]
		flag <- as.logical(nrow(coefs[abs(coefs[,3])<2,]))
			if(nrow(coefs)==1) {
				cat(paste(gvkey," this one failed all of the iterations in the abs(t.values < 2) rule so skipping to next\n",sep=""))
			flag <- (nrow(coefs)-1)
			} else {
			coefs <- coefs[order(coefs[,4],decreasing=T),][-1,]
			mf_cols <- coefs$mfac
			}
		}
		sumthing <- summary(thing)
		error_term = sumthing$sigma
		adj_r_sqr = sumthing$adj.r.squared
		
		ds.gt[(nrow(ds.gt)+1),] <- cbind(reg_id,gvkey,model,term,error_term,intercept,adj_r_sqr)

		gsset <- as.data.frame(matrix(nrow=nrow(coefs),ncol=8))
		names(gsset) <- names(ds.gvkey_coeffs)
			gsset$reg_id	<- 	reg_id
			gsset$gvkey	<- 	gvkey
			gsset$model	<- 	model
			gsset$term	<- 	term
			gsset$rf_code	<-	coefs$mfac
			gsset$coeff	<-	coefs[,1]
			gsset$std_err	<-	coefs[,2]
			gsset$t_stat	<-	coefs[,3]
		ds.gc	<-	rbind(ds.gc,gsset)	
		}
	ds.gt$reg_id <- as.integer(ds.gt$reg_id)
	ds.gt$gvkey <- as.character(ds.gt$gvkey)
	ds.gt$error_term <- as.numeric(ds.gt$error_term)
	ds.gt$intercept <- as.numeric(ds.gt$intercept)
	ds.gt$adj_r_sqr <- as.numeric(ds.gt$adj_r_sqr)
	
	
	

	

	if(min(kdp_values$year_mo) < 199609) notthese <- append(notthese,c('RGDPASIA','CPIASIA'))

	for(y in 1:(length(kdp_cols))) {
		term = toupper(substr(kdp_cols[y],5,7))
		gfacs <- NULL
		for(x in 1:length(mfac_pairs)) {
			formula_str <- as.formula(paste(paste(kdp_cols[y]," ~ ",sep=""),paste(mfac_pairs[[x]],sep="",collapse="+"),sep=""))
			coefs <- as.data.frame(summary(lm(formula_str,testset))[4])[-1,]
			gfacs <- append(gfacs,row.names(coefs[order(coefs[,4],decreasing=F),])[1])
		}
		if((gids$loc=='USA' & gids$market_cap>40000 & !is.element(substr(gids$gsubind,1,4),c('5510','4030'))) | gids$loc!='USA') {
			mf_cols <- c(mfac_set1,gfacs)
		} else {
			mf_cols <- c(mfac_set2,gfacs) 
		}

		mf_cols <- mf_cols[!is.element(mf_cols,notthese)]

		flag <- as.logical(length(mf_cols))
		while(flag) {
			formula_str <- as.formula(paste(paste(kdp_cols[y]," ~ ",sep=""),paste(mf_cols,sep="",collapse="+"),sep=""))
			thing <- lm(formula_str,testset)
			coefs <- as.data.frame(summary(thing)[4])
			intercept <- coefs[1,1]
			coefs <- coefs[-1,]
			flag <- as.logical(nrow(coefs[abs(coefs[,3])<2,]))
			if(nrow(coefs)==1) {
				flag <- F
			} else {
				coefs <- coefs[order(coefs[,4],decreasing=T),][-1,]
				mf_cols <- row.names(coefs)
			}
		}

		while(length(mf_cols)>num_obs*5) {
			formula_str <- as.formula(paste(paste(kdp_cols[y]," ~ ",sep=""),paste(mf_cols,sep="",collapse="+"),sep=""))
			thing <- lm(formula_str,testset)
			coefs <- as.data.frame(summary(thing)[4])
			intercept <- coefs[1,1]
			coefs <- coefs[-1,]
			coefs <- coefs[order(coefs[,4],decreasing=T),][-1,]
			mf_cols <- row.names(coefs)
		}

	sumthing <- summary(thing)
	error_term = sumthing$sigma
	adj_r_sqr = sumthing$adj.r.squared
	
	ds.gt[(nrow(ds.gt)+1),] <- cbind(gvkey,term,error_term,intercept,adj_r_sqr)
	gsset <- as.data.frame(matrix(nrow=nrow(coefs),ncol=6))
	names(gsset) <- names(ds.gvkey_coeffs)
		gsset$gvkey	<- 	gvkey
		gsset$term	<- 	term
		gsset$rf_code	<-	row.names(coefs)
		gsset$coeff	<-	coefs[,1]
		gsset$std_err	<-	coefs[,2]
		gsset$t_stat	<-	coefs[,3]
	ds.gc	<-	rbind(ds.gc,gsset)	
	}

ds.gt$gvkey <- as.character(ds.gt$gvkey)
ds.gt$error_term <- as.numeric(ds.gt$error_term)
ds.gt$intercept <- as.numeric(ds.gt$intercept)
ds.gt$adj_r_sqr <- as.numeric(ds.gt$adj_r_sqr)
fijiWriteTable(ds.gt,'rrf2.gvkey_terms_ccar',T)
fijiWriteTable(ds.gc,'rrf2.gvkey_coeffs_ccar',T)
}
timestamp()

