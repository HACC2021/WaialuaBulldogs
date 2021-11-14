
final_gc_set <- data.frame(matrix(nrow=0,ncol=6))
	names(final_gc_set) <- c('gvkey','term','rf_code','coeff','std_err','t_stat')

final_gt_set <- data.frame(matrix(nrow=0,ncol=5))
	names(final_gt_set) <- c('gvkey','term','error_term','intercept','adj_r_sqr')

term <- 'kdp_3mo'

scenarios <- c(52,53,54)
	
mfset <- oq("select z.rf_num, z.rf_code, z.rf_value, z.year_mo from(
				select m.rf_num, v.rf_code, v.rf_value, substring(convert(char,v.data_date,112),1,6) as year_mo from (
				select rf.rf_num, rv.rf_code, year(rv.data_date) as y_date, month(rv.data_date) as m_date, convert(varchar(8),max(rv.data_date),112) as max_date 
				from mfs.dbo.rf_values rv join mfs.dbo.risk_factors rf on rv.rf_code=rf.rf_code 
				join mfs.dbo.regress_set rs on rs.rf_num=rf.rf_num 
				where rf.rf_code in('RGDP','NGDP','UNEMP','TREAS3MO','TREAS10Y') 
				and rv.data_date>'1900-12-31'  -- HERE IS WHERE WE ADD THE DATE CONSTRAINT
				group by year(rv.data_date), month(rv.data_date), rf.rf_num, rv.rf_code ) m 
				join mfs.dbo.rf_values v on v.rf_code=m.rf_code and v.data_date=m.max_date) z 
				order by year_mo, rf_code")
	mfset$rf_num	<- as.integer(mfset$rf_num)
	mfset$rf_value	<-as.numeric(mfset$rf_value)
	mfset$year_mo	<-as.integer(mfset$year_mo)

rf_values <- mfset
colz <- as.character(unique(rf_values$rf_code))

yearmo_str <- as.integer(unique(rf_values$year_mo[order(rf_values$year_mo)]))

rv <- data.frame(matrix(NA,nrow=length(yearmo_str), ncol=(1+length(colz))))
	names(rv) <- c('year_mo',colz)
	rv$year_mo <- yearmo_str

for(y in 1:nrow(rv))  {
	year_mo_set <-  rf_values[rf_values$year_mo == rv$year_mo[y],c('rf_code','rf_value')]
	colz <- as.character(unique(year_mo_set$rf_code))
	for(cl in colz) rv[y,cl] <- as.numeric(year_mo_set[year_mo_set$rf_code==cl,'rf_value'][1])
}

for(cl in names(rv[2:length(names(rv))])) rv[,cl]<- mergeVectors(rv$year_mo,rv$year_mo,rv[,cl])
rv <- rv[order(rv$year_mo,decreasing=F),]
mfacs <- rv
mfacs <- mfacs[mfacs$year_mo<201406,]

ds.gvkey_terms <- data.frame(matrix(nrow=0,ncol=5))
	names(ds.gvkey_terms) <- c('gvkey','term','error_term','intercept','adj_r_sqr')
ds.gvkey_coeffs <- data.frame(matrix(nrow=0,ncol=6))
	names(ds.gvkey_coeffs) <- c('gvkey','term','rf_code','coeff','std_err','t_stat')

mf_cols <- names(mfacs)[names(mfacs)!='year_mo']
these_cols <- c('NGDP','RGDP')
mf_colz <- mf_cols[!is.element(mf_cols,these_cols)]

varset <- as.data.frame(matrix(nrow=0,ncol=2))
	names(varset) <- c('var','tstat')

nosig_vars <- as.data.frame(matrix(nrow=0,ncol=2))
	names(nosig_vars) <- c('gvkey','term')

gvkeys <- c('001161','011856','001045','063501','004503','005073','006008','275552','002968',
			'161897','007922','008446','004674','013988','009882','184996','015274','008007','011687')	
for(gvkey in gvkeys) {
	kdp_values <- oq(paste("select jc.gvkey, substring(convert(char,jc.data_date,112),1,6) as year_mo,
			(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 
				else -log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo
			from kdp_ts.dbo.kdp_jc5  jc, (
				select gvkey, year(data_date) as y_date, month(data_date) as m_date, max(data_date) as max_date
				from kdp_ts.dbo.kdp_jc5 where gvkey=",gvkey, "  group by year(data_date), month(data_date), gvkey
			) m where jc.gvkey=",gvkey," and jc.gvkey=m.gvkey and jc.data_date=m.max_date order by jc.data_date",sep=""))
		kdp_values$year_mo <- as.integer(kdp_values$year_mo)
		kdp_values$kdp_3mo <- as.numeric(kdp_values$kdp_3mo)
		
#---- now  RRF2 algo
	set <- kdp_values[kdp_values$year_mo>=min(mfacs$year_mo),]
	num_obs <- nrow(set)
	testset <- merge(set,mfacs,by.x='year_mo',by.y='year_mo',all.x=T)
	ds.gt <- ds.gvkey_terms
	ds.gc <- ds.gvkey_coeffs
	nsv <- nosig_vars
	
	thesevars <- varset
			NGDP <- as.numeric(summary(lm(as.formula(paste(term," ~ NGDP")),testset))[[9]])
			RGDP <- as.numeric(summary(lm(as.formula(paste(term," ~ RGDP")),testset))[[9]])
	if(NGDP>RGDP) mf_cols <- append(mf_colz,'NGDP')
	if(NGDP<RGDP) mf_cols <- append(mf_colz,'RGDP')

	for(z in 1:length(mf_cols)) {
			formula_str <- as.formula(paste(term," ~ ",mf_cols[z]))
			tstat <- abs(as.data.frame(summary(lm(formula_str,testset))[4])$coefficients.t.value[2])[[1]]
		if(!is.na(tstat)) if(as.numeric(tstat)>2) thesevars[(nrow(thesevars)+1),] <- cbind(mf_cols[z],tstat)
			}
		if(nrow(thesevars)==0) {		#----- no significant macrofactors
				nsv <- rbind(nsv,cbind(gvkey,term))
		next
		}
	thesevars$tstat <- as.numeric(thesevars$tstat)
	thesevars <- thesevars[order(thesevars$tstat,decreasing=T),]
	thesevars <- thesevars$var
	flag <- as.logical(length(thesevars))
		while(flag) {
		formula_str <- as.formula(paste(paste(term," ~ ",sep=""),paste(thesevars,sep="",collapse="+"),sep=""))
			thing <- lm(formula_str,testset)
				coefs <- as.data.frame(summary(thing)[4])
					coefs$mfac <- row.names(coefs)
				intercept <- coefs[1,1]
				coefs <- coefs[-1,]
		flag <- as.logical(nrow(coefs[abs(coefs[,3])<2,]))
			if(nrow(coefs)==1) {
				cat(paste("\n",gvkey," ",term," failed all iterations of the t.values > 2 rule\n",sep=""))
				flag <- (nrow(coefs)-1)
			} else {
				thesevars <- coefs[order(coefs[,4],decreasing=T),'mfac'][-1]
			}
		}
		sumthing <- summary(thing)
		ds.gt[(nrow(ds.gt)+1),] <- cbind(gvkey,term,
					sumthing['sigma'][[1]],
					intercept,
					sumthing['adj.r.squared'][[1]])

			gsset <- data.frame(matrix(nrow=nrow(coefs),ncol=6))
				names(gsset) <- names(ds.gvkey_coeffs)
				gsset$gvkey	<- 	gvkey
				gsset$term	<- 	term
				gsset$rf_code	<-	coefs$mfac
				gsset$coeff	<-	coefs[,1]
				gsset$std_err	<-	coefs[,2]
				gsset$t_stat	<-	coefs[,3]
		ds.gc <- rbind(ds.gc,gsset)

		ds.gt$gvkey <- as.character(ds.gt$gvkey)
		ds.gt$error_term <- as.numeric(ds.gt$error_term)
		ds.gt$intercept <- as.numeric(ds.gt$intercept)
		ds.gt$adj_r_sqr <- as.numeric(ds.gt$adj_r_sqr)

	if(nrow(ds.gt)>0) { final_gt_set <- rbind(final_gt_set,ds.gt) }
	if(nrow(ds.gc)>0) { final_gc_set <- rbind(final_gc_set,ds.gc) }
	}
timestamp()
#########################
#	063501 kdp_3mo failed all iterations of the t.values > 2 rule
#
#	004503 kdp_3mo failed all iterations of the t.values > 2 rule
#
#	275552 kdp_3mo failed all iterations of the t.values > 2 rule
#
#	161897 kdp_3mo failed all iterations of the t.values > 2 rule
#############################
final_new_kdp_set <- data.frame(matrix(ncol=4,nrow=13))
		names(final_new_kdp_set) <- c('scenario','period','gvkey',term)

scenarios <- c(52,53,54)
for(gvkey in final_gt_set$gvkey) {
	gc_set <- final_gc_set[final_gc_set$gvkey==gvkey,]
	gt_set <- final_gt_set[final_gt_set$gvkey==gvkey,]

	for(scenario in scenarios) {
		scenario_set <- oq(paste("select case rf_num when 50 then 'RGDP' when 51 then 'NGDP' when 54 then 'UNEMP'
							when 56 then 'TREAS3MO' else 'TREAS10Y' end as rf_code, period, rf_value from mfs.dbo.scenario_values 
							where scenario in(",
							scenario,
							") and rf_num in(50,51,54,56,57)",sep=""))
		scenario_set <- merge(scenario_set,gc_set[,c('rf_code','coeff')])
			scenario_set$rf_value <- as.numeric(scenario_set$rf_value)
			scenario_set$rf_value <- scenario_set$rf_value * scenario_set$coeff
    scenario_str <- NULL
	for(x in 1:13) scenario_str <- append(scenario_str,sum(scenario_set[scenario_set$period==x,'rf_value']))
#	scenario_str <- scenario_str + gt_set$intercept
#	scenario_str <- 1/exp(-1*scenario_str)*100
	new_kdp_set <- data.frame(matrix(ncol=4,nrow=13))
		names(new_kdp_set) <- c('scenario','period','gvkey',term)
	new_kdp_set$scenario=scenario
	new_kdp_set$period=1:13
	new_kdp_set$gvkey=gvkey
	new_kdp_set[,term] = scenario_str
	final_new_kdp_set <- rbind(final_new_kdp_set,new_kdp_set)
	}
}
final_new_kdp_set <- final_new_kdp_set[!is.na(final_new_kdp_set$gvkey),]




write.table(final_new_kdp_set,file="./schwab_kdps_set.txt",sep="\t",quote=T,row.names=F)
write.table(final_gc_set,file="./schwab_coeffs.txt",sep="\t",quote=T,row.names=F)
write.table(final_gt_set,file="./schwab_terms.txt",sep="\t",quote=T,row.names=F)

tahitiWriteTable(final_new_kdp_set,"therocrates.sch_stress_test")
tahitiWriteTable(final_gc_set,"therocrates.gvkey_coeffs_sch")
tahitiWriteTable(final_gt_set,"therocrates.gvkey_terms_sch")



select 54 as scenario, period, gvkey, 1/exp(-1*(sum(rf_value) + intercept))*100 as kdp from (
select mf.period, c.gvkey, mf.rf_code, mf.rf_value * c.coeff as rf_value, t.intercept
from therocrates.dbo.gvkey_coeffs_sch c join therocrates.dbo.gvkey_terms_sch t 
on t.gvkey=c.gvkey and t.term=c.term
join #MACRO_FACTORS mf on mf.rf_code=c.rf_code
where c.term='kdp_3mo')
m group by gvkey, period, intercept order by gvkey, period





#################
#	
#	select 54 as scenario, period, gvkey, 1/exp(-1*(sum(rf_value) + intercept))*100 as kdp from (
#		select mf.period, c.gvkey, mf.rf_code, mf.rf_value * c.coeff as rf_value, t.intercept
#		from therocrates.dbo.gvkey_coeffs_sch c join therocrates.dbo.gvkey_terms_sch t 
#			on t.gvkey=c.gvkey and t.term=c.term
#			join #MACRO_FACTORS mf on mf.rf_code=c.rf_code
#		where c.term='kdp_3mo' and c.gvkey='001161')
#		 m group by gvkey, period, intercept order by gvkey, period
#################

