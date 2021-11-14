Sys.setenv(R_HISTSIZE=999999)
mydir <- ifelse(Sys.getenv('COMPUTERNAME') == 'TED-PC','c:/therocrates','d:/KRIS_R')
Sys.setenv(HOME=mydir)
Sys.setenv(R_USER=mydir)
setwd(mydir)


#	cmd.exe /c "R < D:/KRIS_R/do_kris_regress.r id"  > d:/KRIS_R/tmp/outfile.out 2>&1"
args		=	commandArgs()[-(1:2)]
reg_id		=	as.integer(args[1])

reg_id		=	1
#-----	macrofactor data
rightside <- tq(paste("select factor, yearmo as year_mo, rf_value from mfs_q.dbo.run_factors where reg_id=",
					reg_id," order by yearmo",sep=""))
	rightside$factor <- as.character(rightside$factor)
	for(x in 2:3) rightside[,x] <- as.numeric(rightside[,x])

datez <- unique(rightside$year_mo)
colz <- c('year_mo',unique(rightside$factor))

mfacs <- data.frame(matrix(NA,ncol=1,nrow=length(datez)))
	names(mfacs) <- 'year_mo'
		mfacs$year_mo <- datez
	for(x in 2:length(colz)) {
		mfacs <- merge(mfacs,rightside[rightside$factor==colz[x],c('year_mo','rf_value')],all.x=T,all.y=F,by='year_mo')
		names(mfacs)[x] <- colz[x]
	}

#-----	portfolio data
leftside <- tq(paste("select name, yearmo as year_mo, name_value from mfs_q.dbo.run_names where reg_id=",
					reg_id," order by name,yearmo",sep=""))
	leftside$name <- as.character(leftside$name)
	for(x in 2:3) leftside[,x] <- as.numeric(leftside[,x])

rf_codes <- tq(paste("select rf.rf_code, rf.rf_num, sf.transform_code, sf.lag_code, m.reg_desc from 
		samoa.mfs3.dbo.risk_factor rf join samoa.mfs3.dbo.rf_set_factor sf on sf.rf_num=rf.rf_num
		join samoa.mfs3.dbo.regression m on m.rf_set=sf.rf_set where m.reg_id='",reg_id,"'",sep=""))

gvkeys <- unique(leftside$name)
lside <- 'name_value'

#-----	now we loop on uniques in the portfolio 
for(gvkey in gvkeys) {
cat("\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t",gvkey,"\n")
gvkey <- gvkeys[1]
	winning_formulas <- list(NULL)
	winning_rsqs <- list(NULL) 
	gvkeyset <- leftside[leftside$name==gvkey,c(2:3)]

	#----- this is our dataset for regressions
	testset <- merge(gvkeyset,mfacs)
	if(nrow(testset)<(length(colz)-2)) next
		lmtr <- trunc(nrow(testset)*.5)
		lmtr_flag <- FALSE

		wf <- NULL
		wrsq <- 0
		lwf <- wf

		frmla <- as.formula(paste(lside," ~ ",paste(colz[-1],sep="",collapse="+"),sep=""))

		lset <- summary(lm(frmla,testset))
		tscrs <- abs(lset$coefficients[,3][-1])
		trythese <- names(tscrs)[order(tscrs,decreasing=T)][-1]
		wf <- names(tscrs)[order(tscrs,decreasing=T)][1]
		lset <- summary(lm(as.formula(paste("name_value ~ ",paste(wf,sep="",collapse="+"),sep="")),testset))
		wrsq <- as.numeric(lset$adj.r.squared)

		wf_wrsq <- list(lset,wf,wrsq)

		TRYS=10
		qqq <- 1

				while(qqq) {
					trythese <- colz[!is.element(colz,wf_wrsq[[2]])][-1]
					wf_wrsq <- runthese(trythese,wf) #----- this returns lset, wf and wrsq
					lset <- wf_wrsq[[1]]
					wf <- wf_wrsq[[2]]
					wrsq <- wf_wrsq[[3]]
				winning_formulas <- append(winning_formulas,paste(wf,sep="",collapse="+"))
				winning_rsqs <- append(winning_rsqs,paste(wrsq))
						if(qqq>1) if(all(is.element(wf,lwf)) & all(is.element(lwf,wf))) qqq=FALSE
						if(qqq==TRYS) qqq=FALSE	
						if(qqq) qqq=qqq+1
						if(qqq>1) lwf=wf
				}

		winning_formulas <- unique(unlist(winning_formulas[-1]))
		winning_rsqs <- unique(unlist(winning_rsqs[-1]))
		#-----	we have formulas or estimated models, now lets run diagnostics

				for(x in 1:length(winning_formulas)) {
cat("\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t",x,"\n")
					mod_id <- x
					wf <- winning_formulas[x]
					
					frmla <- as.formula(paste(lside," ~ ",wf,sep="",collapse=""))
					EM <- lm(lm(frmla,testset))				#	Estimated Model
					LSET	<- summary(EM) 					#	LSET
					RSE		<- coeftest(EM,vcovHC(EM))  	#	Robust Standard Errors (Heteroscedasticity and Autocorrelation Consistent - HAC)
					IC 		<- c(AIC(EM),BIC(EM)) 			#	Akaike's Information Criterion and Schwarz's Bayesian Information Criterion
					GA 		<- summary(gvlma(EM)) 			#	Global Model Assessment
					MC 		<- vif(EM)						#	Multicollinearity
			#----- Residual Analysis
					BP 		<- bptest(EM)					#	Breusch-Pagan test for homoskedasticity
					GQ 		<- gqtest(EM)					#	Goldfeld-Quandt test for homoskedasticity
					DQ 		<- durbinWatsonTest(EM,4)		#	Durbin-Watson test for first-order serial correlation
					BG 		<- bgtest(EM, order=12)			#	Breusch-Godfrey Test for higher order serial correlation
			#-----	Assessing Outliers
					OT 		<- outlierTest(EM)				#	Bonferonni p-value for most extreme obs
			#-----	Assesing Normality of Residuals
					SW <- shapiro.test(residuals(EM))		#	Shapiro-Wilk test for normality
					SF <- sfTest(residuals(EM))				#	Shapiro-Francia test for normality
				
					rf_names <- row.names(LSET$coefficients)[-1][order(row.names(LSET$coefficients)[-1])]
			reg_detail_set <- data.frame(matrix(nrow=length(rf_names),ncol=18))
					names(reg_detail_set) <- c('reg_id','reg_name','mod_id','rf_num','transform_code','lag_code','coeff','std_err',
											't_stat','p_value','l95b','u95b','hc_stderr','hc_tstat','hc_probt','vif_rank',
											'sf_resid_normality','rf_code')
					reg_detail_set$reg_id				<-	as.integer(reg_id)
					reg_detail_set$reg_name				<-	gvkey
					reg_detail_set$mod_id				<-	as.integer(mod_id)
					reg_detail_set$rf_code				<-	rf_names
					for(z in 1:nrow(reg_detail_set)) {
						reg_detail_set$rf_num[z] 			<- as.integer(rf_codes[rf_codes$rf_code==reg_detail_set$rf_code[z],'rf_num'])
						reg_detail_set$transform_code[z]	<- rf_codes[rf_codes$rf_code==reg_detail_set$rf_code[z],'transform_code']
						reg_detail_set$lag_code[z]			<- rf_codes[rf_codes$rf_code==reg_detail_set$rf_code[z],'lag_code']
					}
					reg_detail_set$coeff				<-	LSET$coefficients[-1,1][order(row.names(LSET$coefficients)[-1])]
						reg_detail_set$coeff			<-	e_notation_to_decimal(reg_detail_set$coeff)
					reg_detail_set$std_err				<-	as.character(LSET$coefficients[-1,2][order(row.names(LSET$coefficients)[-1])])
						reg_detail_set$std_err 			<-	e_notation_to_decimal(reg_detail_set$std_err)
					reg_detail_set$t_stat				<-	LSET$coefficients[-1,3][order(row.names(LSET$coefficients)[-1])]
					reg_detail_set$p_value				<-	as.character(LSET$coefficients[-1,4][order(row.names(LSET$coefficients)[-1])])
						reg_detail_set$p_value			<-	e_notation_to_decimal(reg_detail_set$p_value)
					reg_detail_set$l95b					<-	NA
					reg_detail_set$u95b					<-	NA
					reg_detail_set$hc_stderr			<-	as.character(RSE[-1,2][order(row.names(RSE)[-1])])
						reg_detail_set$hc_stderr		<-	e_notation_to_decimal(reg_detail_set$hc_stderr)
					reg_detail_set$hc_tstat				<-	RSE[-1,3][order(row.names(RSE)[-1])]
					reg_detail_set$hc_probt				<-	as.character(RSE[-1,4][order(row.names(RSE)[-1])])
						reg_detail_set$hc_probt			<-	e_notation_to_decimal(reg_detail_set$hc_probt)
					reg_detail_set$vif_rank				<-	MC[order(names(MC))]
					reg_detail_set$sf_resid_normality	<-	NA #-----	sf_resid_normality does not seem to offer a value varible, but just one value, see SF and SW
#----- we need to run on each input once for each gvkey then bring down here
#---- sfTest(testset$TREAS10Y)
			reg_detail_set <- reg_detail_set[,-18]

			reg_master_set <- data.frame(matrix(nrow=1,ncol=18))
					names(reg_master_set) <- c('reg_id','reg_name','mod_id','reg_p_value','adj_r_sqr','error','aic','bic','vif_mean',
											'bp_hetero_lm','dh_sercorr_lag1','dh_sercorr_lag2','dh_sercorr_lag3','dh_sercorr_lag4',
											'bg_sercorr_lag1','bg_sercorr_lag2','bg_sercorr_lag3','bg_sercorr_lag4')
					reg_master_set$reg_id				<-	as.integer(reg_id)
					reg_master_set$reg_name				<-	gvkey
					reg_master_set$mod_id				<-	as.integer(mod_id)
					reg_master_set$reg_p_value			<-  as.character(pf(LSET$fstatistic[1], LSET$fstatistic[2], LSET$fstatistic[3],lower.tail=F))
						reg_master_set$reg_p_value		<-	e_notation_to_decimal(reg_master_set$reg_p_value)
					reg_master_set$adj_r_sqr			<-	LSET$adj.r.squared 
					reg_master_set$error				<-	LSET$sigma
					reg_master_set$aic					<-	IC[1]
					reg_master_set$bic					<-	IC[2]
					reg_master_set$vif_mean 			<-	mean(MC)
					reg_master_set$bp_hetero_lm			<-	BP$statistic
					reg_master_set$dh_sercorr_lag1		<-	DQ$p[1]
					reg_master_set$dh_sercorr_lag2		<-	DQ$p[2]
					reg_master_set$dh_sercorr_lag3		<-	DQ$p[3]
					reg_master_set$dh_sercorr_lag4		<-	DQ$p[4]
					reg_master_set$bg_sercorr_lag1		<-	bgtest(EM,order=1)$p.value
					reg_master_set$bg_sercorr_lag2		<-	bgtest(EM,order=2)$p.value
					reg_master_set$bg_sercorr_lag3		<-	bgtest(EM,order=3)$p.value
					reg_master_set$bg_sercorr_lag4		<-	bgtest(EM,order=4)$p.value

			reg_residual_set <- data.frame(matrix(nrow=nrow(testset),ncol=5))
					names(reg_residual_set) <- c('reg_id','reg_name','mod_id','yearmo','residual')
					reg_residual_set$reg_id				<-	reg_id
					reg_residual_set$reg_name			<-	gvkey
					reg_residual_set$mod_id				<-	mod_id
					reg_residual_set$yearmo				<-	testset$year_mo
					reg_residual_set$residual			<-	LSET$residuals
						reg_residual_set$residual		<-	e_notation_to_decimal(reg_residual_set$residual)
			#-----	write our outputs to the datagbvase
			for(zz in 1:nrow(reg_residual_set)) tq(paste("insert mfs_q.dbo.mfs_reg_residual values('",paste(reg_residual_set[zz,],sep="",collapse="','"),"')",sep=""))
			tq(gsub("'NA'","NULL",paste("insert mfs_q.dbo.mfs_reg_master values('",paste(reg_master_set[1,],sep="",collapse="','"),"')",sep="")))
			for(zz in 1:nrow(reg_detail_set)) tq(gsub("'NA'","NULL",paste("insert mfs_q.dbo.mfs_reg_detail values('",paste(reg_detail_set[zz,],sep="",collapse="','"),"')",sep="")))
					
		}
}

