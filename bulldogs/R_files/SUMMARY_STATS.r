summary_stats <<- function(winning_formulas,testset) {

#----- diagnostics
	model_inputs <- unique(unlist(strsplit(unlist(winning_formulas),"+",fixed=T)))
	model_inputs <- model_inputs[!is.na(model_inputs)]
	model_inputs <- model_inputs[order(model_inputs)]
	if(length(model_inputs)<1){
		cat("\tError in model inputs -there are no model inputs\n")
	}
	sf_resid_normality <- data.frame(matrix(nrow=length(model_inputs),ncol=2))
		names(sf_resid_normality) <- c('rf_code','item_value')
		sf_resid_normality$rf_code = model_inputs
	for(x in 1:length(model_inputs)) {
		trycatcher <- try(sf_resid_normality[x,2] <- sfTest(testset[,model_inputs[x]])@test$statistic)
		if(is(trycatcher,'try-error')) cat("\tError in sfTest\n")
	}


#----- upper and lower bounds
	bounds <- data.frame(matrix(nrow=length(model_inputs),ncol=3))
		names(bounds) <- c('rf_code','ub','lb')
	for(x in 1:length(model_inputs)){
		trycatcher <- try(conf_intervals	<- CI(testset[,model_inputs[x]][!is.na(testset[,model_inputs[x]])],ci=0.95))
		if(is(trycatcher,'try-error')) { 
			cat("\tError in CI confidence intervals\n")
		} else { 
			bounds[x,1] 	<- model_inputs[x]
			bounds[x,2] 	<- conf_intervals[1]
			bounds[x,3] 	<- conf_intervals[3]
		}
	}
#-----	we have formulas / estimated models, now lets run diagnostics
	for(x in 1:length(winning_formulas)) {
		if(is.na(winning_formulas)[x]) return()
			mod_id <- model_ids[x]
			wf <<- unlist(winning_formulas[x])
			frmla <- as.formula(paste(lside," ~ ",wf,sep="",collapse=""))
			EM <<- lm(lm(frmla,testset))				#	Estimated Model
			LSET	<- summary(EM) 					#	LSET
			RSE		<- coeftest(EM,vcovHC(EM))  		#	Robust Standard Errors (Heteroscedasticity and Autocorrelation Consistent - HAC)
			IC 		<- c(AIC(EM),BIC(EM)) 			#	Akaike's Information Criterion and Schwarz's Bayesian Information Criterion
				if(length(strsplit(wf,'+',T)[[1]])>1) 	{ MC <- vif(EM) } else { MC <- NA }
			BP 		<- bptest(EM)				#	Breusch-Pagan test for homoskedasticity
			DQ 		<- durbinWatsonTest(EM,4)		#	Durbin-Watson test for first-order serial correlation	

					# GA <- summary(gvlma(EM)) 		#	Global Model Assessment
					# GQ <- gqtest(EM)			#	Goldfeld-Quandt test for homoskedasticity
					# OT <- outlierTest(EM)			#	Bonferonni p-value for most extreme obs
					# SW <- shapiro.test(residuals(EM))	#	Shapiro-Wilk test for normality	or residuals	NOT USING THIS ONE
					# SF <- sfTest(residuals(EM))		#	Shapiro-Francia test for normality
	}

rf_codes_tmp <- rf_codes[is.element(rf_codes$rf_code,row.names(LSET$coefficients)),]
	rf_names <- rf_codes_tmp$rf_code
		reg_detail_set <- data.frame(matrix(nrow=length(rf_names),ncol=17))
	names(reg_detail_set) <- c('reg_id','reg_name','rf_num','transform_code','lag_code','coeff','std_err','t_stat','p_value','l95b','u95b','hc_stderr','hc_tstat','hc_probt','vif_rank','sf_resid_normality','rf_code')
	reg_detail_set$reg_id	<-	as.integer(reg_id)
	reg_detail_set$reg_name	<-	gvkey
	reg_detail_set$rf_code	<-	rf_names
	reg_detail_set$rf_num	<-	rf_codes_tmp$rf_num
	reg_detail_set$transform_code			<-	rf_codes_tmp$transform_code
	reg_detail_set$lag_code	<-	rf_codes_tmp$lag_code
	if(!is.na(MC[[1]])) for(z in 1:nrow(reg_detail_set)) reg_detail_set$vif_rank[z] <- MC[names(MC) == reg_detail_set$rf_code[z]]
	for(z in 1:nrow(reg_detail_set)) {
		reg_detail_set$coeff[z]		<-	round(LSET$coefficients[row.names(LSET$coefficients)==reg_detail_set$rf_code[z],'Estimate'],6)
		reg_detail_set$std_err[z]	<-	round(LSET$coefficients[row.names(LSET$coefficients)==reg_detail_set$rf_code[z],'Std. Error'],6)
		reg_detail_set$t_stat[z]	<-	round(LSET$coefficients[row.names(LSET$coefficients)==reg_detail_set$rf_code[z],'t value'],6)
		reg_detail_set$p_value[z]	<-	round(LSET$coefficients[row.names(LSET$coefficients)==reg_detail_set$rf_code[z],'Pr(>|t|)'],6)
		reg_detail_set$l95b[z]		<-	round(bounds[bounds$rf_code==reg_detail_set$rf_code[z],3],6)
		reg_detail_set$u95b[z]		<-	round(bounds[bounds$rf_code==reg_detail_set$rf_code[z],2],6)
		reg_detail_set$hc_stderr[z]	<-	round(RSE[row.names(RSE)==reg_detail_set$rf_code[z],'Std. Error'],6)
		reg_detail_set$hc_tstat[z]	<-	round(RSE[row.names(RSE)==reg_detail_set$rf_code[z],'t value'],6)
		reg_detail_set$hc_probt[z]	<-	round(RSE[row.names(RSE)==reg_detail_set$rf_code[z],'Pr(>|t|)'],6)
		reg_detail_set$sf_resid_normality[z]	<-	round(sf_resid_normality[sf_resid_normality$rf_code==reg_detail_set$rf_code[z],2],6)
	}

	reg_detail_set <- reg_detail_set[,-17]

	rn <- (nrow(reg_detail_set)+1)
			reg_detail_set[rn,'reg_id'] <- reg_id
			reg_detail_set[rn,'reg_name'] <- gvkey
			reg_detail_set[rn,'rf_num'] <- 0
			reg_detail_set[rn,'transform_code'] <- NA
			reg_detail_set[rn,'lag_code'] <- NA
			reg_detail_set[rn,'coeff'] <- round(LSET$coefficients[1,'Estimate'],6)
			reg_detail_set[rn,'std_err'] <- round(LSET$coefficients[1,'Std. Error'],6)
			reg_detail_set[rn,'t_stat'] <- round(LSET$coefficients[1,'t value'],6)
			reg_detail_set[rn,'p_value'] <- round(LSET$coefficients[1,'Pr(>|t|)'],6)
			reg_detail_set[rn,'l95b'] <- NA
			reg_detail_set[rn,'u95b'] <- NA
			reg_detail_set[rn,'hc_stderr'] <- NA
			reg_detail_set[rn,'hc_tstat'] <- NA
			reg_detail_set[rn,'hc_probt'] <- NA
			reg_detail_set[rn,'vif_rank'] <- NA
			reg_detail_set[rn,'sf_resid_normality'] <- NA

	reg_master_set <- data.frame(matrix(nrow=1,ncol=22))
		names(reg_master_set) <- c('reg_id','reg_name','reg_p_value','adj_r_sqr','error','aic','bic','vif_mean','bp_hetero_lm',
				'dh_sercorr_lag1','dh_sercorr_lag2','dh_sercorr_lag3','dh_sercorr_lag4','bg_sercorr_lag1','bg_sercorr_lag2',
				'bg_sercorr_lag3','bg_sercorr_lag4','num_obs','num_dof','dof','f_value','r_sqr')
		reg_master_set$reg_id			<-	as.integer(reg_id)
		reg_master_set$reg_name			<-	gvkey
		reg_master_set$reg_p_value		<-  round(pf(LSET$fstatistic[1], LSET$fstatistic[2], LSET$fstatistic[3],lower.tail=F),6)
		reg_master_set$adj_r_sqr		<-	round(LSET$adj.r.squared,6)
		reg_master_set$error			<-	round(LSET$sigma,6)
		reg_master_set$aic			<-	round(IC[1],6)
		reg_master_set$bic			<-	round(IC[2],6)
		reg_master_set$vif_mean 		<-	round(mean(MC),6)
		reg_master_set$bp_hetero_lm		<-	round(BP$p.value,6)
		reg_master_set$dh_sercorr_lag1		<-	round(DQ$p[1],6)
		reg_master_set$dh_sercorr_lag2		<-	round(DQ$p[2],6)
		reg_master_set$dh_sercorr_lag3		<-	round(DQ$p[3],6)
		reg_master_set$dh_sercorr_lag4		<-	round(DQ$p[4],6)
		reg_master_set$bg_sercorr_lag1		<-	NA
			bg_sercorr_lag1 <- try(bgtest(EM, order=1))
				if(!is(bg_sercorr_lag1,'try-error')) reg_master_set$bg_sercorr_lag1 <- round(bg_sercorr_lag1$p.value,6)
		reg_master_set$bg_sercorr_lag2		<-	NA
			bg_sercorr_lag2 <- try(bgtest(EM, order=2))
				if(!is(bg_sercorr_lag2,'try-error')) reg_master_set$bg_sercorr_lag2 <- round(bg_sercorr_lag2$p.value,6)
		reg_master_set$bg_sercorr_lag3		<-	NA
			bg_sercorr_lag3 <- try(bgtest(EM, order=3)) 
				if(!is(bg_sercorr_lag3,'try-error')) reg_master_set$bg_sercorr_lag3 <- round(bg_sercorr_lag3$p.value,6)
		reg_master_set$bg_sercorr_lag4		<-	NA
			bg_sercorr_lag4 <- try(bgtest(EM, order=4))
				if(!is(bg_sercorr_lag4,'try-error')) reg_master_set$bg_sercorr_lag4 <- round(bg_sercorr_lag4$p.value,6)

		reg_master_set$num_obs	<-	nrow(testset)
		reg_master_set$num_dof	<-	LSET$fstatistic[2]
		reg_master_set$dof		<-	LSET$fstatistic[3]
		reg_master_set$f_value	<-	LSET$fstatistic[1]
		reg_master_set$r_sqr	<-	LSET$r.squared

		reg_residual_set <- data.frame(matrix(nrow=length(LSET$residuals),ncol=4))
	names(reg_residual_set) <- c('reg_id','reg_name','yearmo','residual')
	reg_residual_set$reg_id	<-	reg_id
	reg_residual_set$reg_name			<-	gvkey
	reg_residual_set$yearmo	<-	testset$year_mo[as.integer(names(LSET$residuals))]
	reg_residual_set$residual			<-	round(LSET$residuals,6)
	
		#-----	write our outputs to the Data Base
	for(zz in 1:nrow(reg_residual_set)) fq(paste("insert mfsq.dbo.mfs_reg_residual values('",paste(reg_residual_set[zz,],sep="",collapse="','"),"')",sep=""))
	fq(gsub("'<NA>'","NULL",gsub("'NA'","NULL",paste("insert mfsq.dbo.mfs_reg_master values('",paste(reg_master_set[1,],sep="",collapse="','"),"')",sep=""))))
	for(zz in 1:nrow(reg_detail_set)) fq(gsub("'NaN'","NULL",gsub("'NA'","NULL",paste("insert mfsq.dbo.mfs_reg_detail values('",paste(reg_detail_set[zz,],sep="",collapse="','"),"')",sep=""))))

}