summary_stats_frac <<- function(winning_formulas,testset) {		#----- diagnostics

	winning_formulas <- winning_formulas[[1]]
		if(is.na(winning_formulas[1])) {
			writeLog(reg_id,gvkey,NA,'E','no_model_inputs')
	return()		}
	model_inputs <- unique(unlist(strsplit(unlist(winning_formulas),"+",fixed=T)))
	model_inputs <- model_inputs[!is.na(model_inputs)]
	model_inputs <- model_inputs[order(model_inputs)]

		if(length(model_inputs)==1){
			writeLog(reg_id,gvkey,NA,'E','no_model_inputs')
	return()
		}

	sf_resid_normality <- data.frame(matrix(nrow=length(model_inputs),ncol=2))
		names(sf_resid_normality) <- c('rf_code','item_value')
		sf_resid_normality$rf_code = model_inputs

		for(x in 1:length(model_inputs)) {
			trycatcher <- try(sf_resid_normality[x,2] <- sfTest(testset[,model_inputs[x]])@test$statistic)
				if(is(trycatcher,'try-error')) {
					sf_resid_normality[x,2] <- NA
					writeLog(reg_id,gvkey,model_inputs[x],'W','SFTEST_error')
				}
		}

	bounds <- data.frame(matrix(nrow=length(model_inputs),ncol=3))
		names(bounds) <- c('rf_code','ub','lb')
		bounds$rf_code = model_inputs

		for(x in 1:length(model_inputs)){
			trycatcher <- try(conf_intervals <- CI(testset[,model_inputs[x]][!is.na(testset[,model_inputs[x]])],ci=0.95))
				if(is(trycatcher,'try-error')) { 
					writeLog(reg_id,gvkey,model_inputs[x],'W','CI_error')
					bounds[x,2] 	<- NA
					bounds[x,3] 	<- NA
				} else { 
					bounds[x,2] 	<- conf_intervals[1]
					bounds[x,3] 	<- conf_intervals[3]
				}
		}

	wf <- strsplit(winning_formulas,'\\+')[[1]]
			if(length(wf)==0) {
				writeLog(reg_id,gvkey,NA,'E','fail_frm_call')	
		return()
			}
			thistestset <- testset[,c('year_mo',lside,wf)]
			keep_rows <- ''
			for(x in 1:nrow(thistestset)) if(!sum(is.na(thistestset[x,2:(ncol(thistestset))]))) keep_rows <- append(keep_rows,x)
			keep_rows <- keep_rows[-1]
			thistestset <- thistestset[keep_rows,]
			if(nrow(thistestset)<12) {
				writeLog(reg_id,gvkey,NA,'W','fail_min_rows_test')
		return()
			}

			if(length(wf)>nrow(thistestset)*.5) {
				wf <- wf[1:((nrow(thistestset)*.5)-1)]
			}
			xx=as.data.frame(thistestset[,c(wf)])
				names(xx) <- wf
			y=thistestset[,lside]
			frm_res <- try(frm(y=y,x=xx,linkfrac='logit',type='1P',intercept=T,table=F,variance=T))
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
			Tscores		<- (frm_res$p/sqrt(diag(frm_res$p.var)))
			p_rsq		<- 1-ll/(num_obs*(ybar*log(ybar) + (1-ybar)*log(1-ybar)))
			pd_rsq		<- 1-(sum((y-yhat)^2)/sum((y-ybar)^2))
			pd_adj_rsq <-  1-((1-pd_rsq)*(num_obs-1) /(num_obs-dof-1))
			llo			<- num_obs*(ybar*log(ybar)+(1-ybar)*log(1-ybar))
			p_rsq		<- 1-ll/llo
			rmse		<- sqrt(sum((y-yhat)^2)/num_obs)

		
			coeffs <- frm_res$p
			#	Estimated Model Diagnostics -these work with lm() objects NOT frm() objects
			EM <- NA
			LSET <- NA
			RSE <- NA
			IC <- NA
			MC <- NA
			BP <- NA
			DQ <- NA

			rf_codes_tmp <- rf_codes[is.element(rf_codes$rf_code,frm_res$x.names[-1]),]
			reg_detail_set <- data.frame(matrix(nrow=nrow(rf_codes_tmp),ncol=17))
			names(reg_detail_set) <- c('reg_id','reg_name','rf_num','transform_code','lag_code',
										'coeff','std_err','t_stat','p_value','l95b','u95b','hc_stderr',
										'hc_tstat','hc_probt','vif_rank','sf_resid_normality','rf_code')
				reg_detail_set$reg_id				<-	as.integer(reg_id)
				reg_detail_set$reg_name				<-	gvkey
				reg_detail_set$rf_code				<-	rf_codes_tmp$rf_code
				reg_detail_set$rf_num				<-	rf_codes_tmp$rf_num
				reg_detail_set$transform_code		<-	rf_codes_tmp$transform_code
				reg_detail_set$lag_code				<-	rf_codes_tmp$lag_code
				reg_detail_set$vif_rank <- MC

			for(z in 1:nrow(reg_detail_set)) {
				reg_detail_set$coeff[z]		<-	coeffs[names(coeffs)==reg_detail_set$rf_code[z]][[1]]
				reg_detail_set$std_err[z]	<-	NA
				reg_detail_set$t_stat[z]	<-	Tscores[names(Tscores)==reg_detail_set$rf_code[z]][[1]]
				reg_detail_set$p_value[z]	<-	NA
				reg_detail_set$l95b[z]		<-	round(bounds[bounds$rf_code==reg_detail_set$rf_code[z],3],6)
				reg_detail_set$u95b[z]		<-	round(bounds[bounds$rf_code==reg_detail_set$rf_code[z],2],6)
				reg_detail_set$sf_resid_normality[z]		<-	round(sf_resid_normality[sf_resid_normality$rf_code==reg_detail_set$rf_code[z],2],6)
			}
				reg_detail_set$hc_stderr[z]	<-	RSE
				reg_detail_set$hc_tstat[z]	<-	RSE
				reg_detail_set$hc_probt[z]	<-	RSE

			reg_detail_set <- reg_detail_set[,-17]
				rn <- (nrow(reg_detail_set)+1)

				reg_detail_set[rn,'reg_id'] <- reg_id
				reg_detail_set[rn,'reg_name'] <- gvkey
				reg_detail_set[rn,'rf_num'] <- 0
				reg_detail_set[rn,'transform_code'] <- NA
				reg_detail_set[rn,'lag_code'] <- NA
				reg_detail_set[rn,'coeff'] <- intercept
				reg_detail_set[rn,'std_err'] <- NA
				reg_detail_set[rn,'t_stat'] <- Tscores[1]
				reg_detail_set[rn,'p_value'] <- NA
				reg_detail_set[rn,'l95b'] <- NA
				reg_detail_set[rn,'u95b'] <- NA
				reg_detail_set[rn,'hc_stderr'] <- NA
				reg_detail_set[rn,'hc_tstat'] <- NA
				reg_detail_set[rn,'hc_probt'] <- NA
				reg_detail_set[rn,'vif_rank'] <- NA
				reg_detail_set[rn,'sf_resid_normality'] <- NA

			reg_master_set <- data.frame(matrix(nrow=1,ncol=31))
				names(reg_master_set) <- c('reg_id','reg_name','reg_p_value','num_obs','num_dof','dof','f_value','r_sqr',
					'adj_r_sqr','error','aic','bic','vif_mean','bp_hetero_lm','dh_sercorr_lag1','dh_sercorr_lag2',
					'dh_sercorr_lag3','dh_sercorr_lag4','bg_sercorr_lag1','bg_sercorr_lag2','bg_sercorr_lag3',
					'bg_sercorr_lag4','wald_chi_sqr','prob_gt_chi_sqr','pseudo_r_sqr','log_psuedo_lhood','pd_r_sqr',
					'pd_adj_r_sqr','pd_error','pd_aic','pd_bic')

							reg_master_set$reg_id					<- as.integer(reg_id)
							reg_master_set$reg_name					<- gvkey
							reg_master_set$reg_p_value				<- NA
							reg_master_set$num_obs					<- num_obs
							reg_master_set$num_dof					<- num_obs
							reg_master_set$dof						<- dof
							reg_master_set$f_value					<- NA
							reg_master_set$r_sqr					<- NA
							reg_master_set$adj_r_sqr				<- NA
							reg_master_set$error					<- NA
							reg_master_set$aic						<- NA
							reg_master_set$bic						<- NA
							reg_master_set$vif_mean					<- NA
							reg_master_set$bp_hetero_lm				<- NA
							reg_master_set$dh_sercorr_lag1			<- NA
							reg_master_set$dh_sercorr_lag2			<- NA
							reg_master_set$dh_sercorr_lag3			<- NA
							reg_master_set$dh_sercorr_lag4			<- NA
							reg_master_set$bg_sercorr_lag1			<- NA
							reg_master_set$bg_sercorr_lag2			<- NA
							reg_master_set$bg_sercorr_lag3			<- NA
							reg_master_set$bg_sercorr_lag4			<- NA
							reg_master_set$wald_chi_sqr				<- NA
							reg_master_set$prob_gt_chi_sqr			<- NA
							reg_master_set$pseudo_r_sqr				<- p_rsq
							reg_master_set$log_psuedo_lhood			<- ll
							reg_master_set$pd_r_sqr					<- pd_rsq
							reg_master_set$pd_adj_r_sqr				<- pd_adj_rsq
							reg_master_set$pd_error					<- rmse
							reg_master_set$pd_aic					<- AIC
							reg_master_set$pd_bic					<- BIC

		fq(gsub("'<NA>'","NULL",gsub("'NA'","NULL",paste("insert mfsq.dbo.mfs_reg_master (",
			paste(names(reg_master_set)[!is.na(reg_master_set[1,])],sep="",collapse=","),
			")values('",paste(reg_master_set[!is.na(reg_master_set[1,])],sep="",collapse="','"),
			"')",sep=""))))
		for(zz in 1:nrow(reg_detail_set)) fq(gsub("'NaN'","NULL",gsub("'NA'","NULL",paste("insert mfsq.dbo.mfs_reg_detail ",
			"(reg_id,reg_name,rf_num,transform_code,lag_code,coeff,std_err,t_stat,p_value,l95b,u95b,hc_std_err,hc_t_stat,hc_probt,vif_rank,sf_resid_normality)",
			"values('",paste(reg_detail_set[zz,],sep="",collapse="','"),"')",sep=""))))
return()
}