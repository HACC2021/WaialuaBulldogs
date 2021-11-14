options(scipen=999)
options(warn=-99)
source("D:/KRIS_R/R_utils.r")
source("D:/KRIS_R/SUMMARY_STATS_frac.r")

#-----		cmd.exe /c "D:/KRIS_R/R < D:/KRIS_R/KRIS_REGRESS_FRAC.r 1 > d:/KRIS_R/tmp/outfile.out 2>&1"
args		=	commandArgs()[-(1:2)]
reg_id		<<-	as.integer(args[1])	#-----	reg_id <<- 338	
#----- run_factors
frm_sinkfile<<-format(Sys.time(), "%Y%j%H%M%S")
mfacs <<- fq(paste('exec mfsq.dbo.get_regress_macro_factors ',reg_id,sep=""))

#----- test these regress algos
reg_id_stuff <<- fq(paste("select r.*, s.sig_value from mfsq.dbo.mfs_regression r join NAURU.MFS3.dbo.sig_level s on s.sig_text=r.sig_text and reg_id='",reg_id,"'",sep=""))
for(x in 1:ncol(reg_id_stuff)) reg_id_stuff[,x] <- as.character(reg_id_stuff[,x])
algo_id <<- as.integer(reg_id_stuff$algo_id)[[1]]
TVAL <<- as.numeric(reg_id_stuff$sig_value)
REG_TYPE <<- ifelse(is.na(reg_id_stuff$reg_type[1]), 'LINEAR', reg_id_stuff$reg_type[1])

if(length(mfacs)==0 ) {
	writeLog(reg_id,NA,NA,'E','no_model_inputs')
} else if(nrow(mfacs)<12) {
	writeLog(reg_id,NA,NA,'E','less_than_12_obs')
} else {
		for(x in 2:ncol(mfacs)) mfacs[,x] <- as.numeric(mfacs[,x])
		mfacs$year_mo <- as.integer(mfacs$year_mo)
		names(mfacs)[2:ncol(mfacs)] <- toupper(gsub('_non_non','',names(mfacs)[2:ncol(mfacs)]))

	#-----	run_names
		leftside <<- fq(paste("select name, yearmo as year_mo, name_value from mfsq.dbo.run_names where reg_id=",reg_id," order by name,yearmo",sep=""))
			leftside$name <- as.character(leftside$name)
			for(x in 2:3) leftside[,x] <- as.numeric(leftside[,x])

		rf_codes <- fq(paste("select distinct factor+'_'+transform_code+'_'+lag_code as rf_code, rf_num, transform_code, lag_code, rf_group from mfsq.dbo.run_factors where reg_id='",reg_id,"' order by rf_num",sep=""))
		rf_codes$rf_code <- sub('_NON_NON','',rf_codes$rf_code)

		gvkeys <- unique(leftside$name)
		lside <<- 'name_value'

#-----	now we loop on uniques in the portfolio of run_names
for(gvkey in gvkeys) {
	gvkey <<- gvkey
		cat("\t",gvkey,"\t")

		winning_formulas <<- list(NULL)
		gvkeyset <<- leftside[leftside$name==gvkey,c(2:3)]

#----- lets build and check our dataset for regressions
		testset <- merge(gvkeyset,mfacs)
		names(testset) <- gsub(' ','',names(testset))

		if(nrow(testset)<12) {
			writeLog(reg_id,NA,NA,'E','less_than_12_obs')
		next
		}

		lmtr			<- trunc(nrow(testset)*.5)
		lmtr_flag		<- FALSE
		factorz_lmtd	<- NULL

		notthese <- ""
		for(zz in 3:ncol(testset)) { 
			if(sum(!is.na(testset[,zz])) < 12){
				notthese <- append(notthese,names(testset)[zz])
				writeLog(reg_id,gvkey,names(testset)[zz],'W','fail_12_non_null')
			}
		}
		testset <- testset[,!is.element(names(testset),notthese)]

		if(length(names(testset)[c(-1,-2)])>1) {
			colz <- try(fix_collinearity(names(testset)[c(-1,-2)]))
		} else {
			colz <- names(testset)[c(-1,-2)]
		}	
		if(is(colz,'try-error')) {
			writeLog(reg_id,gvkey,NA,'E','fail_fix_collinearity')
		next
		}
	
testset <<- testset[,c(lside,colz)]

		lmtr			<- trunc(nrow(testset)*.5)
		lmtr_flag		<- FALSE
		factorz_lmtd	<- NULL

#-----	now we have our testset
#-----	first we find our starting factor and order model inputs by tscores descending
		factorz <- names(testset)[c(-1)]
		if(length(factorz)>lmtr) {
			ftscrs <- NULL
			for(x in 1:length(factorz)) {
				wf_wrsq	<- frm_call(lside,factorz[x])
				if(is(wf_wrsq,'try-error')) {
					ftscrs[x] <- 0
				next
				} else {
					ftscrs[x] <- wf_wrsq[[1]][1,4]
				}
			}
		colz <- factorz[order(ftscrs,decreasing=T)][1:lmtr]
		} else {
		colz <- factorz
		}
		
		if(length(colz)>1) {
		colz <<- try(fix_collinearity(names(testset)[c(-1,-2)]))
			if(is(colz,'try-error')) {
				writeLog(reg_id,gvkey,NA,'E','fail_fix_collinearity')
			next
			}
		} else {
		colz <<- colz
		}	

		wf_wrsq	<- try(frm_call(lside,colz))

		if(is(wf_wrsq,'try-error') | is.na(wf_wrsq[[1]])) {
			facs <<- colz
			attempt_gvkey <- try(tryharder(3))
			if(is(attempt_gvkey,'try-error') | is.na(wf_wrsq[[1]])) {
				writeLog(reg_id,gvkey,NA,'W','no_winning_formula')
			next
			}
			wf_wrsq <- frm_call(lside,strsplit(attempt_gvkey[[1]],"\\+")[[1]])
		} 

		lset	<- wf_wrsq[[1]]
		factorz	<- lset[order(lset[,4],decreasing=F),1] #----- order by abs(T) in case we need to use LMTR
		wrsq	<- wf_wrsq[[3]]
		intercept <- wf_wrsq[[4]]
				
		facs<<-factorz
		if(nrow(testset)<length(facs)) {
			writeLog(reg_id,gvkey,NA,'E','too_many_facs')
		next
		}

#----- finally lets do this!!!!!!!!!!!
if(algo_id == 1)	attempt_gvkey <- try(source("D:/KRIS_R/run_sw_asc_frac.r"))
if(algo_id == 2)	attempt_gvkey <- try(source("D:/KRIS_R/run_sw_desc_frac.r"))
if(algo_id == 3)	attempt_gvkey <- try(source("D:/KRIS_R/reg_all_frac.r"))
#-----	if(algo_id == 4)	attempt_gvkey <- try(source("D:/KRIS_R/run_ccar_regr_frac.r"))
	

		if(is(attempt_gvkey,'try-error')) attempt_gvkey <- try(tryharder(2))

		if(is(attempt_gvkey,'try-error')) {
			writeLog(reg_id,gvkey,NA,'E','fail_attempt_gvkey')
		next
		} else if(is.null(attempt_gvkey) | is.na(attempt_gvkey) | length(attempt_gvkey)==0) {
			writeLog(reg_id,gvkey,NA,'W','no_winning_formula')
		next
		} else {
			winning_formulas <<- attempt_gvkey[[1]]
			dostats <- try(summary_stats_frac(winning_formulas,testset))

			if(is(dostats,'try-error')){
				writeLog(reg_id,gvkey,NA,'E','fail_summary_stats')
			next
			} else {
				writeLog(reg_id,gvkey,NA,'I','good_run')
			next
			}
		}
	}
}
