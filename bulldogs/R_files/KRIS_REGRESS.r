options(scipen=999)
options(warn=-99)
source("d:/KRIS_R/R_utils.r")

#	cmd.exe /c "D:/KRIS_R/R < D:/KRIS_R/KRIS_REGRESS.r 1 > d:/KRIS_R/tmp/outfile.out 2>&1"
args		=	commandArgs()[-(1:2)]
reg_id		<<-	as.integer(args[1])		#-----	
#----- run_factors
mfacs <<- nq(paste('exec mfsq.dbo.get_regress_macro_factors ',reg_id,sep=""))

#----- test these regress algos
reg_id_stuff <<- nq(paste("select r.*, s.sig_value from mfsq.dbo.mfs_regression r join NAURU.MFS3.dbo.sig_level s on s.sig_text=r.sig_text and reg_id='",reg_id,"'",sep=""))
for(x in 1:ncol(reg_id_stuff)) reg_id_stuff[,x] <- as.character(reg_id_stuff[,x])
algo_ids <<- as.integer(reg_id_stuff$algo_id)
TVAL <<- as.numeric(reg_id_stuff$sig_value)
REG_TYPE <<- ifelse(is.na(reg_id_stuff$reg_type[1]), 'LINEAR', reg_id_stuff$reg_type[1])


if(length(mfacs)==0 ) {
	writeLog(reg_id,NA,NA,'no_model_inputs')
} else if(nrow(mfacs)<12) { #-----E100000000
	writeLog(reg_id,NA,NA,'less_than_12_obs')
} else {
		for(x in 2:ncol(mfacs)) mfacs[,x] <- as.numeric(mfacs[,x])
		mfacs$year_mo <- as.integer(mfacs$year_mo)
		names(mfacs)[2:ncol(mfacs)] <- toupper(gsub('_non_non','',names(mfacs)[2:ncol(mfacs)]))
		colz <<- names(mfacs)[-1]

		#-----	run_names
		leftside <<- nq(paste("select name, yearmo as year_mo, name_value from mfsq.dbo.run_names where reg_id=",reg_id," order by name,yearmo",sep=""))
			leftside$name <- as.character(leftside$name)
			for(x in 2:3) leftside[,x] <- as.numeric(leftside[,x])

		rf_codes <- nq(paste("select distinct factor+'_'+transform_code+'_'+lag_code as rf_code, rf_num, transform_code, lag_code, rf_group from mfsq.dbo.run_factors where reg_id='",reg_id,"' order by rf_num",sep=""))
		rf_codes$rf_code <- sub('_NON_NON','',rf_codes$rf_code)

		gvkeys <- unique(leftside$name)
		lside <<- 'name_value'
	

for(gvkey in gvkeys) {					#-----	now we loop on uniques in the portfolio of run_names
		colz <<- names(mfacs)[-1]
		gvkey <<- gvkey
		cat("\t",gvkey,"\t")

			source("d:/KRIS_R/RUN_GVKEY.r")
			source("d:/KRIS_R/SUMMARY_STATS.r")
			attempt_gvkey <- try(run_gvkey(gvkey,colz))

		if(is(attempt_gvkey,'try-error')) {
			writeLog(reg_id,gvkey,NA,'fail_summary_stats')
		} else {
			writeLog(reg_id,gvkey,NA,'good_run')
		next
		}
	}
}



