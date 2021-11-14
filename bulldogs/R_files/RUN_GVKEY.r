run_gvkey <- function (gvkey='006066',colz=names(mfacs)[-1]) {

			winning_formulas <<- list(NULL)
			gvkeyset <- leftside[leftside$name==gvkey,c(2:3)]

			#----- this is our dataset for regressions
			testset <<- merge(gvkeyset,mfacs)
			names(testset) <- gsub(' ','',names(testset))	#----- ad hoc fix for column names with spaces 2015-08-19

			if(nrow(testset)<12) {
			write.table(paste(gvkey," less than 12 rows of data",sep=""),file=paste("d:/kris_R/tmp/",reg_id,"_messages.txt",sep=""),append=T,row.names=F, col.names=F)
			cat("\tError in min rows test set rule\n")
			return()
			}
				lmtr <<- trunc(nrow(testset)*.5)
				lmtr_flag <<- FALSE
				facs_lmtd <- NULL

			notthese <- ""
			for(zz in 3:ncol(testset)) { 
				if(sum(!is.na(testset[,zz])) < 12){
					write.table(paste(gvkey,"\t",names(testset)[zz],"\t this factor is NULL for all observations"),
						file=paste("d:/kris_R/tmp/",reg_id,"_messages.txt",sep=""),append=T,row.names=F, col.names=F,quote=F)
					notthese <- append(notthese,names(testset)[zz])
					}
			}
			testset <- testset[,!is.element(names(testset),notthese)]
			colz <<- names(testset)[c(-1,-2)]

			if(length(colz)>1) colz <<- try(fix_collinearity(colz))
			if(is(colz,'try-error')) {
				write.table(paste(gvkey," failed fix_collinearity function ",sep=""),file=paste("d:/kris_R/tmp/",reg_id,"_messages.txt",sep=""),append=T,row.names=F, col.names=F,quote=F)
			cat("\tError in fix collinearity step\n")
			return()
			}

	#----- first we find our starting factor and order model inputs by tscores descending
				tvalues <- NULL
				facs <- NULL
				for(this_col in colz) {
					lset <- summary(lm(as.formula(paste(lside,"~",this_col,sep="")),testset))
						tvalues <- append(tvalues,lset$coefficients[-1,3])
						facs <- append(facs,this_col)
				}
				facs <- facs[order(abs(tvalues),decreasing=T)]
				if(length(facs)>lmtr) {
					facs_lmtd <- facs[1:(lmtr)]
				}
				if(!is.null(facs_lmtd)) {facs<-facs_lmtd}
			facs<<-facs
				if(nrow(testset)<length(facs)) {
				write.table(paste(gvkey," num rows testset less than number of factors",sep=""),file=paste("d:/kris_R/tmp/",reg_id,"_messages.txt",sep=""),append=T,row.names=F, col.names=F,quote=F)
				cat("\tError in number of rows less than number of factors to be tested\n")
				return()
				}

				lset <<- summary(lm(as.formula(paste(lside," ~ ",paste(facs[1],sep="",collapse="+"),sep="")),testset))
				wf <<- facs[1]
				wrsq <<- lset$adj.r.squared
				wf_wrsq <<- list(lset,wf,wrsq)

				for(x in 1:length(algo_ids)) {		#----- now we loop on each regress algo
					algo_id <<- algo_ids[x]
					if(algo_id == 1) {
					source('D:/KRIS_R/SW_ASC.r')
					nf<-try(run_sw_asc())
						if(is(nf,'try-error')) {
							winning_formulas <- append(winning_formulas,NA)
						} else {
							winning_formulas <- append(winning_formulas,nf)
						}
					}
					if(algo_id == 2) {
					source('D:/KRIS_R/SW_DESC.r')
					nf<-try(run_sw_desc())
						if(is(nf,'try-error')) {
							winning_formulas <- append(winning_formulas,NA)
						} else {
							winning_formulas <- append(winning_formulas,nf)
						}
					}
					if(algo_id == 3) {
					source('D:/KRIS_R/REGR_ALL.r')
					nf<-try(reg_all())
						if(is(nf,'try-error')) {
							winning_formulas <- append(winning_formulas,NA)
						} else {
							winning_formulas <- append(winning_formulas,nf)
						}
					}
					if(algo_id == 4) {
					source('D:/KRIS_R/REGR_CCAR.r')
					nf<-try(run_ccar_regr())
						if(is(nf,'try-error')) {
							winning_formulas <- append(winning_formulas,NA)
						} else {
							winning_formulas <- append(winning_formulas,nf)
						}
					}

					if(algo_id == 5) {
					source('D:/KRIS_R/REGR_BASE40.r')
					nf <- 	try(run_base40_regr())
						if(is(nf,'try-error')) {
							winning_formulas <- append(winning_formulas,NA)
						} else {
							winning_formulas <- append(winning_formulas,nf)
						}
					}
				}
			winning_formulas <- unlist(winning_formulas)
			model_ids <<- sub('algo_id_','',names(winning_formulas))
			if(length(winning_formulas)==0)		return()
			if(all(is.na(winning_formulas)))	return()
			if(all(winning_formulas==''))		return()
			dostats <- try(summary_stats(winning_formulas,testset))
			if(is(dostats,'try-error')) cat("\tError in summary_stats\n")
}
