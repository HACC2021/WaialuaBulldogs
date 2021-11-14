
#-----  now we goto stepwise DESCending
run_sw_desc <- function()	{

		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(facs,sep="",collapse="+"),sep="")),testset))
		swd_flag <- sum((abs(lset$coefficients[-1,3])<TVAL))
		while(swd_flag) {
					lset <- summary(lm(as.formula(paste(lside," ~ ",paste(names(lset$coefficients[-1,3][order(abs(lset$coefficients[-1,3]))][-1]),sep="",collapse="+"),sep="")),testset))
					swd_flag <- sum((abs(lset$coefficients[-1,3])<TVAL))
		}
		while(lmtr<length(row.names(lset$coefficients)[-1])) {
					lset <- summary(lm(as.formula(paste(lside," ~ ",paste(names(lset$coefficients[-1,3][order(abs(lset$coefficients[-1,3]))][-1]),sep="",collapse="+"),sep="")),testset))
					swd_flag <- sum((abs(lset$coefficients[-1,3])<TVAL))
			while(swd_flag) {
					lset <- summary(lm(as.formula(paste(lside," ~ ",paste(names(lset$coefficients[-1,3][order(abs(lset$coefficients[-1,3]))][-1]),sep="",collapse="+"),sep="")),testset))
					swd_flag <- sum((abs(lset$coefficients[-1,3])<TVAL))
			}			
		}
		if(as.logical(sum(abs(lset$coefficients[-1,3])>TVAL))) {
			this_wf <- list(paste(row.names(lset$coefficients)[-1],sep="",collapse="+"))
			names(this_wf) <- paste('algo_id_',algo_id,sep="")
		} else {
			this_wf <- list(NULL)
			names(this_wf) <-  paste('algo_id_',algo_id,sep="")
			}
	return(this_wf)
}


