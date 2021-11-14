run_base40_regr <- function() {

		mf_cols <- facs
		flag <- as.logical(length(mf_cols))
		while(flag) {
			formula_str <- as.formula(paste(paste(lside," ~ ",sep=""),paste(mf_cols,sep="",collapse="+"),sep=""))
			thing <- lm(formula_str,testset)
			coefs <- as.data.frame(summary(thing)[4])
				coefs$mfac <- row.names(coefs)
			coefs <- coefs[-1,]
		flag <- as.logical(nrow(coefs[abs(coefs[,3])<TVAL,]))
			if(nrow(coefs)==1) {
			flag <- (nrow(coefs)-1)
			} else {
			coefs <- coefs[order(coefs[,4],decreasing=T),][-1,]
			mf_cols <- coefs$mfac
			}
		}
	this_wf <- paste(row.names(coefs),sep="",collapse="+")
	names(this_wf) <- paste('algo_id_',algo_id,sep="")
return(this_wf)
}


