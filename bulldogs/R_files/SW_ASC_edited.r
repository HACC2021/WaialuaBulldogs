
#-----  now we goto stepwise ASCending
run_sw_asc <- function()	{

dostepwise <<- function(lset = lset,lside=kdp_cols[y]) {
	flag <- ifelse(sum((abs(lset$coefficients[,3][-1])<TVAL))<2,FALSE,as.logical(sum((abs(lset$coefficients[,3][-1])<TVAL))))
	tscrs <- abs(lset$coefficients[,3][-1])
	while(flag) {
		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(row.names(lset$coefficients)[-1][order(tscrs)][-1],collapse=" + "),sep="")),testset))
		flag <- ifelse(sum((abs(lset$coefficients[,3][-1])<TVAL))<2,FALSE,as.logical(sum((abs(lset$coefficients[,3][-1])<TVAL))))
		tscrs <- abs(lset$coefficients[,3][-1])
	}
return(lset)
}

trythisone <<- function(frmla,testset,fwf,fwrsq) {
	wfbk <- fwf
	lset <- summary(lm(frmla,testset))
		if(is.na(lset$adj.r.squared) & !is.null(wfbk)) {
			lset <- summary(lm(as.formula(paste(lside,paste(wfbk,sep="",collapse=" + "),sep=" ~ ")),testset))
			return(list(lset,row.names(lset$coefficients)[-1],as.numeric(lset$adj.r.squared)))
		}
		if(is.na(lset$adj.r.squared) & is.null(wfbk))  {
			return(list(NULL,wfbk,fwrsq))
		}
		if(all(abs(lset$coefficients[,3][-1])>TVAL) & as.numeric(lset$adj.r.squared) > fwrsq) {
			return(list(lset,row.names(lset$coefficients)[-1],lset$adj.r.squared))
		}
		if(!all(abs(lset$coefficients[,3][-1])>TVAL)){
			lset <- dostepwise(lset,lside)
			if(!is.na(lset$adj.r.squared)) {
				return(list(lset,row.names(lset$coefficients)[-1],as.numeric(lset$adj.r.squared)))
			}
		}
		if(!is.null(wfbk)) {
			lset <- summary(lm(as.formula(paste(lside,paste(wfbk,sep="",collapse=" + "),sep=" ~ ")),testset))
			return(list(lset,row.names(lset$coefficients)[-1],as.numeric(lset$adj.r.squared)))
		} else {
			return(list(NULL,wfbk,fwrsq))
		}
}

runthese <<- function(trythese,wf) {
	if(length(trythese)!=0) {
		for(xx in 1:length(trythese)){
			if(length(wf)==lmtr) {
				lmtr_flag <- TRUE
				lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
				wf_holder <- wf
				wf <- row.names(lset$coefficients)[-1][order(abs(lset$coefficients[-1,3]))][-1]
			}
			mf <- trythese[xx]
			if(length(unique(testset[!is.na(testset[,mf]),mf]))==1 | all(is.na(testset[,mf]))) next
			frmla <- as.formula(paste(lside," ~ ",paste(append(wf,mf),sep="", collapse=" + "),sep=""))
			wf_wrsq <- trythisone(frmla,testset,wf,wrsq)
			if(all(abs(wf_wrsq[[1]]$coefficients[,3][-1])>TVAL) & wf_wrsq[[3]]>wrsq) {
				wf <- wf_wrsq[[2]]
				wrsq <- wf_wrsq[[3]]
			} else {
				if(lmtr_flag) wf <- wf_holder
			}
		}
	}
	if(!is.null(wf)){
		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
		return(list(lset,row.names(lset$coefficients)[-1],as.numeric(lset$adj.r.squared)))
	} else {
	return(list(NULL,wf,wrsq))
	}
}	

	winning_rsqs <- list(NULL) 
	wf <- NULL
	wrsq <- 0
	lwf <- wf

	TRYS=10
	qqq <- 1

	while(qqq) {
		trythese <- facs[!is.element(facs,wf)]
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
	winning_formulas <-	winning_formulas[order(winning_rsqs,decreasing=T)][1]
	names(winning_formulas)[1] <- paste('algo_id_',algo_id,sep="")
return(winning_formulas)
}


