reg_all = function() { 	
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

	facsbk <- facs
	rf_codesbk <- rf_codes[is.element(rf_codes$rf_code,facs),]
	sets <- list(rf_codesbk[rf_codesbk$rf_group==0,'rf_code'])
	fgroups <- unique(rf_codesbk$rf_group[rf_codesbk$rf_group>0])
	for(fgroup in fgroups) sets <- append(sets,list(rf_codesbk[rf_codesbk$rf_group==fgroup,'rf_code']))

	fac_list <- sets
	lmtr_flag = FALSE
	wf = NULL
	wrsq = 0

	if(length(fac_list[[1]])!=0 | !is.null(fac_list[[1]])) { 
		trythese <- fac_list[[1]]
		wf_wrsq <- runthese(trythese,wf) 
		wf <- wf_wrsq[[2]]
		wrsq <- wf_wrsq[[3]]
		lset <- wf_wrsq[[1]]
	} 
	
#-----THE WHEEL OF FORTUNE
	lwf = wf
	qqq = 1
	TRYS=10
		while(qqq) {
			if(qqq!=1 & (length(fac_list[[1]])!=0 | !is.null(fac_list[[1]])) ) {
				trythese <- fac_list[[1]][!is.element(fac_list[[1]],wf)]
				wf_wrsq <- runthese(trythese,wf) 
					if( wf_wrsq[[3]] > wrsq ) wf <- wf_wrsq[[2]]
				}

			if(length(fac_list)>1) {
				for(xx in 2:length(fac_list)) {
					wfs <- NULL
					wrsqs <- NULL
					wts <- NULL
					trythese <- fac_list[[xx]]
					trythese <- trythese[is.element(trythese,facsbk)]
					if(length(trythese)>0) {
						frmlas <- NULL
						if(length(wf)==lmtr) {
							lmtr_flag = TRUE
							lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
							wf_holder <- wf
							wf <- row.names(lset$coefficients)[-1][order(abs(lset$coefficients[-1,3]))][-1]
						}
						for(thisone in trythese) frmlas <- append(frmlas,as.formula(paste(lside," ~ ",paste(append(wf[!is.element(wf,trythese)],thisone),sep="",collapse=" + "),sep="")))
						for(yy in 1:length(frmlas)) {
							if(length(unique(testset[,trythese[yy]]))==1 | all(is.na(testset[,trythese[yy]]))) {
								wfs[yy] <- NA
								wrsqs[yy]<- NA
								wts[yy] <- FALSE
							} else {
								wf_wrsq <- trythisone(frmlas[[yy]],testset,wf,wrsq)
								wfs[yy] <- list(wf_wrsq[[2]])
								wrsqs[yy]<- ifelse(is.null(wf_wrsq[[3]]),0,wf_wrsq[[3]])
								wts[yy] <- ifelse(is.null(wf_wrsq[[3]]),FALSE,all(abs(wf_wrsq[[1]]$coefficients[,3][-1])>TVAL))
							}
						}
						if(sum(wts)) {
							wfs <- wfs[wts]
							wrsqs <- wrsqs[wts]
								if(wrsqs[order(wrsqs,decreasing=T)][1]>wrsq){
									wf <- wfs[order(wrsqs,decreasing=T)][[1]]
									wrsq <- wrsqs[order(wrsqs,decreasing=T)][1]
								} else {
									if(lmtr_flag) wf <- wf_holder
								}
						}	
					}
				}
		}
#----- cat(wf,"\n\t",wrsq,"\n")
		if(qqq>1) if(all(is.element(wf,lwf)) & all(is.element(lwf,wf))) qqq=FALSE
		if(qqq==TRYS) qqq=FALSE	
		if(qqq) qqq=qqq+1
		if(qqq>1) lwf=wf
		}

	if(length(wf)>0) {
		this_wf <- paste(wf,sep="",collapse="+")
	} else { 
		this_wf <- NULL
	}
	names(this_wf) <- paste('algo_id_',algo_id,sep="")
return(this_wf)
}
