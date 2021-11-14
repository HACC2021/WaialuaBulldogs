	
		facsbk <- facs
		rf_codesbk <- rf_codes[is.element(rf_codes$rf_code,facs),]
		sets <- list(rf_codesbk[rf_codesbk$rf_group==0,'rf_code'])
		if(length(unique(rf_codesbk$rf_group))>1) {
			fgroups <- unique(rf_codesbk$rf_group[rf_codesbk$rf_group>0])
			for(fgroup in fgroups) sets <- append(sets,list(rf_codesbk[rf_codesbk$rf_group==fgroup,'rf_code']))
		}
		fac_list <- sets
		lmtr_flag = FALSE

		if(length(fac_list[[1]])!=0 | !is.null(fac_list[[1]])) {
			wf = fac_list[[1]][1:2]
			fac_list[[1]] <- fac_list[[1]][c(-1,-2)] 
		}
		wrsq = 0

		if(length(fac_list[[1]])!=0 | !is.null(fac_list[[1]])) { 
			trythese <- fac_list[[1]]
			wf_wrsq <- runthese(trythese,wf) 
			wf <- wf_wrsq[[2]]
			wrsq <- wf_wrsq[[3]]
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
					frmlas <- list(NULL)
						if(length(wf)==lmtr) {
							lmtr_flag = TRUE
							wf_holder <- wf
							wf_wrsq <- runthese(trythese,wf) 
							wf <- wf_wrsq[[2]]
							wrsq <- wf_wrsq[[3]]
						}
					for(thisone in trythese) {
						thisfrmla <-list(paste(paste(wf[!is.element(wf,trythese)],collapse="+"),thisone,sep="+"))
						frmlas<- append (frmlas,thisfrmla)
					}
				frmlas <- frmlas[-1]
					for(yy in 1:length(frmlas)) {
						if(length(unique(testset[,trythese[yy]]))==1 | all(is.na(testset[,trythese[yy]]))) {
						wfs[yy] <- NA
						wrsqs[yy]<- NA
						wts[yy] <- FALSE
						} else {
						wf_wrsq <- trythisone(strsplit(frmlas[[yy]],'\\+')[[1]],wf,wrsq)
						wfs[yy] <- list(wf_wrsq[[2]])
						wrsqs[yy]<- ifelse(is.null(wf_wrsq[[3]]),0,wf_wrsq[[3]])
						wts[yy] <- ifelse(is.null(wf_wrsq[[3]]),FALSE,all(wf_wrsq[[1]]>TVAL))
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

ret_f <- function(){
	return(this_wf)
}
ret_f()


