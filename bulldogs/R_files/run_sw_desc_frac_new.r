	wf_wrsq <- frm_call(lside,facs)
		swd_flag <- as.logical(sum(wf_wrsq[[1]]<TVAL))
		if(swd_flag) mf_cols <- names(wf_wrsq[[1]][order(wf_wrsq[[1]],decreasing=F)])[-1]
		while(swd_flag) {
			wf_wrsq <- frm_call(lside,mf_cols)
			swd_flag <- as.logical(sum(wf_wrsq[[1]]<TVAL))
			mf_cols <- names(wf_wrsq[[1]][order(wf_wrsq[[1]],decreasing=F)])[-1]
			if(length(mf_cols)==1) {
				wf_wrsq <- frm_call(lside,mf_cols)
				swd_flag<-F
			}
		}

		mf_cols <-  names(wf_wrsq[[1]][order(wf_wrsq[[1]],decreasing=F)])
		while(lmtr<length(mf_cols)) {
			mf_cols <- mf_cols[-1]
			wf_wrsq <- frm_call(lside,mf_cols)
			swd_flag <- sum(wf_wrsq[[1]]<TVAL)
			while(swd_flag) {
				wf_wrsq <- frm_call(lside,mf_cols)
				swd_flag <- as.logical(sum(wf_wrsq[[1]]<TVAL))
				mf_cols <- names(wf_wrsq[[1]][order(wf_wrsq[[1]],decreasing=F)])[-1]
				if(length(mf_cols)==1) {
					wf_wrsq <- frm_call(lside,mf_cols)
					swd_flag<-F
				}
			}
		}

		mf_cols <- wf_wrsq[[2]]
		if(as.logical(all(wf_wrsq[[1]]>TVAL))) {
			this_wf <- paste(mf_cols,sep="",collapse="+")
			names(this_wf) <- paste('algo_id_',algo_id,sep="")
		} else {
			this_wf <- NULL
			names(this_wf) <-  paste('algo_id_',algo_id,sep="")
		}

ret_f <- function(){
	return(this_wf)
}
ret_f()


