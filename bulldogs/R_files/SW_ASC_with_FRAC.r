run_sw_asc_frac <- function()	{
source("D:/KRIS_R/frag_regress_util.r")
	these_winning_formulas <- list(NULL)
	winning_rsqs <- list(NULL) 
	
	wf <- lset[lset[,4]>TVAL,1]
	if(length(wf)<1) wf <- lset[order(lset[,4],decreasing=T),1][1]
	wf_wrsq <- frm_call(lside,wf)
	if(length(wf)>1)  wf_wrsq <- dostepwise(wf_wrsq[[1]],lside) 
		wf 	<- wf_wrsq[[2]]
		wrsq	<- wf_wrsq[[3]]
		lwf 	<- wf

	TRYS=10
	qqq <- 1
	while(qqq) {
cat("\n\t\t qqq is ",qqq,"  ")
		trythese <- facs[!is.element(facs,wf)]
cat("\n\t\t",paste(wf,sep=" ",collapse=" + "))
		wf_wrsq <- runthese(trythese,wf) #----- this returns lset, wf, wrsq and intercept
			lset <<- wf_wrsq[[1]]
			wf <- wf_wrsq[[2]]
			wrsq <- wf_wrsq[[3]]
		these_winning_formulas <- append(these_winning_formulas,paste(wf,sep="",collapse="+"))
		winning_rsqs <- append(winning_rsqs,paste(wrsq))
		if(qqq>1) if(all(is.element(wf,lwf)) & all(is.element(lwf,wf))) qqq=FALSE
		if(qqq==TRYS) qqq=FALSE	
		if(qqq) qqq=qqq+1
		if(qqq>1) lwf=wf
	}
	these_winning_formulas <- unique(unlist(these_winning_formulas[-1]))
	winning_rsqs <- unique(unlist(winning_rsqs[-1]))
	these_winning_formulas <- these_winning_formulas[order(winning_rsqs,decreasing=T)][1]
return(these_winning_formulas)
}
