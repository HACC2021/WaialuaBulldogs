run_sw_desc <- function()	{

	lset <- summary(lm(as.formula(paste(lside," ~ ",paste(facs,sep="",collapse="+"),sep="")),testset))
	if(as.logical(sum(as.logical(lset$aliased[-1])))) {
		
		basefacs <- c('EUR_USD','JPY_USD','OILPRICE','UNEMPLOY')
		sets <- list(c('TR10YR_C','TR10YR_E','TR10YR_G','TR10YR_J','TR10YR_U'),
						c('CS_IDX_R1Y_NON','CS_IDX_R2Y_NON','CS_IDX_R3Y_NON','CS_IDX_R4Y_NON','CS_IDX_R5Y_NON'),
						c('EUR_USD_R1Y_NON','EUR_USD_R2Y_NON','EUR_USD_R3Y_NON'),
						c('GDP_R10Y_NON','GDP_R1Y_NON','GDP_R2Y_NON','GDP_R3Y_NON','GDP_R4Y_NON','GDP_R5Y_NON'),
						c('JPY_USD_R1Y_NON','JPY_USD_R2Y_NON','JPY_USD_R3Y_NON'),
						c('OILPRICE_R1Y_NON','OILPRICE_R2Y_NON','OILPRICE_R3Y_NON'),
						c('IDX_C_R2Y_NON','IDX_E_R2Y_NON','IDX_G_R2Y_NON','IDX_J_R2Y_NON','IDX_U_R2Y_NON'),
						c('IDX_U_SDR1Y_NON','IDX_J_SDR1Y_NON','IDX_G_SDR1Y_NON','IDX_E_SDR1Y_NON','IDX_C_SDR1Y_NON'))
			
		if(all(is.element(facs, c(unlist(basefacs),unlist(sets))))) {
		
		wf <- NULL
		for(xx in 1:length(sets)) {
			lset <- summary(lm(as.formula(paste(lside,"~",paste(sets[[xx]],sep="",collapse="+"),sep="")),testset))
			wf <- append(wf,paste(names(lset$coefficients[-1,1])[order(lset$coefficients[-1,1],decreasing=T)][1],
					names(lset$coefficients[-1,1])[order(lset$coefficients[-1,1],decreasing=T)][(nrow(lset$coefficients)-1)],sep="+"))
		}
		wf <- paste(paste(basefacs,sep="",collapse="+"),"+",paste(wf,sep="",collapse="+"),sep="")
		lset <- summary(lm(as.formula(paste(lside,"~",wf,sep="")),testset))
		} else {

		gfac_holder <- row.names(lset$coefficients)[-1]
		bfacs <- facs[!is.element(facs,gfac_holder)]
		bfacflag <- length(bfacs)
			while(bfacflag) {
				bfacflag <- length(bfacs)
				if(length(bfacs)==1) {
					lset <- summary(lm(as.formula(paste(lside,"~",paste(append(gfac_holder,bfacs),sep="",collapse="+"),sep="")),testset))
					bfacs <- gfac_holder[!is.element(gfac_holder,row.names(lset$coefficients)[-1])]
					gfacs <- row.names(lset$coefficients)[-1]
					if(length(bfacs)==1) bfacs <- bfacs[-1]
				}
				if(length(bfacs)==2) {
					lset <- summary(lm(as.formula(paste(lside,"~",paste(bfacs,sep="",collapse="+"),sep="")),testset))
					gfacs <- append(gfac_holder, row.names(lset$coefficients[-1,])[order(abs(lset$coefficients[-1,3]),decreasing=T)][1])
					bfacs <- bfacs[!is.element(bfacs,gfacs)]
				}
				if(length(bfacs)>2){
				gfacs <- gfac_holder
					for(zz in 1:length(bfacs)) {
						lset <- summary(lm(as.formula(paste(lside,"~",paste(append(gfacs,bfacs[zz]),sep="",collapse="+"),sep="")),testset))
						if(!as.logical(sum(as.logical(lset$aliased[-1])))) gfacs <- row.names(lset$coefficients[-1,])
					}
				lset <- summary(lm(as.formula(paste(lside,"~",paste(gfacs,sep="",collapse="+"),sep="")),testset))
				if(as.logical(sum(as.logical(lset$aliased[-1])))) lset <- summary(lm(as.formula(paste(lside,"~",paste(gfac_holder,sep="",collapse="+"),sep="")),testset))
				gfacs <- row.names(lset$coefficients[-1,])
				bfacflag=0
				}
			}
		lset <- summary(lm(as.formula(paste(lside,"~",paste(gfacs,sep="",collapse="+"),sep="")),testset))
		}
	if(!as.logical(sum(as.logical(lset$aliased[-1])))){
		swd_flag <- sum((abs(lset$coefficients[-1,3])<2))
		while(swd_flag) {
					lset <- summary(lm(as.formula(paste(lside," ~ ",paste(names(lset$coefficients[-1,3][order(abs(lset$coefficients[-1,3]))][-1]),sep="",collapse="+"),sep="")),testset))
					swd_flag <- sum((abs(lset$coefficients[-1,3])<2))
		}
		while(lmtr<length(row.names(lset$coefficients)[-1])) {
					lset <- summary(lm(as.formula(paste(lside," ~ ",paste(names(lset$coefficients[-1,3][order(abs(lset$coefficients[-1,3]))][-1]),sep="",collapse="+"),sep="")),testset))
					swd_flag <- sum((abs(lset$coefficients[-1,3])<2))
			while(swd_flag) {
					lset <- summary(lm(as.formula(paste(lside," ~ ",paste(names(lset$coefficients[-1,3][order(abs(lset$coefficients[-1,3]))][-1]),sep="",collapse="+"),sep="")),testset))
					swd_flag <- sum((abs(lset$coefficients[-1,3])<2))
			}			
		}
		if(as.logical(sum(abs(lset$coefficients[-1,3])>2))) {
			this_wf <- list(paste(names(lset$coefficients[-1,3]),sep="",collapse="+"))
			names(this_wf) <- paste('algo_id_',algo_id,sep="")
		} else {
			this_wf <- list(NULL)
			names(this_wf) <-  paste('algo_id_',algo_id,sep="")
		}
	}
return(this_wf)
	
}
