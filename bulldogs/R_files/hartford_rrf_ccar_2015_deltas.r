dostepwise <- function(lset = lset,lside=kdp_cols[y]) {
	flag <- ifelse(sum((abs(lset$coefficients[,3][-1])<2))<2,FALSE,as.logical(sum((abs(lset$coefficients[,3][-1])<2))))
	tscrs <- abs(lset$coefficients[,3][-1])
	while(flag) {
		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(row.names(lset$coefficients)[-1][order(tscrs)][-1],collapse=" + "),sep="")),testset))
		flag <- ifelse(sum((abs(lset$coefficients[,3][-1])<2))<2,FALSE,as.logical(sum((abs(lset$coefficients[,3][-1])<2))))
        tscrs <- abs(lset$coefficients[,3][-1])
	}
return(lset)
}

trythisone <- function(frmla,testset,fwf,fwrsq) {
	wfbk <- fwf
	lset <- summary(lm(frmla,testset))
	if(is.na(lset$adj.r.squared) & !is.null(wfbk)) {
		lset <- summary(lm(as.formula(paste(lside,paste(wfbk,sep="",collapse=" + "),sep=" ~ ")),testset))
		return(list(lset,row.names(lset$coefficients)[-1],as.numeric(lset$adj.r.squared)))
	}
	if(is.na(lset$adj.r.squared) & is.null(wfbk))  {
		return(list(NULL,wfbk,fwrsq))
	}
	if(all(abs(lset$coefficients[,3][-1])>2) & as.numeric(lset$adj.r.squared) > fwrsq) {
		return(list(lset,row.names(lset$coefficients)[-1],lset$adj.r.squared))
	}
	if(!all(abs(lset$coefficients[,3][-1])>2)){
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

runthese <- function(trythese,wf) {
	if(length(trythese)!=0) {
		for(xx in 1:length(trythese)){
			if(length(wf)==lmtr) {
				lmtr_flag = TRUE
				lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
				wf_holder <- wf
				wf <- row.names(lset$coefficients)[-1][order(abs(lset$coefficients[-1,3]))][-1]
			}
			mf <- trythese[xx]
			if(length(unique(testset[!is.na(testset[,mf]),mf]))==1 | all(is.na(testset[,mf]))) next
			frmla <- as.formula(paste(lside," ~ ",paste(append(wf,mf),sep="", collapse=" + "),sep=""))
			wf_wrsq <- trythisone(frmla,testset,wf,wrsq)
			if(all(abs(wf_wrsq[[1]]$coefficients[,3][-1])>2) & wf_wrsq[[3]]>wrsq) {
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

set <- read.table("./tmp/hartford_corpspreads.txt",sep="\t",header=T)
	set$datadate <- paste(substring(set$date,7,10),substring(set$date,1,2),substring(set$date,4,5),sep="")
	set$year_mo <- substring(set$datadate,1,6)

set <- set[,c('datadate','year_mo','AA_1yr','A_1yr','BBB_1yr','BB_1yr','B_1yr','B_5yr','BB_5yr','BBB_5yr','A_5yr','AA_5yr','AA_10yr','A_10yr','BBB_10yr','BB_10yr','B_10yr')]
	set$datadate <- as.integer(set$datadate)
	set$year_mo <- as.integer(set$year_mo)
	for(z in 1:ncol(set)) set[,z] <- gsub(',','',as.character(set[,z]))
	for(z in 3:ncol(set)) set[,z] <- as.numeric(set[,z])
	set <- set[order(set$datadate,decreasing=T),]

year_mos <- unique(set$year_mo)
thisset <- data.frame(matrix(nrow=length(year_mos),ncol=17))
	names(thisset) <- c('data_date','year_mo','AA_1yr','A_1yr','BBB_1yr','BB_1yr','B_1yr','B_5yr','BB_5yr','BBB_5yr','A_5yr','AA_5yr','AA_10yr','A_10yr','BBB_10yr','BB_10yr','B_10yr')
	thisset$year_mo <- year_mos

for(x in 1:nrow(thisset)) {
	subset <- set[set$year_mo==thisset$year_mo[x],]
	thisset[x,] <- subset[order(subset$datadate,decreasing=T),][1,]
}
thisset <- thisset[,-1]
kdp_cols <- names(thisset)[-1]
terms <- gsub('_','',substring(kdp_cols,(nchar(kdp_cols)-3),nchar(kdp_cols)))

#-----mfacs <- getMacroFactors(3)$ccar
mfacs <- getRRF3mf()

mfacs <- mfacs[,names(mfacs)[!is.element(names(mfacs),c('BBBCORP','BBBSPD'))]]
names(mfacs) <- ifelse(names(mfacs)=='PRIME1YD','PRIME_1YD',names(mfacs))
names(mfacs) <- ifelse(names(mfacs)=='TR10Y1YD','TR10Y_1YD',names(mfacs))

base_formula <- c('DJ1YR','CPI','TSLOPE','RGDP')
large_co_facs <- c('CPIASIA','CPIEUR','CPIJPN','CPIUK','FXASIA','FXEUR','FXJPN','FXUK','RGDPASIA','RGDPEUR','RGDPJPN','RGDPUK')
base_plus <- list(c('TREAS3MO','TREAS5YR','TREAS10Y'), c('TR3M_1YD','TR5Y_1YD','TR10Y_1YD'))

testset <- merge(thisset,mfacs,by='year_mo')
write.table(testset[,c('year_mo','CPI','DJ1YR','NGDP','RGDP','TR10Y_1YD','TR3M_1YD','TR5Y_1YD','TREAS10Y','TREAS3MO','TREAS5YR','TSLOPE')],file='./tmp/macro_factors_for_hartford.txt',sep="\t",quote=F,row.names=F)

for(x in 2:ncol(testset)) testset[,x] <- as.numeric(testset[,x])
for(x in 2:16) testset[,x] <- testset[,x]-lagn(testset[,x],1)

fac_list <- list(base_formula,base_plus)

num_obs <- nrow(testset)
lmtr <- trunc(num_obs*.5)

for(y in 1:(length(kdp_cols))) { 
		lmtr_flag <- FALSE
		term = terms[y]
		lside = kdp_cols[y]
		wf <- NULL
		wrsq <- 0

#-----THE WHEEL OF FORTUNE
		lwf <- wf
		qqq <- 1
		TRYS=10
			while(qqq) {
			#-----DOMESTIC MACRO-FACTORS
				trythese <- unlist(fac_list[[1]])
				trythese <- trythese[!is.element(trythese,wf)]
				wf_wrsq <- runthese(trythese,wf)
					wf <- wf_wrsq[[2]]
					wrsq <- wf_wrsq[[3]]
			#-----GROUPED MACRO-FACTORS
				for(x in 1:length(fac_list[[2]])) { 
					wfs <- NULL
					wrsqs <- NULL
					wts <- NULL
					trythese <- fac_list[[2]][x][[1]]
					frmlas <- NULL
					if(length(wf)==lmtr) {
						lmtr_flag = TRUE
						lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))
						wf_holder <- wf
						wf <- row.names(lset$coefficients)[-1][order(abs(lset$coefficients[-1,3]))][-1]
					}
					for(thisone in trythese) frmlas <- append(frmlas,as.formula(paste(lside," ~ ",paste(append(wf[!is.element(wf,trythese)],thisone),sep="",collapse=" + "),sep="")))
					for(xx in 1:length(frmlas)) {
						mf <- trythese[xx]
						if(length(unique(testset[,mf]))==1 | all(is.na(testset[,mf]))) {
							wfs[xx] <- NA
							wrsqs[xx]<- NA
							wts[xx] <- FALSE
						} else {
							wf_wrsq <- trythisone(frmlas[[xx]],testset,wf,wrsq)
							wfs[xx] <- list(wf_wrsq[[2]])
							wrsqs[xx]<- ifelse(is.null(wf_wrsq[[3]]),0,wf_wrsq[[3]])
							wts[xx] <- ifelse(is.null(wf_wrsq[[3]]),FALSE,all(abs(wf_wrsq[[1]]$coefficients[,3][-1])>2))
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
			#-----INTERNATIONAL & LARGE COMPANY FACS excluded in this instance, but keeping this code in for posterity
				if(length(fac_list)==3){
					trythese <- unlist(fac_list[[3]])
					trythese <- trythese[!is.element(trythese,wf)]
						wf_wrsq <- runthese(trythese,wf)
						wf <- wf_wrsq[[2]]
						wrsq <- wf_wrsq[[3]]
				}
			if(qqq>1) if(all(is.element(wf,lwf)) & all(is.element(lwf,wf))) qqq=FALSE
			if(qqq==TRYS) qqq=FALSE	
			if(qqq) qqq=qqq+1
			if(qqq>1) lwf=wf
cat(qqq,"\t\n")

			}
			if(length(wf)>0) {
		lset <- summary(lm(as.formula(paste(lside," ~ ",paste(wf,sep="",collapse=" + "),sep="")),testset))

#	kdp_cols[y],
#	paste(kdp_cols[y],'_delta',sep=""),
		gcset <- data.frame(cbind(
				paste(kdp_cols[y],'_delta',sep=""),
				row.names(lset$coefficients)[-1],
				lset$coefficients[-1,1],
				lset$coefficients[-1,2],
				lset$coefficients[-1,3],
				lset$coefficients[-1,4],
				lset$sigma,
				lset$coefficients[1,1],
				lset$adj.r.squared))
		names(gcset) <- c('spread','mfac','coeff','std_err','t_stat','prob_t','error_term','intercept','adj_r_sq')
		tahitiWriteTable(gcset,'therocrates.hartford_spreads_rrf3_3')
	}
}

#-----	select  [SPREAD],[MFAC], cast([COEFF] as float),cast([STD_ERR] as float),cast([T_STAT] as float),cast([PROB_T] as float),cast([ERROR_TERM] as float),cast([INTERCEPT] as float),cast([ADJ_R_SQ] as float) from therocrates.[dbo].[hartford_spreads_rrf3]


