mydir <- ifelse(Sys.getenv('COMPUTERNAME') == 'TEDS-LAPTOP','/therocrates','/kamakura_r')
source(paste(mydir,"/kamakura_Rutils.r",sep=""))
timestamp()

#-----	that which we seek to write to the database
ds.gvkey_terms <- data.frame(matrix(nrow=0,ncol=5))
	names(ds.gvkey_terms) <- c('gvkey','term','error_term','intercept','adj_r_sqr')

ds.gvkey_coeffs <- data.frame(matrix(nrow=0,ncol=6))
	names(ds.gvkey_coeffs) <- c('gvkey','term','rf_code','coeff','std_err','t_stat')

#----- lets get our universe
gvkeys <- oq("select distinct gvkey from kris.dbo.client_portfolio where client='UNUM' and class in ('SOV','PUB','SEC')")[[1]]
MacroFactors <- getMacroFactors(2)[[1]]
MacroFactors <- MacroFactors[MacroFactors$year_mo>200612,]
mfacs <- MacroFactors[is.element(substr(MacroFactors$year_mo,5,6),c('03','06','09','12')),]
mfacs <- mfacs[,!is.element(names(mfacs),c('CS_IDX','CSSTD2YR','GDPSTD2Y'))]

mf_cols <- names(mfacs)[names(mfacs)!='year_mo']

mf_sets <- list(c('GDP_1YR','GDP_2YR','GDP_3YR','GDP_5YR','GDP_10YR'),
		c('CSRET1YR','CSRET2YR','CSRET3YR','CSRET5YR'),
		c('JPY_USD1','JPY_USD2','JPY_USD3'),
		c('EUR_USD1','EUR_USD2','EUR_USD3'),
		c('OILCHG1','OILCHG2','OILCHG3'))
mf_sets.1 <- mf_cols[!is.element(mf_cols,paste(unlist(mf_sets)))]
		
varset <- as.data.frame(matrix(nrow=0,ncol=2))
	names(varset) <- c('var','tstat')

nosig_vars <- as.data.frame(matrix(nrow=0,ncol=2))
	names(nosig_vars) <- c('gvkey','term')
	
for(gvkey in gvkeys) {
cat(gvkey,"...")

	kdp_values <- getKdpJc5RRF2(gvkey)
	if(nrow(kdp_values)<12) next
	kdp_values <- kdp_values[kdp_values$year_mo>=min(mfacs$year_mo),]

	num_obs <- nrow(kdp_values)
	if(num_obs < 12) next

	kdp_cols <- names(kdp_values)[substr(names(kdp_values),1,3)=='kdp']
	testset <- merge(kdp_values,mfacs,by.x='year_mo',all.x=T)

	for(y in 1:(length(kdp_cols))) {

	ds.gt <- ds.gvkey_terms
	ds.gc <- ds.gvkey_coeffs
	nsv <- nosig_vars
	term  <- toupper(substr(kdp_cols[y],5,7))
		thesevars <- varset
			for(z in 1:length(mf_cols)) {
				formula_str <- as.formula(paste(kdp_cols[y]," ~ ",mf_cols[z]))
				tstat <- abs(as.data.frame(summary(lm(formula_str,testset))[4])$coefficients.t.value[2])[[1]]
			if(!is.na(tstat)) if(as.numeric(tstat)>2) thesevars[(nrow(thesevars)+1),] <- cbind(mf_cols[z],tstat)
			}
		if(nrow(thesevars)==0) {		#----- no significant macrofactors
				nsv <- rbind(nsv,cbind(gvkey,term))
			fijiWriteTable(nsv,'rrf2.nosig_vars',T)
		next
		}
			thesevars$tstat <- as.numeric(thesevars$tstat)
		thesevars <- thesevars[order(thesevars$tstat,decreasing=T),]

		prelim_vars <- varset
		prelim_vars <- rbind(prelim_vars,thesevars[!is.element(thesevars$var,(c(unlist(mf_sets)))),])
		
		for(z in 1:length(mf_sets)) {
			if(nrow(thesevars[is.element(thesevars$var,mf_sets[[z]]),])>0) {
				prelim_vars <- rbind(prelim_vars,thesevars[is.element(thesevars$var,mf_sets[[z]]),][1,])
			}
		}
		prelim_vars <- prelim_vars[order(prelim_vars$tstat,decreasing=T),]
		if(nrow(prelim_vars)>trunc(num_obs*.5)) prelim_vars <- prelim_vars[1:trunc(num_obs*.5),]
			
		thesevars <- prelim_vars$var
		
		flag <- as.logical(length(thesevars))
		while(flag) {
		formula_str <- as.formula(paste(paste(kdp_cols[y]," ~ ",sep=""),paste(thesevars,sep="",collapse="+"),sep=""))
			thing <- lm(formula_str,testset)
				coefs <- as.data.frame(summary(thing)[4])
					coefs$mfac <- row.names(coefs)
				intercept <- coefs[1,1]
				coefs <- coefs[-1,]
		flag <- as.logical(nrow(coefs[abs(coefs[,3])<2,]))
			if(nrow(coefs)==1) {
				cat(paste("\n",gvkey," ",term," failed all iterations of the t.values > 2 rule\n",sep=""))
				flag <- (nrow(coefs)-1)
			} else {
				thesevars <- coefs[order(coefs[,4],decreasing=T),'mfac'][-1]
			}
		}
		sumthing <- summary(thing)
		ds.gt[(nrow(ds.gt)+1),] <- cbind(2,gvkey,'JC5',term,
					sumthing['sigma'][[1]],
					intercept,
					sumthing['adj.r.squared'][[1]])

	gsset <- data.frame(matrix(nrow=nrow(coefs),ncol=8))
		names(gsset) <- names(ds.gvkey_coeffs)
			gsset$reg_id	<- 	2
			gsset$gvkey	<- 	gvkey
			gsset$model	<- 	'JC5'
			gsset$term	<- 	term
			gsset$rf_code	<-	coefs$mfac
			gsset$coeff	<-	coefs[,1]
			gsset$std_err	<-	coefs[,2]
			gsset$t_stat	<-	coefs[,3]
	ds.gc <- rbind(ds.gc,gsset)
	
		ds.gt$reg_id <- as.integer(ds.gt$reg_id)
		ds.gt$gvkey <- as.character(ds.gt$gvkey)
		ds.gt$error_term <- as.numeric(ds.gt$error_term)
		ds.gt$intercept <- as.numeric(ds.gt$intercept)
		ds.gt$adj_r_sqr <- as.numeric(ds.gt$adj_r_sqr)

	if(nrow(ds.gt)>0) fijiWriteTable(ds.gt,'rrf2.gvkey_terms',T)
	if(nrow(ds.gc)>0) fijiWriteTable(ds.gc,'rrf2.gvkey_coeffs',T)
	}
}
timestamp()
