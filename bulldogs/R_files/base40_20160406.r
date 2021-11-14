mydir <- ('d:/kamakura_r')
source(paste(mydir,"/kamakura_Rutils.r",sep=""))
timestamp()

#	cmd.exe /c "R < d:/kamakura_r/base40_20160406.r "PUB" "1" "6"> d:/kamakura_r/tmp/base40_20160406.1.out 2>&1"
#	cmd.exe /c "R < d:/kamakura_r/base40_20160406.r "PUB" "2" "6"> d:/kamakura_r/tmp/base40_20160406.2.out 2>&1"
#	cmd.exe /c "R < d:/kamakura_r/base40_20160406.r "PUB" "3" "6"> d:/kamakura_r/tmp/base40_20160406.3.out 2>&1"
#	cmd.exe /c "R < d:/kamakura_r/base40_20160406.r "PUB" "4" "6"> d:/kamakura_r/tmp/base40_20160406.4.out 2>&1"
#	cmd.exe /c "R < d:/kamakura_r/base40_20160406.r "PUB" "5" "6"> d:/kamakura_r/tmp/base40_20160406.5.out 2>&1"
#	cmd.exe /c "R < d:/kamakura_r/base40_20160406.r "PUB" "6" "6"> d:/kamakura_r/tmp/base40_20160406.6.out 2>&1"

args		=	commandArgs()[-(1:2)]
thisclass	=	as.character(args[3])
quantile	=	as.integer(args[1])
quantiles	=	as.integer(args[2])

#-----	that which we seek to write to the database
ds.gvkey_terms <- data.frame(matrix(nrow=0,ncol=7))
	names(ds.gvkey_terms) <- c('reg_id','gvkey','model','term','error_term','intercept','adj_r_sqr')

ds.gvkey_coeffs <- data.frame(matrix(nrow=0,ncol=8))
		names(ds.gvkey_coeffs) <- c('reg_id','gvkey','model','term','rf_code','coeff','std_err','t_stat')

gvkeys <- getGvkeys(thisclass,quantiles,quantile)

mfacs <-  tq("exec therocrates.dbo.get_40macro_factors 0")
	names(mfacs) <- toupper(names(mfacs))
	names(mfacs)[1] <- tolower(names(mfacs)[1])
mfacs <- mfacs[,!is.element(names(mfacs),c('CS_IDX','CSSTD2YR','GDPSTD2Y'))] #----- excluding these mfacs
	for(x in 1:ncol(mfacs)) mfacs[,x] <- as.numeric(mfacs[,x])

mf_cols <- names(mfacs)[names(mfacs)!='YEARMO']
mf_sets <- list(c('GDP_1YR','GDP_2YR','GDP_3YR','GDP_5YR','GDP_10YR'),
		c('CSRET1YR','CSRET2YR','CSRET3YR','CSRET5YR'),
		c('JPY_USD1','JPY_USD2','JPY_USD3'),
		c('EUR_USD1','EUR_USD2','EUR_USD3'),
		c('OILCHG1','OILCHG2','OILCHG3'))
kdp_cols <- c('kdp_1mo','kdp_3mo','kdp_1yr','kdp_5yr')
TVAL <- 2

for(gvkey in gvkeys) {
cat(gvkey,"...")
	ds.gt <- ds.gvkey_terms
	ds.gc <- ds.gvkey_coeffs

	kdps <- get_kdpjc6(gvkey)
	for(x in 1:ncol(kdps)) kdps[,x] <- as.numeric(kdps[,x])

	testset <- merge(kdps,mfacs,by='yearmo')
	notthese <- ""
	for(zz in 6:ncol(testset)) if(sum(!is.na(testset[,zz])) < 12){ notthese <- append(notthese,names(testset)[zz]) }

	testset <- testset[,!is.element(names(testset),notthese)]
		colz <- names(testset)[!is.element(names(testset),c('yearmo',kdp_cols))]
		
	lmtr <<- trunc(nrow(testset)*.5)
	num_obs <- nrow(testset)
	if(num_obs < 12) next

	for(y in 1:(length(kdp_cols))) {
	lside <- kdp_cols[y]
	term  <- toupper(substr(kdp_cols[y],5,7))

		tvalues		<- NULL
		facs		<- NULL

		for(this_col in colz) {
			lset <- summary(lm(as.formula(paste(lside,"~",this_col,sep="")),testset))
				tvalues <- append(tvalues,lset$coefficients[-1,3])
				facs <- append(facs,this_col)
		}

		facs	<-	facs[abs(tvalues)>TVAL]
		if(length(facs)==0) next
		tvalues	<-	tvalues[abs(tvalues)>TVAL]
		facs	<-	facs[order(abs(tvalues),decreasing=T)]
		tvalues	<-	tvalues[order(abs(tvalues),decreasing=T)]

		for(z in 1:length(mf_sets)) {
			if(sum(is.element(facs,mf_sets[[z]]))>1) facs <- facs[!is.element(facs,facs[is.element(facs,mf_sets[[z]])][-1])]
		}
		if(length(facs)>lmtr)		facs <- facs[1:lmtr]

		flag <- as.logical(length(facs))
		while(flag) {
			coefs <- as.data.frame(summary(lm(as.formula(paste(paste(lside," ~ ",sep=""),paste(facs,sep="",collapse="+"),sep="")),testset))$coefficients[-1,])
			if(nrow(coefs)==1) flag=0
			flag <- as.logical(nrow(coefs[abs(coefs[,3])<TVAL,]))
			coefs <- coefs[order(coefs[,4],decreasing=T),][-1,]
			facs <- row.names(coefs)
		}
		wf <- row.names(coefs)
		lset <- summary(lm(as.formula(paste(lside,"~",paste(wf,sep="",collapse="+"),sep="")),testset))

		ds.gt[(nrow(ds.gt)+1),] <- cbind(2,gvkey,'JC6',term,
									lset$sigma,
									lset$coefficients[1,1],	#intercept
									lset$adj.r.squared)

		gsset <- data.frame(matrix(nrow=nrow(coefs),ncol=8))
			names(gsset) <- names(ds.gvkey_coeffs)
				gsset$reg_id		<- 	2
				gsset$gvkey			<- 	gvkey
				gsset$model			<- 	'JC6'
				gsset$term			<- 	term
				gsset$rf_code		<-	row.names(coefs)
				gsset$coeff			<-	coefs[,1]
				gsset$std_err		<-	coefs[,2]
				gsset$t_stat		<-	coefs[,3]
		ds.gc <- rbind(ds.gc,gsset)
	}	

		ds.gt$reg_id		<- as.integer(ds.gt$reg_id)
		ds.gt$gvkey			<- as.character(ds.gt$gvkey)
		ds.gt$model			<- as.character(ds.gt$model)
		ds.gt$term			<- as.character(ds.gt$term)
		ds.gt$error_term	<- as.numeric(ds.gt$error_term)
		ds.gt$intercept		<- as.numeric(ds.gt$intercept)
		ds.gt$adj_r_sqr		<- as.numeric(ds.gt$adj_r_sqr)

		ds.gc$reg_id		<- as.integer(ds.gc$reg_id)
		ds.gc$gvkey			<- as.character(ds.gc$gvkey)
		ds.gc$model			<- as.character(ds.gc$model)
		ds.gc$term			<- as.character(ds.gc$term)
		ds.gc$rf_code		<- as.character(ds.gc$rf_code)
		ds.gc$coeff			<- as.numeric(ds.gc$coeff)
		ds.gc$std_err		<- as.numeric(ds.gc$std_err)
		ds.gc$t_stat		<- as.numeric(ds.gc$t_stat)
		
	if(nrow(ds.gt)>0) tahitiWriteTable(ds.gt,'therocrates.gvkey_terms_20160406',T)
	if(nrow(ds.gc)>0) tahitiWriteTable(ds.gc,'therocrates.gvkey_coeffs_20160406',T)
}

