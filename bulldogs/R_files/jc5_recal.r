mydir <- ifelse(Sys.getenv('COMPUTERNAME') == 'TEDS-LAPTOP','/therocrates','/kamakura_r')
source(paste(mydir,"/kamakura_Rutils.r",sep=""))
timestamp()

args <- commandArgs()[-(1:2)]
cat(paste(args,sep="",collapse="\t"))

#-----	that which we seek to write to the database
ds.gvkey_terms <- data.frame(matrix(nrow=0,ncol=7))
	names(ds.gvkey_terms) <- c('reg_id','gvkey','model','term','error_term','intercept','adj_r_sqr')

ds.gvkey_coeffs <- data.frame(matrix(nrow=0,ncol=8))
	names(ds.gvkey_coeffs) <- c('reg_id','gvkey','model','term','rf_code','coeff','std_err','t_stat')

#----- lets get our universe
if(length(args)==0) gvkeys <- getGvkeys(1,1)
if(length(args)==2) {
	quantiles <- as.integer(args[1])
	quantile <- as.integer(args[2])
	gvkeys <- getGvkeys(quantiles,quantile)
}
cat("\nlets do these:\t",min(gvkeys),"\tthrough:\t",max(gvkeys),"\n")

#----- lets get our macro factors 
mfacs <- getMacroFactors(2)$base_40
mfacs <- mfacs[,!is.element(names(mfacs),c('CS_IDX','CSSTD2YR','GDPSTD2Y'))]

#----- we'll loop on these
kdp_cols <- c('kdp_1mo','kdp_3mo','kdp_1yr','kdp_5yr')

#-----	helper data frame 
sset <- as.data.frame(matrix(nrow=nrow(mfacs),ncol=6))
	names(sset) <- c('gvkey','year_mo','kdp_1mo','kdp_3mo','kdp_1yr','kdp_5yr')
	sset$year_mo <- mfacs$year_mo
	
#----- 	static data items
reg_id = 2
model = 'JC5'

for(gvkey in gvkeys) {
cat(gvkey,"..  ")

ds.gt <- ds.gvkey_terms
ds.gc <- ds.gvkey_coeffs
set <- sset
	set$gvkey <- gvkey

	kdp_values <- oq(paste("select jc.gvkey, substring(convert(char,jc.data_date,112),1,6) as year_mo,
			jc.kdp_1mo, jc.kdp_3mo, jc.kdp_1yr, jc.kdp_5yr 
			from kdp_ts.dbo.kdp_jc5 jc where jc.gvkey='",
			gvkey,"' order by data_date desc",sep=""))
	if(nrow(kdp_values)==0) next
	for(x in 1:nrow(set)) set[x,] <- kdp_values[kdp_values$year_mo==set$year_mo[x],][1,]
	set <- set[!is.na(set$gvkey),]
		set$year_mo <- as.integer(set$year_mo)
	for(x in 3:6) set[,x] <- as.numeric(set[,x])

	num_obs <- nrow(set)
	if(num_obs < 12) next

	kdp_values <- set
	testset <- merge(kdp_values,mfacs,by.x='year_mo',all.x=T)
		for(y in 1:(length(kdp_cols))) {
		term = toupper(substr(kdp_cols[y],5,7))
		mf_cols <- names(mfacs)[names(mfacs)!='year_mo']

		flag <- as.logical(length(mf_cols))
		while(flag) {
			formula_str <- as.formula(paste(paste(kdp_cols[y]," ~ ",sep=""),paste(mf_cols,sep="",collapse="+"),sep=""))
			thing <- lm(formula_str,testset)
			coefs <- as.data.frame(summary(thing)[4])
				coefs$mfac <- row.names(coefs)
			intercept <- thing$coefficients[[1]]
			coefs <- coefs[-1,]
		flag <- as.logical(nrow(coefs[abs(coefs[,3])<2,]))
			if(nrow(coefs)==1) {
				cat(paste(gvkey," this one failed all of the iterations in the abs(t.values < 2) rule so skipping to next\n",sep=""))
			flag <- (nrow(coefs)-1)
			} else {
			coefs <- coefs[order(coefs[,4],decreasing=T),][-1,]
			mf_cols <- coefs$mfac
			}
		}
		sumthing <- summary(thing)
		error_term = sumthing$sigma
		adj_r_sqr = sumthing$adj.r.squared
		
		ds.gt[(nrow(ds.gt)+1),] <- cbind(reg_id,gvkey,model,term,error_term,intercept,adj_r_sqr)

		gsset <- as.data.frame(matrix(nrow=nrow(coefs),ncol=8))
		names(gsset) <- names(ds.gvkey_coeffs)
			gsset$reg_id	<- 	reg_id
			gsset$gvkey	<- 	gvkey
			gsset$model	<- 	model
			gsset$term	<- 	term
			gsset$rf_code	<-	coefs$mfac
			gsset$coeff	<-	coefs[,1]
			gsset$std_err	<-	coefs[,2]
			gsset$t_stat	<-	coefs[,3]
		ds.gc	<-	rbind(ds.gc,gsset)	
		}
	ds.gt$reg_id <- as.integer(ds.gt$reg_id)
	ds.gt$gvkey <- as.character(ds.gt$gvkey)
	ds.gt$error_term <- as.numeric(ds.gt$error_term)
	ds.gt$intercept <- as.numeric(ds.gt$intercept)
	ds.gt$adj_r_sqr <- as.numeric(ds.gt$adj_r_sqr)
fijiWriteTable(ds.gt,'rrf2.gvkey_calibrate_terms',T)
fijiWriteTable(ds.gc,'rrf2.gvkey_calibrate_coeffs',T)
}
timestamp()
