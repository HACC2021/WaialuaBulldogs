mydir <- ifelse(Sys.getenv('COMPUTERNAME') == 'TEDS-LAPTOP','/therocrates','/kamakura_r')
source(paste(mydir,"/kamakura_Rutils.r",sep=""))
timestamp()

#-----	cmd.exe /c "R < d:/kamakura_r/ccar_pub_calibrate.r "5" "1" > d:/kamakura_r/tmp/ccar_pub_calibrate.out 2>&1"

args <- commandArgs()[-(1:2)]
cat(paste(args,sep="",collapse="\t"))

#----- lets get our universe
	if(length(args)==0) gvkeys <- getGvkeys(1,1)
	if(length(args)==2) {
		quantiles <- as.integer(args[1])
		quantile <- as.integer(args[2])
		gvkeys <- getGvkeys(quantiles,quantile)
	}
cat("\nlets do these:\t",min(gvkeys),"\tthrough:\t",max(gvkeys),"\n")

#-----	these are the kdps to be tested
kdp_cols <- c('kdp_1mo','kdp_3mo','kdp_1yr','kdp_5yr')

#-----	that which we seek to write to the database
ds.gvkey_terms <- data.frame(matrix(nrow=0,ncol=5))
	names(ds.gvkey_terms) <- c('gvkey','term','error_term','intercept','adj_r_sqr')
ds.gvkey_coeffs <- data.frame(matrix(nrow=0,ncol=6))
	names(ds.gvkey_coeffs) <- c('gvkey','term','rf_code','coeff','std_err','t_stat')

#-----	these are our ccar macro factor variables 
mfacs <- read.table(file='D://kamakura_r//tmp//ccar_test_1.csv',sep=",",header=T,colClasses='character')
#mfacs <- read.table(file='c://therocrates//ccar_test_1.csv',sep=",",header=T,colClasses='character')
mfac_pairs <- list(c('RGDP','NGDP'),c('RDISINC','NDISINC'),c('TREAS10Y','MORT'))
mfac_set1 <- c('CPI','UNEMP','TREAS3MO','BBBCORP','DJ1YR','HOUSE1YR','CRE1YR','VIX','RGDPEUR',
				'CPIEUR','FXEUR','RGDPASIA','CPIASIA','FXASIA','RGDPJPN','CPIJPN','FXJPN','RGDPUK','CPIUK','FXUK')
mfac_set2 <- c('CPI','UNEMP','TREAS3MO','BBBCORP','DJ1YR','HOUSE1YR','CRE1YR','VIX')

for(x in 1:ncol(mfacs)) mfacs[,x] <- as.numeric(mfacs[,x])
names(mfacs) <- ifelse(names(mfacs)=='YEARMO','year_mo',names(mfacs))

#-----	helper data frame
sset <- as.data.frame(matrix(nrow=nrow(mfacs),ncol=6))
	names(sset) <- c('gvkey','year_mo','kdp_1mo','kdp_3mo','kdp_1yr','kdp_5yr')
	sset$year_mo <- mfacs$year_mo

for(gvkey in gvkeys) {
cat(gvkey,"..")
	ds.gt <- ds.gvkey_terms
	ds.gc <- ds.gvkey_coeffs
	set <- sset
	kdp_values <- oq(paste("select jc.gvkey, substring(convert(char,jc.data_date,112),1,6) as year_mo,
							(case jc.kdp_1mo when 0 then -11.5129 when 100 then 11.5129 else
							-log((1-jc.kdp_1mo/100.0)/(jc.kdp_1mo/100.0)) end ) as kdp_1mo,
							(case jc.kdp_3mo when 0 then -11.5129 when 100 then 11.5129 else
							-log((1.0-jc.kdp_3mo/100.0)/(jc.kdp_3mo/100.0))  end) as kdp_3mo,
							(case jc.kdp_1yr when 0 then -11.5129 when 100 then 11.5129 else
							-log((1.0-jc.kdp_1yr/100.0)/(jc.kdp_1yr/100.0))  end) as kdp_1yr,                               
							(case jc.kdp_5yr when 0 then -11.5129 when 100 then 11.5129 else
							-log((1.0-jc.kdp_5yr/100.0)/(jc.kdp_5yr/100.0))  end) as kdp_5yr
							from kdp_ts.dbo.kdp_jc5 jc where jc.gvkey='",
					gvkey,"' order by data_date desc",sep=""))
	if(nrow(kdp_values)==0) next
	for(x in 1:nrow(set)) set[x,] <- kdp_values[kdp_values$year_mo==set$year_mo[x],][1,]
	set <- set[!is.na(set$gvkey),]
		set$year_mo <- as.integer(set$year_mo)
	for(x in 1:ncol(set)) set[,x] <- as.numeric(set[,x])
	num_obs <- nrow(set)
	if(num_obs < 12) next
	testset <- merge(set,mfacs,by.x='year_mo',all.x=T)
	gids <- oq(paste("select top 1 d.market_cap, c.gsubind, c.loc from market.dbo.security_daily d
				join market.dbo.company c on c.gvkey=d.gvkey where c.gvkey='",
				gvkey,"' and d.datadate<'",(max(testset$year_mo)*100)+1,"' order by datadate desc, iid",sep=""))
	notthese <- NULL
	if(min(kdp_values$year_mo) < 199609) notthese <- append(notthese,c('RGDPASIA','CPIASIA'))

	for(y in 1:(length(kdp_cols))) {
		term = toupper(substr(kdp_cols[y],5,7))
		gfacs <- NULL
		for(x in 1:length(mfac_pairs)) {
			formula_str <- as.formula(paste(paste(kdp_cols[y]," ~ ",sep=""),paste(mfac_pairs[[x]],sep="",collapse="+"),sep=""))
			coefs <- as.data.frame(summary(lm(formula_str,testset))[4])[-1,]
			gfacs <- append(gfacs,row.names(coefs[order(coefs[,4],decreasing=F),])[1])
		}
		if((gids$loc=='USA' & gids$market_cap>40000 & !is.element(substr(gids$gsubind,1,4),c('5510','4030'))) | gids$loc!='USA') {
			mf_cols <- c(mfac_set1,gfacs)
		} else {
			mf_cols <- c(mfac_set2,gfacs) 
		}

		mf_cols <- mf_cols[!is.element(mf_cols,notthese)]

		flag <- as.logical(length(mf_cols))
		while(flag) {
			formula_str <- as.formula(paste(paste(kdp_cols[y]," ~ ",sep=""),paste(mf_cols,sep="",collapse="+"),sep=""))
			thing <- lm(formula_str,testset)
			coefs <- as.data.frame(summary(thing)[4])
			intercept <- coefs[1,1]
			coefs <- coefs[-1,]
			flag <- as.logical(nrow(coefs[abs(coefs[,3])<2,]))
			if(nrow(coefs)==1) {
				flag <- F
			} else {
				coefs <- coefs[order(coefs[,4],decreasing=T),][-1,]
				mf_cols <- row.names(coefs)
			}
		}

		while(length(mf_cols)>num_obs*5) {
			formula_str <- as.formula(paste(paste(kdp_cols[y]," ~ ",sep=""),paste(mf_cols,sep="",collapse="+"),sep=""))
			thing <- lm(formula_str,testset)
			coefs <- as.data.frame(summary(thing)[4])
			intercept <- coefs[1,1]
			coefs <- coefs[-1,]
			coefs <- coefs[order(coefs[,4],decreasing=T),][-1,]
			mf_cols <- row.names(coefs)
		}

	sumthing <- summary(thing)
	error_term = sumthing$sigma
	adj_r_sqr = sumthing$adj.r.squared
	
	ds.gt[(nrow(ds.gt)+1),] <- cbind(gvkey,term,error_term,intercept,adj_r_sqr)
	gsset <- as.data.frame(matrix(nrow=nrow(coefs),ncol=6))
	names(gsset) <- names(ds.gvkey_coeffs)
		gsset$gvkey	<- 	gvkey
		gsset$term	<- 	term
		gsset$rf_code	<-	row.names(coefs)
		gsset$coeff	<-	coefs[,1]
		gsset$std_err	<-	coefs[,2]
		gsset$t_stat	<-	coefs[,3]
	ds.gc	<-	rbind(ds.gc,gsset)	
	}

ds.gt$gvkey <- as.character(ds.gt$gvkey)
ds.gt$error_term <- as.numeric(ds.gt$error_term)
ds.gt$intercept <- as.numeric(ds.gt$intercept)
ds.gt$adj_r_sqr <- as.numeric(ds.gt$adj_r_sqr)
fijiWriteTable(ds.gt,'rrf2.gvkey_terms_ccar',T)
fijiWriteTable(ds.gc,'rrf2.gvkey_coeffs_ccar',T)
}
timestamp()

