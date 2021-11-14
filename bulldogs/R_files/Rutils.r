#########################################################################################
#	Rutils.r written by Ted Spradlin -DECEMBER 2019 
#	1.  Running R 3.6.1
#	2.  download and install the DBI and RODBC packages located here
#		http://cran.r-project.org/web/packages/available_packages_by_name.html
#		or use R's load package functionality from within an R session
#	3. create a sql server 2008 data source for windows that R can use: 
#		this link:     http://statistical-research.com/wp-content/uploads/2012/09/database-connect.pdf
#		offers an excellent set of instructions for setting up a data connection on windows 
#		R can use these connections as a local client
#########################################################################################
#	leave this at defaults and it will write to your 'My Documents' folder in windows os
.Library.site <- "C:/Program Files/R/R-3.6.1/library"
library(RODBC)
library(DBI)
library(IMFData)

Sys.setenv(R_HISTSIZE=999999)
.First <- function() {
	mydir <<- ifelse(Sys.getenv('COMPUTERNAME') == 'TED-PC','/therocrates','D:/kamakura_r')
	Sys.setenv(HOME=mydir)
	Sys.setenv(R_USER=mydir)
	setwd(mydir)
	source(paste(mydir,"/kamakura_Rutils.r",sep=""))
} 

.Last <- function() {
	if(Sys.getenv('SESSIONNAME')=='Console') savehistory()
	odbcCloseAll()
}

if(!exists("tahiti")) {	#10.100.25.117
	dbdb <- 'TAHITI'
	dbuser <- 'TAHITI_User'
	dbpw <- 'An#51$q4!7g3'
	dbConnectTahiti <- function() {
		res <- odbcConnect(dbdb,dbuser,dbpw)
	if(res==-1)  { 
		paste("failed to connect to TAHIT") 
	} else {
		assign('tahiti',res,env=.GlobalEnv)
		invisible()
	}
	}
	dbConnectTahiti()
}

if(!exists("guam")) {	#10.100.25.117
	dbdb <- 'GUAM'
	dbuser <- 'GUAM_User'
	dbpw <- 'An#51$q4!7g3'
	dbConnectGuam <- function() {
			res <- odbcConnect(dbdb,dbuser,dbpw)
		if(res==-1)  { 
			paste("failed to connect to GUAM") 
		} else {
			assign('guam',res,env=.GlobalEnv)
			invisible()
		}
	}
	dbConnectTahiti()
}

if(!exists("therocrates") & Sys.getenv('COMPUTERNAME') == 'TED-PC') {
	dbdb <- 'therocrates'
	dbuser <- 'tspradlin'
	dbpw <- '123NEWpassword098_'
	dbConnectThero <- function() {
		res <- odbcConnect(dbdb,dbuser,dbpw,interpretDot=F)
	if(res==-1)  { 
		paste("failed to connect to THEROCRATES") 
	} else {
		assign('therocrates',res,env=.GlobalEnv)
		invisible()
	}
	}
	dbConnectThero()
}

sendQuery <- function(query="select name from sys.databases",dbh=tahiti,as.is=T) {
	res <- sqlQuery(dbh,query,as.is=as.is,errors=T,rows_at_time=1024)
	if(class(res) == 'character' && length(res) ==2) {
		res  <- append(res,odbcGetErrMsg(dbh))
		writeLines(res)
	} else {
		names(res) <- tolower(names(res))
	return(res)
	}
}

tq <- function(query="select name from sys.databases order by name",dbh=tahiti,as.is=T) {
	sendQuery(query,dbh,as.is)
}
thq	 <- function(query="select name from sys.databases order by name",dbh=therocrates,as.is=T) {
	sendQuery(query,dbh,as.is)
}
gq <- function(query="select name from sys.databases order by name",dbh=guam,as.is=T) {
	sendQuery(query,dbh,as.is)
}
kamaWriteTable <- function(ds, dblocation, append=T, dbh=tahiti) {
	strs <- unlist(strsplit(dblocation, "\\."))
		dbname <- strs[1]
		tablename <- strs[2]
	dblocation <- paste(dbname,".dbo.",tablename,sep="")
	sendQuery(paste("use", dbname),dbh,T)
	if(!append) sendQuery(paste("truncate table ", tablename,sep=""),dbh,T)
	names(ds) <- toupper(names(ds))
	sqlSave(dbh, ds, tablename, append=T, rownames=F, safer=T, fast=T)
	invisible()
}
tahitiWriteTable <- function(ds,dblocation,append=T,dbh=tahiti) {
	kamaWriteTable(ds,dblocation,append,dbh)
}
theroWriteTable <- function(ds,dblocation,append=T,dbh=therocrates) {
	kamaWriteTable(ds,dblocation,append,dbh)
}
guamWriteTable <- function(ds,dblocation,append=T,dbh=guam) {
	kamaWriteTable(ds,dblocation,append,dbh)
}

show_dbase_tables <- function(dbase='therocrates', db_con='tahiti') {
	query <- paste("select table_name from ",
				dbase,
			".information_schema.tables where table_type='base table'
			order by table_name", sep="")
	if(db_con=='tahiti')		res <- tq(query)
	if(db_con=='therocrates')	res <- thq(query)
return(res)
}

show_table_cols <- function(dbase = 'COMPUSTAT', table = 'COMPANY',db_con='TAHITI') {
query <- paste("select column_name as column_ , 
		(cast(data_type as varchar) + '  ' +
			(case when character_maximum_length is null then 
				(cast(numeric_precision as varchar) + ',' + 
				cast(numeric_precision_radix as varchar)) 
			else cast(character_maximum_length as varchar) end)) 
		as data_type 
		from ",dbase,".information_schema.columns 
		where table_name='",table,"'",sep="")
	if(db_con=='tahiti')		res <- tq(query)
	if(db_con=='therocrates')	res <- thq(query)

return(res)
}

mergeVectors <- function(dates_vector, merge_dates, merge_vals) {	# merge asynchronous data
	fullsetdates=dates_vector
	mdates=merge_dates
	mvals=merge_vals

	fullsetdates <- data.frame(fullsetdates)
		names(fullsetdates) <- c("date")
		fullsetdates$date <- as.integer(as.character(fullsetdates$date))
	mset <- data.frame(cbind(mdates, mvals))
		names(mset) <- c("date", "mvals")
		mset$date <- as.integer(as.character((mset$date)))
	msetone <- mset[mset$date <= fullsetdates$date[1],]

	mset <- merge(fullsetdates, mset, by="date", all.x=T, all.y=F)
	currentmval <- ifelse(nrow(msetone)>0, msetone$mvals[nrow(msetone)], NA)

		for(i in 1:nrow(mset)) {
			if(is.na(mset$mvals[i])) {
				mset$mvals[i] <- currentmval
			} else {
		currentmval <- mset$mvals[i]
			}
		}
	return(mset$mvals)
}
lagn <- function(d, n) {	#----- returns a vector that is d lagged by n
    if(length(d) <= n) return(rep(NA, length(d)))
    return(c(rep(NA, n), d[1:(length(d)-n)]))

}

uRank <- function(x, napos="mid") {
	lx <- length(x)
	if(lx==1) {
		if(is.na(x[1])) {
			if(napos=="lo") return (0)
			if(napos=="mid") return (0.5)
			if(napos=="hi") return (1)
		return(NA)
		}
	return(1)
	}
	dfo <- cbind((1:lx), x) 	# ----- preserve NA's here 
	df <- dfo[!is.na(dfo[,2]),]	
		t <- df[,1]
		x <- df[,2]
		r <- (rank(x) - 1) / (length(x) - 1)
	dfm <- merge(dfo, cbind(t, r), by=1, all.x=TRUE)
	#----- our new vector with NA's preserved 
	y <- dfm[,3]
	if(napos=="lo")	y <- ifelse(is.na(y), 0, y)
	if(napos=="hi")	y <- ifelse(is.na(y), 1, y)
	if(napos=='mid')	y <- ifelse(is.na(y),.5, y)
return(y)
}

ma <- function(x, n, na.rm=TRUE) {

    lx <- length(x)
    if(length(x) < n) { return(rep(NA, lx)) }
    if(sum(!is.na(x)) < n) { return(rep(NA, lx)) }

    if(na.rm==TRUE) {
        dfo <- cbind((1:lx), x)
        df <- dfo[!is.na(dfo[,2]),]
        t <- df[,1]
        x <- df[,2]
    }

    csx <- cumsum(x)
    csxlag <- c(rep(NA, (n-1)), 0, csx[0:(length(csx)-n)])
    mav <- (csx - csxlag) / n

    if(na.rm==TRUE) {
        dfm <- merge(dfo, cbind(t, mav), by=1, all.x=TRUE)
        mav <- dfm[,3]
    }

    return(mav)
}
MovingUnivariateRegression <- function(y, x, n, na.rm=TRUE, returnVal="residuals") {
	olen <- length(y)
	if(na.rm==TRUE) {
		dfo <- cbind(x,y)
		dfo <- data.frame(dfo)
		dfo$mergex <- 1:nrow(dfo)
		df <- dfo[!(is.na(dfo[,2]) | is.na(dfo[,1])),]
		x <- df[,1]
		y <- df[,2]
		mergexr <- df$mergex
	}
if (length(y) < n) return(rep(NA, olen))
	xy <- x * y
	csxy <- cumsum(xy)
	csxylag <- c(rep(NA, (n-1)), 0, csxy[0:(length(csxy)-n)])
	movsumxy <- csxy - csxylag
	xx <- x * x
	csxx <- cumsum(xx)
	csxxlag <- c(rep(NA, (n-1)), 0, csxx[0:(length(csxx)-n)])
	movsumxx <- csxx - csxxlag
	movxbar <- ma(x, n, na.rm)
	movybar <- ma(y, n, na.rm)
	movbeta <- (movsumxy - n*movxbar*movybar) / (movsumxx - n*(movxbar^2))
	movalpha <- movybar - movbeta*movxbar
	movresid <-  y - movbeta*x - movybar + movbeta*movxbar

	if(na.rm==TRUE) {
		dfm <- merge(dfo, cbind(mergexr, x, movresid, movbeta, movalpha),by=1, all.x=TRUE)
		xs <- dfm$x
		movresid <- dfm$movresid
		movbeta <- dfm$movbeta
		movalpha <- dfm$movalpha
	}
	percentchange <- xs*movbeta / movalpha

	if(returnVal=='all') {
		thing <- list(movalpha,movbeta,percentchange,movresid)
			names(thing) <- c('alpha','beta','pct_chg','resid')
		return(thing)
	}
	if(returnVal=="alphas") return(movalpha)
	if(returnVal=="betas") return(movbeta)
	if(returnVal=="percentChange") return(percentchange)
	#return((movbeta / (movalpha + xs*movbeta)))
return(movresid)     # default is to return residuals
}
getGrowthRate <- function(y, x, ppy, na.rm=TRUE) {

    # NA the negative or zero y's
    fy <- ifelse(y <= 0, NA, y)
    numy <- sum(!is.na(fy))
    if(numy >= 3) {
        b <- MovingUnivariateRegression(log(fy), x, numy, na.rm, returnVal="betas")
        b <- b[length(y)]
        ra <- exp(b*ppy) - 1
        return(100*ra)
    }
    return(NA)

}
getTimeStamp <- function() { return(format(Sys.time(),"%Y%m%d %a %X")) }

##########-----	 modified IMFData functions

DataflowMethod <- function(){
  r <- httr::GET('http://dataservices.imf.org/REST/SDMX_JSON.svc/Dataflow', httr::add_headers('user-agent' = ''))
  r.parsed <- jsonlite::fromJSON(httr::content(r, "text"))
  available.datasets <- r.parsed$Structure$Dataflows$Dataflow
  available.datasets.id <- available.datasets$KeyFamilyRef$KeyFamilyID
  available.datasets.text <- available.datasets$Name$`#text`
  available.db <- data.frame(
    DatabaseID = available.datasets.id,
    DatabaseText = available.datasets.text,
    stringsAsFactors = FALSE
  )
  return(available.db)
}

CompactDataMethod <- function(databaseID, queryfilter=NULL,
                              startdate='2001-01-01', enddate='2001-12-31',
                              checkquery = FALSE, verbose=FALSE, tidy=FALSE){
  if(checkquery){
    available.datasets <- DataflowMethod()$DatabaseID
    if (!is.element(databaseID, available.datasets)){
      stop('databaseID is not exist in API')
      return(NULL)
    }
    acceptedquery <- DataStructureMethod(databaseID)
    if (length(queryfilter) !=0 ||
        length(queryfilter) != length(acceptedquery)){
      stop('queryfilter is wrong format')
      return(NULL)
    }
  }

  queryfilterstr <- ''
  if (length(queryfilter) > 0){
    queryfilterstr <- paste0(
      unlist(plyr::llply(queryfilter,
                         function(x)(paste0(x, collapse="+")))), collapse=".")
  }

  APIstr <- paste0('http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/',
                    databaseID,'/',queryfilterstr,
                    '?startPeriod=',startdate,'&endPeriod=',enddate)
  r <- httr::GET(APIstr, httr::add_headers('user-agent' = ''))

  if(verbose){
    cat('\nmaking API call:\n')
    cat(APIstr)
    cat('\n')
  }

  if(httr::http_status(r)$reason != "OK"){
    stop(paste(unlist(httr::http_status(r))))
    return(list())
  }
  r.parsed <- jsonlite::fromJSON(httr::content(r, "text"))

  if(is.null(r.parsed$CompactData$DataSet$Series)){
    warning("No data available")
    return(NULL)
  }

  if(class(r.parsed$CompactData$DataSet$Series) == "data.frame"){
    r.parsed$CompactData$DataSet$Series <- r.parsed$CompactData$DataSet$Series[!plyr::laply(r.parsed$CompactData$DataSet$Series$Obs, is.null),]
    if(nrow(r.parsed$CompactData$DataSet$Series) ==0){
      warning("No data available")
      return(NULL)
    }
  }

  if(class(r.parsed$CompactData$DataSet$Series) == "list"){
    if(is.null(r.parsed$CompactData$DataSet$Series$Obs)){
      warning("No data available")
      return(NULL)
    }
    ret.df <- as.data.frame(r.parsed$CompactData$DataSet$Series[1:(length(r.parsed$CompactData$DataSet$Series)-1)])
    ret.df$Obs <- list(r.parsed$CompactData$DataSet$Series$Obs)
    names(ret.df) <- names(r.parsed$CompactData$DataSet$Series)
    r.parsed$CompactData$DataSet$Series <- ret.df
  }

  if(tidy){
    ret.df <- r.parsed$CompactData$DataSet$Series
    for(i in 1:length(ret.df$Obs)){
      ret.df$Obs[[i]] <- merge(ret.df$Obs[[i]], ret.df[i,1:(ncol(ret.df)-1)])
    }
    ret.df <- plyr::ldply(ret.df$Obs)
    return(ret.df)
  }

  return(r.parsed$CompactData$DataSet$Series)
}

##########-----	countries!
iso2s <- c('AD','AE','AF','AG','AI','AL','AM','AN','AO','AQ','AR','AS','AT','AU','AW','AZ','BA','BB',
			'BD','BE','BF','BG','BH','BI','BJ','BM','BN','BO','BR','BS','BT','BV','BW','BY','BZ',
			'CA','CC','CF','CG','CH','CI','CK','CL','CM','CN','CO','CR','CU','CV','CX','CY','CZ',
			'DE','DJ','DK','DM','DO','DZ','EC','EE','EG','EH','ER','ES','ET','FI','FJ','FK','FM',
			'FO','FR','FX','GA','GB','GD','GE','GF','GG','GH','GI','GL','GM','GN','GP','GQ','GR',
			'GS','GT','GU','GW','GY','HK','HM','HN','HR','HT','HU','ID','IE','IL','IM','IN','IO',
			'IQ','IR','IS','IT','JE','JM','JO','JP','KE','KG','KH','KI','KM','KN','KP','KR','KW',
			'KY','KZ','LA','LB','LC','LI','LK','LR','LS','LT','LU','LV','LY','MA','MC','MD','MG',
			'MH','MK','ML','MM','MN','MO','MP','MQ','MR','MS','MT','MU','MV','MW','MX','MY','MZ',
			'NA','NC','NE','NF','NG','NI','NL','NO','NP','NR','NU','NZ','OM','PA','PE','PF','PG',
			'PH','PK','PL','PM','PN','PR','PT','PW','PY','QA','RE','RO','RS','RU','RW','SA','SB',
			'SC','SD','SE','SG','SH','SI','SJ','SK','SL','SM','SN','SO','SR','SS','ST','SV','SY',
			'SZ','TC','TD','TF','TG','TH','TJ','TK','TM','TN','TO','TP','TR','TT','TV','TW','TZ',
			'UA','UG','UM','US','UY','UZ','VA','VC','VE','VG','VI','VN','VU','WF','WS','YE','YT',
			'YU','ZA','ZM','ZR','ZW')
			
iso3s <- c('AND','ARE','AFG','ATG','AIA','ALB','ARM','ANT','AGO','ATA','ARG','ASM','AUT','AUS','ABW','AZE','BIH',
			'BRB','BGD','BEL','BFA','BGR','BHR','BDI','BEN','BMU','BRN','BOL','BRA','BHS','BTN','BVT',
			'BWA','BLR','BLZ','CAN','CCK','CAF','COG','CHE','CIV','COK','CHL','CMR','CHN','COL','CRI',
			'CUB','CPV','CXR','CYP','CZE','DEU','DJI','DNK','DMA','DOM','DZA','ECU','EST','EGY','ESH',
			'ERI','ESP','ETH','FIN','FJI','FLK','FSM','FRO','FRA','FXX','GAB','GBR','GRD','GEO','GUF',
			'GGY','GHA','GIB','GRL','GMB','GIN','GLP','GNQ','GRC','SGS','GTM','GUM','GNB','GUY','HKG',
			'HMD','HND','HRV','HTI','HUN','IDN','IRL','ISR','IMN','IND','IOT','IRQ','IRN','ISL','ITA',
			'JEY','JAM','JOR','JPN','KEN','KGZ','KHM','KIR','COM','KNA','PRK','KOR','KWT','CYM','KAZ',
			'LAO','LBN','LCA','LIE','LKA','LBR','LSO','LTU','LUX','LVA','LBY','MAR','MCO','MDA','MDG',
			'MHL','MKD','MLI','MMR','MNG','MAC','MNP','MTQ','MRT','MSR','MLT','MUS','MDV','MWI','MEX',
			'MYS','MOZ','NAM','NCL','NER','NFK','NGA','NIC','NLD','NOR','NPL','NRU','NIU','NZL','OMN',
			'PAN','PER','PYF','PNG','PHL','PAK','POL','SPM','PCN','PRI','PRT','PLW','PRY','QAT','REU',
			'ROU','SRB','RUS','RWA','SAU','SLB','SYC','SDN','SWE','SGP','SHN','SVN','SJM','SVK','SLE',
			'SMR','SEN','SOM','SUR','SSD','STP','SLV','SYR','SWZ','TCA','TCD','ATF','TGO','THA','TJK',
			'TKL','TKM','TUN','TON','TMP','TUR','TTO','TUV','TWN','TZA','UKR','UGA','UMI','USA','URY',
			'UZB','VAT','VCT','VEN','VGB','VIR','VNM','VUT','WLF','WSM','YEM','MYT','YUG','ZAF','ZMB',
			'ZAR','ZWE')
			
these_countries <-  c('ANDORRA','UNITED ARAB EMIRATES','AFGHANISTAN','ANTIGUA AND BARBUDA','ANGUILLA',
			'ALBANIA','ARMENIA','NETHERLANDS ANTILLES','ANGOLA','ANTARCTICA','ARGENTINA',
			'AMERICAN SAMOA','AUSTRIA','AUSTRALIA','ARUBA','AZERBAIJAN','BOSNIA AND HERZEGOWINA',
			'BARBADOS','BANGLADESH','BELGIUM','BURKINA FASO','BULGARIA','BAHRAIN','BURUNDI','BENIN',
			'BERMUDA','BRUNEI DARUSSALAM','BOLIVIA','BRAZIL','BAHAMAS','BHUTAN','BOUVET ISLAND',
			'BOTSWANA','BELARUS','BELIZE','CANADA','COCOS ISLANDS','CENTRAL AFRICAN REPUBLIC',
			'CONGO','SWITZERLAND','COTE DIVOIRE','COOK ISLANDS','CHILE','CAMEROON','CHINA','COLOMBIA',
			'COSTA RICA','CUBA','CAPE VERDE','CHRISTMAS ISLAND','CYPRUS','CZECH REPUBLIC','GERMANY',
			'DJIBOUTI','DENMARK','DOMINICA','DOMINICAN REPUBLIC','ALGERIA','ECUADOR','ESTONIA',
			'EGYPT','WESTERN SAHARA','ERITREA','SPAIN','ETHIOPIA','FINLAND','FIJI','FALKLAND ISLANDS',
			'MICRONESIA, FEDERATED STATES OF','FAROE ISLANDS','FRANCE','FRANCE, METROPOLITAN','GABON',
			'UNITED KINGDOM','GRENADA','GEORGIA','FRENCH GUIANA','GUERNSEY','GHANA','GIBRALTAR',
			'GREENLAND','GAMBIA','GUINEA','GUADELOUPE','EQUATORIAL GUINEA','GREECE',
			'SOUTH GEORGIA AND SANDWICH ISLANDS','GUATEMALA','GUAM','GUINEA-BISSAU',
			'GUYANA','HONG KONG','HEARD AND MC DONALD ISLANDS','HONDURAS','CROATIA',
			'HAITI','HUNGARY','INDONESIA','IRELAND','ISRAEL','ISLE OF MAN','INDIA',
			'BRITISH INDIAN OCEAN TERRITORY','IRAQ','IRAN','ICELAND','ITALY','JERSEY',
			'JAMAICA','JORDAN','JAPAN','KENYA','KYRGYZSTAN','CAMBODIA','KIRIBATI','COMOROS',
			'SAINT KITTS AND NEVIS','NORTH KOREA','KOREA, REPUBLIC OF','KUWAIT','CAYMAN ISLANDS',
			'KAZAKHSTAN','LAO PEOPLES DEMOCRATIC REPUBLIC','LEBANON','SAINT LUCIA','LIECHTENSTEIN',
			'SRI LANKA','LIBERIA','LESOTHO','LITHUANIA','LUXEMBOURG','LATVIA','LIBYAN ARAB JAMAHIRIYA',
			'MOROCCO','MONACO','MOLDOVA, REPUBLIC OF','MADAGASCAR','MARSHALL ISLANDS','MACEDONIA',
			'MALI','MYANMAR','MONGOLIA','MACAU','NORTHERN MARIANA ISLANDS','MARTINIQUE','MAURITANIA',
			'MONTSERRAT','MALTA','MAURITIUS','MALDIVES','MALAWI','MEXICO','MALAYSIA','MOZAMBIQUE',
			'NAMIBIA','NEW CALEDONIA','NIGER','NORFOLK ISLAND','NIGERIA','NICARAGUA','NETHERLANDS',
			'NORWAY','NEPAL','NAURU','NIUE','NEW ZEALAND','OMAN','PANAMA','PERU','FRENCH POLYNESIA',
			'PAPUA NEW GUINEA','PHILIPPINES','PAKISTAN','POLAND','ST. PIERRE AND MIQUELON','PITCAIRN',
			'PUERTO RICO','PORTUGAL','PALAU','PARAGUAY','QATAR','REUNION','ROMANIA','SERBIA',
			'RUSSIAN FEDERATION','RWANDA','SAUDI ARABIA','SOLOMON ISLANDS','SEYCHELLES','SUDAN',
			'SWEDEN','SINGAPORE','ST. HELENA','SLOVENIA','SVALBARD AND JAN MAYEN ISLANDS','SLOVAKIA',
			'SIERRA LEONE','SAN MARINO','SENEGAL','SOMALIA','SURINAME','SOUTH SUDAN',
			'SAO TOME AND PRINCIPE','EL SALVADOR','SYRIAN ARAB REPUBLIC','SWAZILAND',
			'TURKS AND CAICOS ISLANDS','CHAD','FRENCH SOUTHERN TERRITORIES','TOGO','THAILAND',
			'TAJIKISTAN','TOKELAU','TURKMENISTAN','TUNISIA','TONGA','EAST TIMOR','TURKEY',
			'TRINIDAD AND TOBAGO','TUVALU','TAIWAN, PROVINCE OF CHINA','TANZANIA','UKRAINE',
			'UGANDA','UNITED STATES MINOR OUTLYING ISLANDS','UNITED STATES','URUGUAY','UZBEKISTAN',
			'HOLY SEE VATICAN CITY STATE','SAINT VINCENT AND THE GRENADINES','VENEZUELA',
			'BRITISH VIRGIN ISLANDS','U.S. VIRGIN ISLANDS','VIET NAM','VANUATU',
			'WALLIS AND FUTUNA ISLANDS','SAMOA','YEMEN','MAYOTTE','YUGOSLAVIA','SOUTH AFRICA','ZAMBIA','ZAIRE','ZIMBABWE'
)
			
countries <- data.frame(iso2s,iso3s,these_countries)