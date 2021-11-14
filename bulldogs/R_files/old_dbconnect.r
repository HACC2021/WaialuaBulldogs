# Connection and query routines 

if(!exists("DBCONNECT_IS_LOADED")) {

	user <- as.character(system("whoami", T))
	source(paste("/home/", user, "/proj/fsys/util/dbQueries.r", sep=""))

	MalthusUser <- "farm"
	MalthusPw <- "erjohn"

	library(RMySQL)		# get mysql library

	dbConnectMalthus <- function() {
		chm <- dbConnect(dbDriver("MySQL"), host="malthus", user=MalthusUser,
						 password=MalthusPw, dbname="fsys")
		return(chm)
	}

	dbDisconnectMalthus <- function() {
		dbDisconnect(chm)
		invisible()
	}

	chm <- dbConnectMalthus()
	validdates <- getValiddates()
	weekends <- getWeekends()
	DBCONNECT_IS_LOADED <- TRUE
	
	queriesOnActiveConnection <- 0	
	maxQueriesPerConnection <- 200

	dcat <- function(str) {
		# cat(str) # uncomment for debug mode
	}

	mq <- function(query="") {
		if(queriesOnActiveConnection >= maxQueriesPerConnection) {
			dcat(" xxx ")
			dbDisconnectMalthus()
			gc()
			assign("chm", dbConnectMalthus(), 1)
			assign("queriesOnActiveConnection", 0, 1)
		}
		assign("queriesOnActiveConnection", queriesOnActiveConnection + 1, 1)
		#res <- dbGetQuery(chm, query)
		dcat("=")
		res <- NA
		tryres <- try(res <- dbSendQuery(chm, query))
		dcat(".")
		if(class(tryres)=="try-error") {
			if(class(res)=="MySQLResult") try(dbClearResult(res))	
			return(data.frame())
		}
		dcat(".")
		tryres <- try(rs <- fetch(res, -1), silent=T)
		dcat(".")
		if(class(tryres)=="try-error") {
			if(class(res)=="MySQLResult") try(dbClearResult(res))	
			return(data.frame())
		}
		dcat(".")
		if(nrow(rs)==0) {
			if(class(res)=="MySQLResult") try(dbClearResult(res))	
			return(data.frame())
		}
		dcat(".")
		try(dbClearResult(res))
		dcat("+")
		return(rs)

	}

}




