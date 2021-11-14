# Connection and query routines 

trychm <- try(typeof(chm), silent=T)
trychs <- try(typeof(chs), silent=T)

if(class(trychm) == "try-error" || class(trychs) == "try-error") {

	user <- as.character(system("whoami", T))
	source(paste("/home/", user, "/proj/fsys/util/dbQueries.r", sep=""))

	MalthusUser <- "farm"
	MalthusPw <- "erjohn"

	library(RODBC)

	dbConnectMalthus <- function() {
		res <- odbcConnect("malthus", MalthusUser, MalthusPw, believeNRows=F)
		assign("chm", res, env=.GlobalEnv)
		invisible()
	}

	dbDisconnectMalthus <- function() {
		try(odbcClose(chm), silent=T)
		invisible()
	}

	dbConnectMalthus()
	mqRetries <- as.integer(0)

	mq <- function(query="") {
		
		queryDone <- F
		mqRetries <- 0

		while(!queryDone && (mqRetries < 4)) {

			mqRetries <- mqRetries + 1
			resb <- F
			resa <- try(resb <- sqlQuery(chm, query, as.is=T), silent=T)
			queryDone <- T

			if(typeof(resb)=="logical") {
				cat("\nWARNING (c1): ", resa[[1]], "\n")
				if(substr(resa[[1]], 46, 88)=="first argument is not an open RODBC channel") {
					cat("WARNING (c1): Will try to reconnect after pausing for 60 seconds...\n")
					dbDisconnectMalthus()
					system("sleep 60")
					dbConnectMalthus()
					queryDone <- F
				} else {
					stop("cannot handle this error in dbconnect.r:\nresa[[1]]\n")
				}

			} else if(typeof(resb)=="character" && length(resb) > 0) {
				cat("WARNING (c2): ", resb[1], "\n", resb[2], "\n")

				errcode10 <- substr(resb[2], 1, 10)
				errcode5 <- substr(resb[2], 1, 5) 
				if(errcode10=="HY000 2006" || errcode10=="HY000 2013" ||
				   errcode5 =="S1T00" || errcode10=="HY000 2003") {
					# we got a disconnect -- handle it nicely
					cat("\nWARNING (c2): Will try to reconnect after pausing for 60 seconds...\n")
					cat("error codes are: ", errcode5, errcode10, "\n")
					dbDisconnectMalthus()
					system("sleep 60")
					dbConnectMalthus()
					queryDone <- F
				} else {
					stop(paste("cannot handle this error (", errcode10,
							   ") in dbconnect.r.\n", sep=""))
				}
			}

		}

		return(resb)
	}

	#modates <- getMonthlyDates()

	#lastPitDate <- getLastPitDate()
	#lastIbesDate <- getLastIbesDate()
	#lastDailyDate <- getLastDailyDate()

	#cat("Last Pit Date:     ", lastPitDate, "\n")
	#cat("Last Ibes BT Date: ", lastIbesDate, "\n")
	#cat("Last Daily Date:   ", lastDailyDate, "\n")

	# now connect to solow
	dbConnectSolow <- function() {
		res <- odbcConnect("solow","TRQA","TQA123P")
		assign("chs", res, env=.GlobalEnv)
		invisible()
	}

	dbConnectSolow()

	sq <- function(query="",as.is=F) {
		res <- sqlQuery(chs, query, max=0, rows_at_time=1024, believeNRows=F, as.is=as.is)
		names(res) <- tolower(names(res))
		return(res)
	}

	validdates <- getValiddates()
	mergeable_dsinfocodes <-  sq(paste("select seccode, vencode dsinfocode ",
							"from secmap where ventype=34 and rank=1"))
	cat("Last loaded date: ", validdates[length(validdates)], "\n") 

}


