# Sample for SEC data evaluation for Ted @ Kamakura
# Ted wants 10 year history for Dow 30, plus 20 at random from each of the 5 market cap groups


# get the universe: DJIA 30 + 20 at random from each capgroup
uni <- getCurrentUniverse()
ss <- mq("select ctid, capgroup from ct.snapshot")
uni <- merge(uni, ss, by=c("ctid"), all.x=F, all.y=F)

dowtics <- read.csv("dow30.csv", as.is=T, header=F)[,1]
ds <- uni[is.element(uni$tic, dowtics),]
ds$group <- "DOW"
uni <- uni[!is.element(uni$tic, dowtics),]

for(i in 1:5) {
	x <- uni[uni$capgroup==i,]
	picks <- round(runif(20, 1, nrow(x)))
	x <- x[picks,]
	x$group <- as.character(i)
	ds <- rbind(ds, x)
}

# for each stock in the universe, collect the data:
for(i in 1:nrow(ds)) {

	# first the raw records:
	query <- paste("select * from sec.raw where cik=", ds$cik[i],
					" order by repdate, popdate, item", sep="")
	res <- mq(query)
	onames <- names(res)

	# add some extra keys & info
	res$gvkey <- ds$gvkey[i]
	res$iid <- ds$iid[i]
	res$tic <- ds$tic[i]
	res$group <- ds$group[i]
	res <- res[,c("gvkey", "iid", "tic", "group", onames)] # moving gvkey etc. to front
	if(i==1) { raw <- res
	} else { raw <- rbind(raw, res) }

	# now the digested records (dfin = digested financials)
	query <- paste("select * from ct.dfin where ctid=", ds$ctid[i],
					" and refdate >= 20030101 ",
					" order by refdate, popdate, period", sep="")
	res <- mq(query)
	onames <- names(res)
	
	# add some extra keys & info
	res$gvkey <- ds$gvkey[i]
	res$iid <- ds$iid[i]
	res$tic <- ds$tic[i]
	res$group <- ds$group[i]
	res <- res[,c("gvkey", "iid", "tic", "group", onames)] # moving gvkey etc. to front
	if(i==1) { dfin <- res
	} else { dfin <- rbind(dfin, res) }
	
}

write.table(raw, "raw_sample.csv", quote=F, sep="|", row.names=F, na="")
write.table(dfin, "dfin_sample.csv", quote=F, sep="|", row.names=F, na="")

