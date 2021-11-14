#########################
source("d:/kamakura_r/kamakura_Rutils.r")
setwd("d:/kamakura_r")
options(scipen=999)

datez <-tq("select convert(varchar(8),dateadd(dd,-1,cast(m.yearmo+'01' as date)),112) as date_in,convert(varchar(8),dateadd(dd,-1,cast(n.yearmo+'01' as date)),112) as date_out from( select row_number() over(order by yearmo) as idx, yearmo from (select distinct convert(varchar(6),data_date,112) as yearmo from okinawa.kdp_ts.dbo.kdp_jc5 where gvkey='006066') z) m join(select (row_number() over(order by yearmo))-1 as idx, yearmo from (select distinct convert(varchar(6),data_date,112) as yearmo from okinawa.kdp_ts.dbo.kdp_jc5 where gvkey='006066') z) n on m.idx=n.idx")
for(x in 1:ncol(datez)) datez[,x] <- as.character(datez[,x])
datez <- datez[substring(datez$date_in,1,4)>1994,]
datez <- datez[order(datez$date_in,decreasing=F),]
idxs <- c('1500')
for(idx in idxs) {
cat("\n",idx,"\t")
	for(y in 1:(nrow(datez)-5)) {
	d <- as.character(tq(paste("select convert(varchar(8),dateadd(dd,1,'",datez$date_in[y],"'),112)"))[[1]])
		date_out <- datez$date_out[y+5]
cat("\t\t",d,"\t",date_out,"\n")
		ids <- getIdx(d=d,idx=idx)[[2]]
				for(x in 1:ncol(ids)) ids[,x] <- as.character(ids[,x])
		gvkeystr <- paste("''",paste(ids$gvkey,sep="",collapse="'',''"),"''",sep="")
		kdps <- tq(paste("EXEC COMPUSTAT.[dbo].[get_monthly_kdps_dprc_divs] 'kdps','",datez$date_in[y],"','",gvkeystr,"'",sep=""))
				for(x in 1:ncol(kdps)) kdps[,x] <- as.character(kdps[,x])
			ids$moret_6 <- getTotalRet(ids$gvkey,ids$iid,d,date_out)
		kdps <- merge(kdps,ids[,c('gvkey','moret_6')],by='gvkey',all.x=T)
		for(x in 4:ncol(kdps)) kdps[,x] <- as.numeric(kdps[,x])
		kdps$idx <- idx
tahitiWriteTable(kdps[,-2],'therocrates.kdps_6morets')
	}
}

#########################
truncate table therocrates.dbo.kdps_morets
truncate table therocrates.dbo.kdps_3morets
truncate table therocrates.dbo.kdps_6morets

now lets fill these tables up
source("d:/kamakura_r/kamakura_Rutils.r")
setwd("d:/kamakura_r")
options(scipen=999)

ids <- read.table("d:/Kamakura_r/quick.txt",colClasses="character",header=T)
		for(x in 1:ncol(ids)) ids[,x] <- as.character(ids[,x])
gvkeystr <- paste("''",paste(ids$gvkey,sep="",collapse="'',''"),"''",sep="")

datez <-tq("select convert(varchar(8),dateadd(dd,-1,cast(m.yearmo+'01' as date)),112) as date_in,convert(varchar(8),dateadd(dd,-1,cast(n.yearmo+'01' as date)),112) as date_out from( select row_number() over(order by yearmo) as idx, yearmo from (select distinct convert(varchar(6),data_date,112) as yearmo from okinawa.kdp_ts.dbo.kdp_jc5 where gvkey='006066') z) m join(select (row_number() over(order by yearmo))-1 as idx, yearmo from (select distinct convert(varchar(6),data_date,112) as yearmo from okinawa.kdp_ts.dbo.kdp_jc5 where gvkey='006066') z) n on m.idx=n.idx")

		for(x in 1:ncol(datez)) datez[,x] <- as.character(datez[,x])
		datez <- datez[substring(datez$date_in,1,4)>1994,]
		datez <- datez[order(datez$date_in,decreasing=F),]

	for(y in 1:(nrow(datez)-1)) {
		d <- as.character(tq(paste("select convert(varchar(8),dateadd(dd,1,'",datez$date_in[y],"'),112)"))[[1]])
		date_out <- datez$date_out[y]
cat("\t\t",d,"\t",date_out,"\n")
		kdps <- tq(paste("EXEC COMPUSTAT.[dbo].[get_monthly_kdps_dprc_divs] 'kdps','",datez$date_in[y],"','",gvkeystr,"'",sep=""))
				for(x in 1:ncol(kdps)) kdps[,x] <- as.character(kdps[,x])
				ids$moret_1 <- getTotalRet(ids$gvkey,ids$iid,d,date_out)
		kdps <- merge(kdps,ids[,c('gvkey','moret_1')],by='gvkey',all.x=T)
		for(x in 4:ncol(kdps)) kdps[,x] <- as.numeric(kdps[,x])
		kdps$idx <- NA
tahitiWriteTable(kdps[,-2],'therocrates.kdps_morets')
	}
}

#############

source("c:/therocrates/kamakura_rutils.r")
setwd("c:/therocrates")
options(scipen=999)

datez <- tq("select distinct datadate from therocrates.dbo.kdps_3morets order by datadate")[[1]]
secs  <- c('10','15','20','25','30','35','40','45','50','55','60')

for(y in 1:(length(datez)-3)) {
			set <-  tq(paste("select c.gsector, k.gvkey, k.kdp_1mo, k.moret_3 from therocrates.dbo.kdps_3morets k ",
								"join compustat.dbo.company c on c.gvkey=k.gvkey where datadate='",datez[y],"' ",
								"and moret_3 is not null order by kdp_1mo desc",sep=""))

			for(x in 1:ncol(set)) set[,x] <- as.character(set[,x])
				set$kdp_1mo <- as.numeric(set$kdp_1mo)
				set$moret_3 <- as.numeric(set$moret_3)

				thisset <- data.frame(matrix(NA,nrow=0,ncol=3))
					names(thisset) <- c('gvkey','kdp_1mo','moret_3')

				for(sec in secs) thisset <- append(thisset,set[set$gsector==sec,][1:10,])


				set <- set[order(set$kdp_1mo,decreasing=F),]
			n <- trunc(nrow(set)/10)
		ret_set1 <- mean(set[1:n,'ret'])
		ret_set2 <- mean(set[(n+1):(n*2),'ret'])
		ret_set3 <- mean(set[(n*2+1):(n*3),'ret'])
		ret_set4 <- mean(set[(n*3+1):(n*4),'ret'])
		ret_set5 <- mean(set[(n*4+1):(n*5),'ret'])
		ret_set6 <- mean(set[(n*5+1):(n*6),'ret'])
		ret_set7 <- mean(set[(n*6+1):(n*7),'ret'])
		ret_set8 <- mean(set[(n*7+1):(n*8),'ret'])
		ret_set9 <- mean(set[(n*8+1):(n*9),'ret'])
		ret_set10 <- mean(set[(n*9+1):nrow(set),'ret'])
		idx_ret <- mean(set[,'ret'])
		finalset[y,] <- cbind(datez[y],ret_set1,ret_set2,ret_set3,ret_set4,ret_set5,
				ret_set6,ret_set7,ret_set8,ret_set9,ret_set10,idx_ret)
}	
finalset_rets <- finalset 
thesesets <- list(finalset_rets[c(1,4,7,10,13,16),],
				finalset_rets[c(2,5,8,11,14,17),],
				finalset_rets[c(3,6,9,12,15),]
			)

for(z in 1:length(thesesets)) {
	finalset <- thesesets[[z]]

	for(x in 2:ncol(finalset)) finalset[,x] <- cumprod(finalset[,x])
	finalset <- rbind(append(finalset$data_date[1],rep(1,11)),finalset)
	lastdt <- tq(paste("select convert(varchar(8),dateadd(mm,3,cast('",max(finalset$data_date),"' as date)),112)",sep=""))
	finalset$data_date <- append(thesesets[[z]]$data_date,lastdt)
	finalset$idx <- 1:nrow(finalset)

	v <- NULL
	num_xlabs <- nrow(finalset)-1
	for(xx in 1:num_xlabs) v <- append(v,xx/num_xlabs)
	v <- append(1,trunc(v*nrow(finalset)))
	xlabels <- finalset$data_date[v]

	vv <- NULL
	for(xx in 2:11) vv <- unique(append(vv,round(as.numeric(finalset[,xx]),2)))

	vv <- append(vv,(max(vv)+.05))
	vv <- vv[order(vv,decreasing=T)]
	vv <- unique(round(vv,1))

	x <- finalset$idx
	y <- finalset$idx_ret
	par(pch=22, col="black") # plotting symbol and color 
	heading = paste("S&P 1500 performance quantiled by JC6 1-month KDP
					3-month mean return cumulative series") 

		plot(x, y, type="n",lty="solid",lwd=0,main=heading,
			xaxp		=	c(min(finalset$idx),max(finalset$idx),10),
			yaxp		=	c((min(vv)-.10),max(vv),6),
			xlim		=	c(min(x),max(x)),
			ylim		=	c((min(vv)-.10),max(vv)),
			axes		=	F)
		axis(side=1,tck=1,lty=3,col=colors()[147],at=v,labels=xlabels,lwd.ticks=1,lwd=0,tick=T)
		axis(side=2,tck=1,lty=3,col=colors()[147],at=vv,lwd.ticks=1,lwd=0)

		lines(x, finalset$ret_set1, type='l',col=colors()[258],lwd=3) 
		lines(x, finalset$ret_set2, type='l',col='green3',lwd=2) 
		lines(x, finalset$ret_set3, type='l',col=colors()[132],lwd=1) 
		lines(x, finalset$ret_set4, type='l',col=colors()[129],lwd=1) 
		lines(x, finalset$ret_set5, type='l',col=colors()[125],lwd=1) 
		lines(x, finalset$ret_set6, type='l',col=colors()[142],lwd=1) 
		lines(x, finalset$ret_set7, type='l',col=colors()[146],lwd=1) 
		lines(x, finalset$ret_set8, type='l',col=colors()[150],lwd=1) 
		lines(x, finalset$ret_set9, type='l',col=colors()[503],lwd=1) 
		lines(x, finalset$ret_set10, type='l',col='red',lwd=3) 
		lines(x, finalset$idx_ret, type='l',col='black',lwd=3) 

		legend(min(x),mean(vv),
			names(finalset)[2:12],
			lty=c(rep(1,11)),
			lwd=c(3,2,2,2,2,2,2,2,2,3,3),
			col= c(colors()[258],'green3',colors()[132],colors()[129],
				colors()[125],colors()[142],colors()[146],colors()[150],
				colors()[503],'red','black'),
			bg=colors()[15]
		)
dev.copy(png,paste("./quarterly_",z,".png",sep=""),width=1080,height=607)
dev.off()		
}

