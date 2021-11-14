source("d:/kamakura_r/kamakura_Rutils.r")
setwd("d:/kamakura_r")
options(scipen=999)

datez <-tq("with m as ( select row_number() over(order by data_date) as idx, convert(varchar(8),data_date,112) as kdp_date 
								from guam.kdps.dbo.month_end_dates where gvkey='006066' and data_date>'2003-05-31' and data_date<'2006-10-30'
	) select m.kdp_date, convert(varchar(6),dateadd(mm,1,m.kdp_date),112)+'01' as date_in from m ")

datez$date_out <- NA
for(x in 1:ncol(datez)) datez[,x] <- as.character(datez[,x])
for(x in 4:length(datez$kdp_date)) datez$date_out[x-3] <- datez$kdp_date[x]
datez <- datez[!is.na(datez$date_out),]
datez <- datez[order(datez$kdp_date,decreasing=F),]
#-----	datez <- datez[-37,]

finalset <- data.frame(matrix(nrow=0,ncol=9))
	names(finalset) <- c('gvkey','iid','tic','conm','moret_3','kdp','gsubind','idx','data_date')
idxs <- c('500','400','600')
for(idx in idxs) {
cat("\n",idx,"\t")
	for(y in 1:nrow(datez)) {
		kdp_date <- datez$kdp_date[y]
		date_in <-  datez$date_in[y]
		date_out <- datez$date_out[y]
cat("\t\t",kdp_date,"\t",date_in,"\t",date_out,"\n")

		ids <- getIdx(d=kdp_date,idx=idx)[[2]]
		for(x in 1:ncol(ids)) ids[,x] <- as.character(ids[,x])
			ids$moret <- getTotalRet(ids$gvkey,ids$iid,date_in,date_out)

		kdps <- tq(paste("select k.gvkey, k.kdp_1mo, h.gsubind 
							from nauru.kdp_ts.dbo.kdp_JC6 k
							join nauru.kris.dbo.company_ticker h on h.gvkey=k.gvkey
							where k.model='JC6' and k.gvkey in('",
							paste(unique(ids$gvkey),sep="",collapse="','")
						,"') and k.data_date=cast('",kdp_date,"' as date)",sep=""))
							
		thisset <- merge(ids,kdps,by='gvkey')
			thisset$idx <- idx
			thisset$data_date <- kdp_date
finalset <- rbind(finalset,thisset)
	}
}

names(finalset) <- ifelse(names(finalset)=='moret','moret3',names(finalset))

ffinalset <- finalset[,c('data_date','gvkey','iid','tic','conm','gsubind','idx','kdp_1mo','moret3')]
for(x in 1:7) ffinalset[,x] <- as.character(ffinalset[,x])
for(x in 8:9) ffinalset[,x] <- as.numeric(ffinalset[,x])

tahitiWriteTable(ffinalset,'therocrates.kdps_morets3')

#######################
lets do some picking

source("c:/therocrates/kamakura_rutils.r")
setwd("c:/therocrates")
options(scipen=999)

datez <- tq("select distinct data_date from therocrates.dbo.kdps_morets3 order by data_date")[[1]]

thesedates <- datez[1]
for(z in 2:length(datez)) {
		if(z%%3==0) thesedates <- append(thesedates,datez[(z+1)])
}


finalset <- data.frame(matrix(NA,nrow=length(thesedates),ncol=12))
	names(finalset) <- c('data_date','ret_set1','ret_set2','ret_set3','ret_set4','ret_set5',
						'ret_set6','ret_set7','ret_set8','ret_set9','ret_set10','idx_ret')

for(y in 1:length(thesedates)) {
	
	set <- tq(paste("select data_date, gvkey, kdp_1mo, moret3 as ret from therocrates.dbo.kdps_morets3 where data_date='",
			thesedates[y],"' and moret3 is not null", sep=""))

		for(x in 1:ncol(set)) set[,x] <- as.character(set[,x])
			set$kdp_1mo <- as.numeric(set$kdp_1mo)
			set$ret <- as.numeric(set$ret)
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
	finalset[y,] <- cbind(thesedates[y],ret_set1,ret_set2,ret_set3,ret_set4,ret_set5,
			ret_set6,ret_set7,ret_set8,ret_set9,ret_set10,idx_ret)
}	
#----- since 2003
#finalset <- finalset_rets
finalset_rets <- finalset 
for(x in 2:ncol(finalset)) finalset[,x] <- cumprod(finalset[,x])

finalset <- rbind(append(thesedates[1],rep(1,11)),finalset)
	finalset$data_date[1:(nrow(finalset)-1)] <- thesedates
finalset <- finalset[-nrow(finalset),]
	finalset$idx <- 1:nrow(finalset)

plot_finalset(finalset,"c:/therocrates/JC6_top_bottom_2003")
#----- since 2007
finalset <- finalset_rets[finalset_rets$data_date>'20060930',]
thesedates  <- thesedates[thesedates>'20060930']

finalset <- rbind(append(thesedates[1],rep(1,11)),finalset)
	finalset$data_date[1:(nrow(finalset)-1)] <- thesedates
finalset <- finalset[-nrow(finalset),]
	finalset$idx <- 1:nrow(finalset)

plot_finalset(finalset,"c:/therocrates/JC6_top_bottom_2007")
#----- since 2012
finalset <- finalset_rets[finalset_rets$data_date>'20110930',]
thesedates  <- thesedates[thesedates>'20110930']

finalset <- rbind(append(thesedates[1],rep(1,11)),finalset)
	finalset$data_date[1:(nrow(finalset)-1)] <- thesedates
finalset <- finalset[-nrow(finalset),]
	finalset$idx <- 1:nrow(finalset)

plot_finalset(finalset,"c:/therocrates/JC6_top_bottom_2012")

#----- since 2014
finalset <- finalset_rets[finalset_rets$data_date>'20130930',]
thesedates  <- thesedates[thesedates>'20130930']

finalset <- rbind(append(thesedates[1],rep(1,11)),finalset)
	finalset$data_date[1:(nrow(finalset)-1)] <- thesedates
finalset <- finalset[-nrow(finalset),]
	finalset$idx <- 1:nrow(finalset)
plot_finalset(finalset,"c:/therocrates/JC6_top_bottom_2014")

	
plot_finalset <- function (finalset,outname) {	

			v <- NULL
			num_xlabs <- 6
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
							1-month mean return cumulative series") 
			plot(x, y, type="n",lty="solid",lwd=0,main=heading,
						xaxp		=	c(min(finalset$idx),max(finalset$idx),10),
						yaxp		=	c((min(vv)-.10),max(vv),6),
						xlim		=	c(min(x),max(x)),
						ylim		=	c((min(vv)-.10),max(vv)),
						axes		=	F)
			axis(side=1,tck=1,lty=3,col=colors()[147],at=v,labels=xlabels,lwd.ticks=1,lwd=0,tick=T)
			axis(side=2,tck=1,lty=3,col=colors()[147],at=vv,lwd.ticks=1,lwd=0)

			lines(x, finalset$ret_set1, type='l',col=colors()[258],lwd=3) 
			#lines(x, finalset$ret_set2, type='l',col='green3',lwd=2) 
			#lines(x, finalset$ret_set3, type='l',col=colors()[132],lwd=1) 
			#lines(x, finalset$ret_set4, type='l',col=colors()[129],lwd=1) 
			#lines(x, finalset$ret_set5, type='l',col=colors()[125],lwd=1) 
			#lines(x, finalset$ret_set6, type='l',col=colors()[142],lwd=1) 
			#lines(x, finalset$ret_set7, type='l',col=colors()[146],lwd=1) 
			#lines(x, finalset$ret_set8, type='l',col=colors()[150],lwd=1) 
			#lines(x, finalset$ret_set9, type='l',col=colors()[503],lwd=1) 
			lines(x, finalset$ret_set10, type='l',col='red',lwd=3) 
			lines(x, finalset$idx_ret, type='l',col='black',lwd=3) 

			legend(min(x),max(vv),
				c('Lowest KDPS','HIGHEST KDPS','SP 1500'),
#				names(finalset)[c(2:12],
				lty=c(rep(1,3)),
#				lty=c(rep(1,11)),
				lwd=c(3,
#					2,2,2,2,2,2,2,2,
					3,3),
				col= c(colors()[258],
#					'green3',colors()[132],colors()[129],colors()[125],
#					colors()[142],colors()[146],colors()[150],colors()[503],
				'red','black'),
				bg=colors()[15]
			)

			dev.copy(png,paste(outname,".png",sep=""),width=1080,height=607)
			dev.off()	
}

plot_finalset(finalset)

############################
