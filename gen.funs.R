### General Functions can be applied to any data frames.

## check whether a library is installed, if yes,load it; if not, install and load it.
check_install_load_libs <- function(libs){
	installed.libs <- rownames(installed.packages())
	for (lib in libs) {
		if (!lib %in% installed.libs){
			install.packages(lib)
		}
		library(lib, character.only=T)
	}
}

## convert a character vector to date
character.to.date = function(myvector, dt.format, dt.str.range=NA){
	if(!identical(dt.str.range, NA)){
		myvector = substr(myvector, first=dt.str.range[['first']], last=dt.str.range[["last"]])
	}
	myvector = as.Date(myvector, format = dt.format)
}

## set up the type of each column
set.col.typ = function(mydf, 
					   num.cols=NA, factor.cols=NA, date.cols=NA, 
					   date.format=NA, date.str.range=NA){
	if(!"data.frame" %in% class(mydf)){
		stop('mydf must be a data frame!')
	}
	mydf[colnames(mydf)] <- lapply(mydf[colnames(mydf)], as.character)
	col.names = colnames(mydf)
	if(!identical(num.cols, NA)){
		mydf[num.cols] <- lapply(mydf[num.cols], as.numeric)
	}
	if(!identical(factor.cols, NA)){
		mydf[factor.cols] <- lapply(mydf[factor.cols], as.factor)
	}
	if(!identical(date.cols, NA)){
		mydf[date.cols] <- lapply(mydf[date.cols], 
								  function(x){character.to.date(x, dt.format=date.format, dt.str.range=date.str.range)})
	}
	return(mydf)			
}

# get the summary statistics for each column, and output these statistics in a data frame.
summary.statistics = function(mydf){
	if(!"data.frame" %in% class(mydf)){
		stop("mydf must be a data frame!")
	}
	cols = colnames(mydf)
	cols.stats = foreach(col = cols, .combine=rbind) %do% {
		this.col = mydf[[col]]
		col.typ = class(this.col)
		col.n.na = sum(is.na(this.col))
		col.samples = paste0(sample(this.col, size=min(5, length(this.col))), collapse=", ")
		if(col.typ == "character" | col.typ == "factor"){
			col.n.unique = length(unique(this.col))	
			col.min = NA
			col.max = NA
			col.median = NA
			col.mean = NA
		}else{
			col.n.unique = NA
			col.min = as.character(min(this.col, na.rm=T))
			col.max = as.character(max(this.col, na.rm=T))
			col.median = as.character(median(this.col, na.rm=T))
			col.mean = as.character(mean(this.col, na.rm=T))
		}
		return(data.frame(ColName=col, 
						  ColType=col.typ,
						  ColNNA = col.n.na,
						  ColNUnique = col.n.unique,
						  ColMin = col.min,
						  ColMax = col.max,
						  ColMedian = col.median,
						  ColMean = col.mean,
						  ColSamples = col.samples))
	}
	return(cols.stats)
}






