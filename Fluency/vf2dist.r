function(d){
	sjs <- unique(d[,1])
	items <- unique(d[,2])
	nsjs <- length(sjs)
	nitems <- length(items)
	
	o <- matrix(0, nitems, nitems)
	rownames(o) <- items
	colnames(o) <- items
	
	for(i in sjs){
		sd <- d[d[,1]==i,2]
		indx <- match(sd, items)
		n <- length(sd)
		dsm <- as.matrix(dist(c(1:n)-1))/(n-1)
		dbg <- matrix(1, nitems, nitems)
		dbg[indx, indx] <- dsm
		o <- o + dbg
		}
	o
}