cnlt.biv <-function (x1, x2 = NULL, f1, f2, P = 100, nkeep = 2, use.same.trajectories = FALSE, verbose = TRUE, ...) 
{

det1<-det2<-lre1<-lre2<-lreA1<-lreA2<-list()

if(is.null(x2)|identical(x1,x2)){
	# same grids
	same.grid<-TRUE
	x2 <- x1	# make sure in the case of no x2 input
	# use same trajectory set
	use.same.trajectories <- TRUE
}
else{
	same.grid<-FALSE
}

if(use.same.trajectories){
    if (verbose) {
        pb <- txtProgressBar(min = 0, max = 100, style = 3)
    }
    for (i in 1:P) {
        if (verbose) {
            setTxtProgressBar(pb, 100 * i/P)
        }
        veci <- sample(1:length(x1), length(x1) - nkeep, FALSE)
        ff1 <- fwtnppermC(x1, f1, nkeep = nkeep, mod = veci, ...)
        ff2 <- fwtnppermC(x2, f2, nkeep = nkeep, mod = veci, ...)

        det1[veci] <- mapply(c, det1[veci], as.list(ff1$coeffv[as.vector(veci)]))
        det2[veci] <- mapply(c, det2[veci], as.list(ff2$coeffv[as.vector(veci)]))
        lre1[veci] <- mapply(c, lre1[veci], as.list(ff1$lengthsremove))
        lre2[veci] <- mapply(c, lre2[veci], as.list(ff2$lengthsremove))
	lreA1[veci] <- mapply(c, lreA1[veci], as.list(ff1$Ialpha))
	lreA2[veci] <- mapply(c, lreA2[veci], as.list(ff2$Ialpha))

    }
    cat("\n")
}
else{
	# do runs independently
	tmp<-cnlt.univ(x1, f1, P = P, nkeep = nkeep, verbose = verbose, ...)
	tmp2<-cnlt.univ(x2, f2, P = P, nkeep = nkeep, verbose = verbose, ...)

	det1<-tmp$det1
	lre1<-tmp$lre
	lreA1<-tmp$lreA
	det2<-tmp2$det1
	lre2<-tmp2$lreA
	lreA2<-tmp2$lreA
}

if(same.grid){
	l<-list(x1 = x1, x2 = x2, det1 = det1, det2 = det2, lre = lre1, lreA = lreA1)
	class(l)<-append(class(l),c("cnlt","biv","SG"))
	return(l)
}
else{
	l<-list(x1 = x1, x2 = x2, det1 = det1, det2 = det2, lre1 = lre1, lre2 = lre2, lreA1 = lreA1, lreA2 = lreA2)
	class(l)<-append(class(l),c("cnlt","biv","DG"))
	return(l)
}

}


