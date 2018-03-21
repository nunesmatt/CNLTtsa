cnlt.univ <-function (x, f, P = 100, nkeep = 2, verbose = TRUE, ...){

    dl <- ll <- ll2<- list()
    if (verbose) {
        pb <- txtProgressBar(min = 0, max = 100, style = 3)
    }
    for (i in 1:P) {
        if (verbose) {
            setTxtProgressBar(pb, 100 * i/P)
        }
        veci <- sample(1:length(x), length(x) - nkeep, FALSE)
        ff <- fwtnppermC(x, f, nkeep = nkeep, mod = veci, ...)
        dl[veci] <- mapply(c, dl[veci], as.list(ff$coeffv[as.vector(veci)]))
        ll[veci] <- mapply(c, ll[veci], as.list(ff$lengthsremove))
        ll2[veci] <- mapply(c, ll2[veci], as.list(ff$Ialpha))
    }
    cat("\n")

    l<-list(x = x, det1 = dl, lre = ll, lreA = ll2)
    
    class(l) <- append(class(l),c("cnlt","univ","SG"))
    return(l)
}
