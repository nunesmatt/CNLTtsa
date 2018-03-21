cnlt.spec.DG <-function(x, M = 50, fact = 1, ...){

cnlt.obj<-x

# check if we have the right object:

if("DG"%in%class(cnlt.obj)){
	x1<-cnlt.obj$x1
	x2<-cnlt.obj$x2
}
else{
	stop("Please supply appropriate cnlt object!")
}

# compute "pre periodograms"

C1 <- pre.per(x1, sapply(cnlt.obj$det1,Re), cnlt.obj$lre1, cnlt.obj$lreA1, ...)
C2 <- pre.per(x2, sapply(cnlt.obj$det2,Re), cnlt.obj$lre2, cnlt.obj$lreA2, ...)
Q1 <- pre.per(x1, sapply(cnlt.obj$det1,Im), cnlt.obj$lre1, cnlt.obj$lreA1, ...)
Q2 <- pre.per(x2, sapply(cnlt.obj$det2,Im), cnlt.obj$lre2, cnlt.obj$lreA2, ...)

# sample appropriately to make sure they have the same number of coefficients

C12 <- pre.per.sample(C1$spec, C2$spec)
Q12 <- pre.per.sample(Q1$spec, Q2$spec)

# combine pre periodogram
# cross terms
CR1 <- pre.per.comb(C12$spec1, C12$spec2)
CR2 <- pre.per.comb(Q12$spec1, Q12$spec2)
QR1 <- pre.per.comb(Q12$spec1, C12$spec2)
QR2 <- pre.per.comb(C12$spec1,Q12$spec2)

# individual periodograms
R1 <- pre.per.comb(C12$spec1, C12$spec1)
I1 <- pre.per.comb(Q12$spec1, Q12$spec1)
R2 <- pre.per.comb(C12$spec2, C12$spec2)
I2 <- pre.per.comb(Q12$spec2, Q12$spec2)

# combine elements
PerC <- CR1+CR2
PerQ <- -(QR1-QR2)
PerR <- R1+I1
PerI <- R2+I2

# smooth over time
PC <- smooth.over.time(C1$mtime, PerC, M = M, fact = fact)
PQ <- smooth.over.time(C1$mtime, PerQ, M = M, fact = fact)
F1 <- smooth.over.time(C1$mtime, PerR, M = M, fact = fact)
F2 <- smooth.over.time(C1$mtime, PerI, M = M, fact = fact)

coh <- sqrt(PC^2+PQ^2)/sqrt(F1*F2)
phase <- atan(-PQ/PC)

l<-list(coh = coh, phase = phase, C = PC, Q = PQ, S1 = F1, S2 = F2, mscale = C1$mscale, mtime = C1$mtime)

class(l)<-append(class(l),c(class(cnlt.obj),"spec"))

return(l)

}
