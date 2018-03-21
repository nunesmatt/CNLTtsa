cnlt.spec.SG <-function(x, M = 50, fact = 1, ...){

cnlt.obj<-x

# this functions does scale and time smoothing for cnlt.univ and cnlt.biv (same grid) objects.

# output from cnlt.univ has entries: x det lre lreA
# output from cnlt.biv has entries: x1 x2 det1 det2 lre lreA (same grid)

# check which spectral object we have:

if("SG"%in%class(cnlt.obj)){
	if("univ"%in%class(cnlt.obj)){
		univ<-TRUE	# convenience for subclass indicator
		x1<-cnlt.obj$x	
	}
	else{
		univ<-FALSE
		x1<-cnlt.obj$x1	# same as cnlt.obj$x2
	}
}
else{
	# the object is of the wrong type
	stop("Please supply a valid cnlt object!")
}

if(univ){

	# smooth individual spectra - these must be positive
	PerA1 <- smooth.over.scale(x1, sapply(cnlt.obj$det1,Re), sapply(cnlt.obj$det1,Re), cnlt.obj$lre, cnlt.obj$lreA, positive = TRUE, ...)
	PerA11 <- smooth.over.scale(x1, sapply(cnlt.obj$det1,Im), sapply(cnlt.obj$det1,Im), cnlt.obj$lre, cnlt.obj$lreA, positive = TRUE, ...)

	# smooth all spectral quantities over time
	P1 <- smooth.over.time(x1, spec=PerA1$spec, M = M, fact = fact)
	P11 <- smooth.over.time(x1, spec=PerA11$spec, M = M, fact = fact)

	S11 = (P1+P11)

	l<-list(S1 = S11, mscale = PerA1$mscale, mtime = x1)
}
else{

	# bivariate series

	# elements of c
	PerCR1 <- smooth.over.scale(x1, sapply(cnlt.obj$det1,Re), sapply(cnlt.obj$det2,Re), cnlt.obj$lre, cnlt.obj$lreA, positive = FALSE, ...)
	PerCR2 <- smooth.over.scale(x1, sapply(cnlt.obj$det1,Im), sapply(cnlt.obj$det2,Im), cnlt.obj$lre, cnlt.obj$lreA, positive = FALSE, ...)

	#combine
	PerC <- PerCR1$spec + PerCR2$spec

	# elements of q (the cross terms!!)

	PerQR1 <- smooth.over.scale(x1, sapply(cnlt.obj$det1,Im), sapply(cnlt.obj$det2,Re), cnlt.obj$lre, cnlt.obj$lreA, positive = FALSE, ...)
	PerQR2 <- smooth.over.scale(x1, sapply(cnlt.obj$det1,Re), sapply(cnlt.obj$det2,Im), cnlt.obj$lre, cnlt.obj$lreA, positive = FALSE, ...)

	# combine
	PerQ <- -(PerQR1$spec - PerQR2$spec)


	# and the individual spectra - these must be positive
	PerA1 <- smooth.over.scale(x1, sapply(cnlt.obj$det1,Re), sapply(cnlt.obj$det1,Re), cnlt.obj$lre, cnlt.obj$lreA, positive = TRUE, ...)
	PerA2 <- smooth.over.scale(x1, sapply(cnlt.obj$det2,Re), sapply(cnlt.obj$det2,Re), cnlt.obj$lre, cnlt.obj$lreA, positive = TRUE, ...)
	PerA11 <- smooth.over.scale(x1, sapply(cnlt.obj$det1,Im), sapply(cnlt.obj$det1,Im), cnlt.obj$lre, cnlt.obj$lreA, positive = TRUE, ...)
	PerA22 <- smooth.over.scale(x1, sapply(cnlt.obj$det2,Im), sapply(cnlt.obj$det2,Im), cnlt.obj$lre, cnlt.obj$lreA, positive = TRUE, ...)

	# smooth all spectral quantities over time

	PC <- smooth.over.time(x1, spec=PerC, M = M, fact = fact)
	PQ <- smooth.over.time(x1, spec=PerQ, M = M, fact = fact)

	P1 <- smooth.over.time(x1, spec=PerA1$spec, M = M, fact = fact)
	P2 <- smooth.over.time(x1, spec=PerA2$spec, M = M, fact = fact)
	P11 <- smooth.over.time(x1, spec=PerA11$spec, M = M, fact = fact)
	P22 <- smooth.over.time(x1, spec=PerA22$spec, M = M, fact = fact)

	S11 <- P1 + P11
	S22 <- P2 + P22

	coh <- sqrt(PC^2 + PQ^2)/sqrt(S11*S22) 	# coherence
	phase <- atan(-PQ/PC) 			# phase

	l<-list(coh = coh, phase = phase, C = PC, Q = PQ, S1 = S11, S2 = S22, mscale = PerA1$mscale, mtime = x1)

}

class(l)<-append(class(l),c(class(cnlt.obj),"spec"))

return(l)

}
