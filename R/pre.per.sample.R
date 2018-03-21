pre.per.sample <-function(spec1, spec2){

spec11 <- spec1
spec22 <- spec2

Nr <- nrow(spec1)
Nc <- ncol(spec1)

#spec <- matrix(nrow=Nr, ncol=Nc)

for(i in 1:Nr){
	for(k in 1:Nc){
		a <- unlist(spec1[i,k])
		b <- unlist(spec2[i,k])
		MN <- min(length(a),length(b))

		#which points to take from each series?
		s1 <- sample(1:length(a), size=MN, replace = FALSE)
		s2 <- sample(1:length(b), size=MN, replace = FALSE)

		aa <- a[sort(s1)]
		bb <- b[sort(s2)]

		spec11[i,k] <- list(aa)
		spec22[i,k] <- list(bb)
	}
}

return(list(spec1=spec11, spec2=spec22))

}
