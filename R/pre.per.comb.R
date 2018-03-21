pre.per.comb <-function(spec1, spec2){

Nr <- nrow(spec1)
Nc <- ncol(spec1)

spec <- matrix(nrow=Nr, ncol=Nc)

for(i in 1:Nr){
	for(k in 1:Nc){
		a <- unlist(spec1[i,k])
		b <- unlist(spec2[i,k])
		MN <- max(length(a),length(b))

		# padding for if the lengths are different
		if(length(a)<MN){
			a <- c(a,rep(NaN, (MN-length(a))))
		}
		if(length(b)<MN){
			b <- c(b,rep(NaN, (MN-length(a))))
		}

		ab <- a*b
		spec[i,k] <- mean(ab[is.na(ab)==FALSE])
	}
}

return(spec)

}
