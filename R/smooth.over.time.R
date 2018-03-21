smooth.over.time <- function(x, spec, M, fact = 1){

nscale <- nrow(spec)
smooth.spec <- matrix(data = NA, nrow = nscale, ncol = ncol(spec))

if(length(M)%in%c(1,nscale)){
	# error checking

	if(length(M)==1){

	# smoothing window increases with scale according to fact
	v<-(1:nscale)-1
	Mvec <- M * fact^v
	}
	else{
		Mvec<-M
	}
}
else{
	stop("Please supply valid smoothing bandwidth(s)!")
}

for(i in 1:nscale){
	a <- cbind(order(x),x ,spec[i,])
	a <- a[is.na(a[,3])==FALSE,]

	# must have MORE THAN 1 obs at each scale
	if(length(a)<=3){
		smooth.spec[i,] <- rep(NA, dim(spec)[2] )
	}
	else{
		sm <- ksmooth(a[,2], a[,3], kernel="box", bandwidth=M[i], x.points =x[a[,1]])
		smooth.spec[i,a[,1]] <- sm$y
	}
}

return(smooth.spec)
}


