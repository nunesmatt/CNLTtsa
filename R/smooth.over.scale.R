smooth.over.scale<-function(x, det1, det2, lre, lreA, scale.range = NULL, Arange = NULL, Jstar = 20, splines = FALSE, positive = FALSE, dfS = 10, interpolate = FALSE){

# this is a function which does smoothing (over scale) of general spectral quantities, for outputs of cnlt.univ and cnlt.biv (same grid).

spec <- matrix(ncol=length(x), nrow=Jstar)

scale.range1 <- range(logb(unlist(lre),2))
min.s <- max(scale.range1[1], scale.range[1])	# should work with NULL arg
max.s <- min(scale.range1[2], scale.range[2])

scale <- seq(from=min.s, to=(max.s+0.01), length=(Jstar+1)) # add a little on to the max
s1 <- scale[1:Jstar]
s2 <- scale[2:(Jstar+1)]
mscale <- (s1+s2)/2

# convenience matching function
find.values <-function(a,b,c){

	findV <- sapply(1:length(a),function(i){length(which((c>=a[i])&(c < b[i])))})
	findV <- which(findV > 0)

	return(findV)
}

if(is.null(Arange)){
	Arange<-c(-Inf,Inf)  # set to silly values to include everything
}

for(k in 1:length(x)) {
	mat <- matrix(ncol=3, nrow=length(lre[[k]]))
	mat[,1] <- logb(lre[[k]],2)
	mat[,2] <- det1[[k]]*det2[[k]]
	mat[,3] <- lreA[[k]]
	
	# only use values with a certain alpha range
	mat <- (mat[(Arange[1]<= mat[,3])&(mat[,3]< Arange[2]),])

	# needs to have more than one value in mat
	if(length(mat) > 3){
		if(splines){
			xobs <- find.values(s1,s2,mat[,1])

			if(positive){
			# ensure spectral values are positive
			spline1 <- smooth.spline(mat[,1], sqrt(mat[,2]), df = dfS)
				if(interpolate){
					spec[,k] <-   (approx(spline1$x, spline1$y, xout = mscale, rule = 2)$y)^2
				}
				else{
					spec[xobs,k] <-   (approx(spline1$x, spline1$y, xout = mscale[xobs], rule = 2)$y)^2
				}
			}
			else{
				# standard spline smoothing (doesn't need to be positive)
				spline1 <- smooth.spline(mat[,1], (mat[,2]), df = dfS)
				if(interpolate){
					spec[,k] <-   (approx(spline1$x, spline1$y, xout = mscale, rule = 2)$y)
				}
				else{
					spec[xobs,k] <-   (approx(spline1$x, spline1$y, xout = mscale[xobs], rule = 2)$y)
				} 
			}
		}
		else{
			for(i in 1:Jstar){
				for(i in 1:Jstar){
					spec[i,k] <- mean(mat[(scale[i]<= mat[,1])&(mat[,1]< scale[i+1]),2])
				}
			}
		} 
	}
}

return(list(spec=spec, mscale=mscale))

}
