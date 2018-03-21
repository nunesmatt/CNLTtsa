pre.per <-function(x, det, lre, lreA, scale.range = NULL, time.range = NULL, Arange = NULL, Jstar = 20, Tstar = 50){

spec2 <- rep(0, Jstar*Tstar)

scale.range1 <- range(logb(unlist(lre),2))
min.s <- max(scale.range1[1], scale.range[1])
max.s <- min(scale.range1[2], scale.range[2])

time.range1 <-range(x)
min.t <- max(time.range1[1], time.range[1])
max.t <- min(time.range1[2], time.range[2])

# simpler version if nec.
#min.s <- scale.range[1]
#max.s <- scale.range[2]
#min.t <- time.range[1]
#max.t <- time.range[2]

# discretising the scale
scale <- seq(from=min.s, to=(max.s+0.01), length=(Jstar+1)) # add a little on to the max
s1 <- scale[1:Jstar] 
s2 <- scale[2:(Jstar+1)]
mscale <- (s1+s2)/2

# discretising the time
time <- seq(from=min.t, to=(max.t+0.01), length=(Tstar+1)) # add a little on to the max
t1 <- time[1:Tstar]
t2 <- time[2:(Tstar+1)]
mtime <- (t1+t2)/2

if(is.null(Arange)){
        Arange<-c(-Inf,Inf)  # set to silly values to include everything
}

for(k in 1:length(mtime)) {
	kpoint <- which((x< time[k+1])&(x >= time[k]))

	mat <- matrix(ncol=3, nrow=length(unlist(lre[kpoint])))
	mat[,1] <- logb(unlist(lre[kpoint]),2)
	mat[,2] <- unlist(det[kpoint])
	mat[,3] <- unlist(lreA[kpoint])

	#only use the values between a certain alpha range
	mat <- (mat[(Arange[1]<= mat[,3])&(mat[,3]< Arange[2]),])

	#NEEDS TO HAVE MORE THAN ONE VALUE IN MAT!
	if(length(mat) > 3){
		for(i in 1:Jstar){
			el <- list(mat[(scale[i]<= mat[,1])&(mat[,1]< scale[i+1]),2])
			if(length(el)==0){
				el <- NaN
			}
			spec2[i+(k-1)*Jstar] <- el
		}
	}
}

spec <- matrix(spec2,nrow=Jstar,ncol=Tstar,byrow=FALSE)

list(spec=spec, mscale=mscale,mtime=mtime)
}
