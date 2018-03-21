cnltspec.plot <-function(spec, timevec, scalevec, zrange = NULL, xtitle = "Time", ytitle= "Scale", col.scale = tim.colors(64)[1:45], SFratio = 2, dt = 1, parsw = 3, axis4 = FALSE, frequencies = NULL){

# dt is the sampling interval (e.g dt = 60 is plotting minutes)

# other colour options include:
# grey col=gray((32:0)/32)
# rainbow, e.g. col= rainbow(n=50,start=(0.6)/6, end=2/6)

scaleP <- unique(round(scalevec)) 		#scale range
freqP <- (SFratio*2^scaleP)/dt 		#frequency range

### adding a legend strip "by hand"
switch(parsw,
{
	par( mar=c( 3.5,4,2,6.5)) # USED FOR 4 way plot
},
{
	par( mar=c( 3.5,4,2,6.5)) # USED FOR 2 way plot
},
{
	par( mar=c( 5,4,4,7.5)) # USED FOR Single plot
})

# plot blank image and fill in required spectral info

# remove potential NAs -- this will be zero for coherence and spectral values
spec[which(is.na(spec))]<-0

if(is.null(zrange)){
	zrange<-range(spec, na.rm = TRUE)
}


image(sort(timevec),scalevec,t(spec)[order(timevec),], xlab=xtitle,ylab=ytitle, add=FALSE,col=col.scale,zlim=zrange, cex.lab=1, axes=FALSE)
axis(1)
axis(2)
box()

if(axis4){
	if(is.null(frequencies)){
		axis(4, at= scaleP, tick=TRUE, labels= freqP); # automatic method for scale/freq ratio
	}
	else{
		# JUST PLOT THESE FOURIER FREQUENCIES
		axis(4, at= logb((frequencies*dt)/2,2), tick=TRUE, labels= frequencies) 
	}
}

image.plot(sort(timevec),scalevec,t(spec)[order(timevec),],legend.only=TRUE, zlim=zrange, graphics.reset=FALSE,col=col.scale)
}
