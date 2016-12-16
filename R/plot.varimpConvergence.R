
#vim <- varimp(forest, return.vector=T)

nacumsum <- function(x) {
	x[is.na(x)] <- 0
	return(cumsum(x))
}

plot.varimpConvergence <- function( vim, lty=NULL, idx=NULL, 
                                     legend.x="topright", clw=4, 
                                     legend.cex=1.5,ylim=NULL,
                                     ...) {

impsum <- apply(vim$importance, 2, nacumsum)

M <- ncol(impsum)
N <- nrow(impsum)

if (is.null(lty)) {
	lty <- 1:M
}

mylw = clw

if (is.null(idx)) {
	idx <- 1:N
}

#colors = c("black","blue","green","red","orange","purple","pink")
colors = rainbow(M)

pdata <- matrix(0, nrow=N, ncol=M)
for (i in 1:M) {
	norm <- cumsum(!is.na(vim$importance[,i]))
	#norm <- 1:N
	pdata[,i] <- impsum[,i]/norm
}

heur.max <- max(pdata[10:nrow(pdata),], na.rm=T)
heur.min <- min(pdata[10:nrow(pdata),], na.rm=T)

#heur.max <- quantile(pdata[10:nrow(pdata),], na.rm=T,probs=0.97)
#heur.min <- min(pdata[10:nrow(pdata),], na.rm=T)

if (is.null(ylim)) {
  ylim <- c(heur.min,heur.max)
}

plot(0,0,xlim=c(0,max(idx)),ylim=ylim ,xlab="Number of Bootstrap Samples",ylab="Improvement",cex.lab=2,cex.axis=1.2,
     type="n",...)

# set NAs to 0 TODO: good default?

pdata[N,is.na(pdata[N,])] <- 0

# sort by last value
xs <- sort(pdata[N,],index.return=T)


for (j in 1:M) {
	i <- xs$ix[j]
	lines(pdata[idx,i],lwd=mylw,lty=lty[i],col=colors[i])
}

rix <- rev(xs$ix)
if (!is.null(legend.x)) {
 	legend( legend.x,legend=vim$var.names[rix], col=colors[rix],lwd=mylw, lty=lty[rix], cex=legend.cex,bty="n")
}

}