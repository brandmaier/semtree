
#vim <- varimp(forest, return.vector=T)

nacumsum <- function(x) {
	x[is.na(x)] <- 0
	return(cumsum(x))
}

varimpConvergencePlot <- function( x, lty=NULL, idx=NULL, 
                                     legend.x="topright", clw=4, 
                                     legend.cex=1.5,ylim=NULL,
                                     cex.lab=1,
                                     xlim=NULL, xlab=NULL,
                                     ylab=NULL, legend.bg=NULL, 
                                     legend.bty="n", na.omit=FALSE,
                                     extra.legend=NULL, ...) {

vim <- x

if (na.omit) {
  vim$importance
  vim$importance[is.na(vim$importance)] <- 0
}

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

# a crude display heuristic
# if more than 20 trees, base the ylim on everything
# but the first 10 trees which have a lot of fluctuation
# should rather be a percentage-type heuristic in the long run
if (N > 20) {
 heur.max <- max(pdata[10:nrow(pdata),], na.rm=TRUE)
 heur.min <- min(pdata[10:nrow(pdata),], na.rm=TRUE)
} else {
  heur.max <- max(pdata,na.rm=TRUE)
  heur.min <- min(pdata, na.rm=TRUE)
}

#heur.max <- quantile(pdata[10:nrow(pdata),], na.rm=T,probs=0.97)
#heur.min <- min(pdata[10:nrow(pdata),], na.rm=T)

if (is.null(ylim)) {
  ylim <- c(heur.min,heur.max)
}

if (is.null(xlim)) {
	xlim<-c(0,max(idx))
}


if (is.null(xlab)) {
  xlab<-"Number of Trees"
}

if (is.null(ylab)) {
 ylab<-"Absolute Increase in Misfit"
}

if (!is.null(extra.legend)) {
  #par(mar=par()$mar-c(0,0,0,-4))
  par(mar=c(5.1,4.1,4.1,4+extra.legend))
}

plot(0,0,xlim=xlim,ylim=ylim , 
     xlab=xlab,ylab=ylab,cex.lab=cex.lab,cex.axis=1.2,type="n",...)

# set NAs to 0 TODO: good default?

pdata[N,is.na(pdata[N,])] <- 0

# sort by last value
xs <- sort(pdata[N,],index.return=T)

for (j in 1:M) {
	i <- xs$ix[j]
	lines(pdata[idx,i],lwd=mylw,lty=lty[i],col=colors[i])
}

#legend <- c("Race","Mother's Edu","Father's Edu","Sex","Rnd10","Age","Rnd2")
leg <- rev(vim$var.names[xs$ix]) #[max(xs$ix)-xs$ix+1]

rix <- rev(xs$ix)
if (!is.null(legend.x)) {
  
  if (!is.null(extra.legend)) {
    par(xpd=TRUE)
    legend.x <- xlim[2]+ (xlim[2]-xlim[1])*0.05
    legend.y <- ylim[2] * 1.05
  } else {
    legend.y <- NULL
  }
  
  legend( x=legend.x,y=legend.y,
          leg,
          col=colors[rix],lwd=mylw, lty=lty[rix], 
          cex=legend.cex,bty=legend.bty, bg=legend.bg)
    
  
  #legend( legend.x,legend=vim$var.names[rix], col=colors[rix],lw=mylw, lty=lty[rix], cex=legend.cex,bty="n")
}

}


