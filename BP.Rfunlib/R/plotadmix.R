## A function to plot ADMIXTURE results in a pretty way
##
## It takes an array with the Q's from ADMIXTURE as it's sole argument but 
## the Genotypes labels should be present as rownames in order for them to 
## appear on the x-axis. This array is sorted according to the plot intern-
## ally and can be returned by uncommenting the return keyword at the bottom.
## Bjorn Pieper. Cologne, July 2016.

plotadmix <- function(adm) {
  ladm <- length(adm)
  noclust <- adm[which(apply(adm, 1, max) < 0.5),]
  add <- ifelse(length(noclust[,1]) > 0, 1, 0)
  ranked <- cbind(1:ladm, abs(rank(colSums(adm))-(ladm+1)))
  for (i in 1:(ladm+add)) {
    tbar <- matrix(ncol=ladm, nrow=length(adm[,1]), data=NA)
    if (i <= ladm) {
      tmp <- adm[which(adm[,ranked[which(ranked[,2]==i),1]] >= 0.5),]
    } else if ( i > ladm ) {
      tmp <- noclust
    }
    tmp <- tmp[order(tmp[, which(abs(rank(colSums(tmp))-(ladm+1)) == 1)],
                       decreasing=TRUE),]
    tmp.rank <- order(seq(1, ladm)[abs(rank(colSums(tmp))-(ladm+1))])
    if (!exists('res')) { 
      res <- tmp
    } else {
      res <- rbind(res, tmp)
    }
    tmp <- tmp[,tmp.rank]
    if (!exists('progres')) {
      progres <- c(1, length(tmp[,1]))
    } else {
      progres <- c((progres[2]+1), (progres[2] + length(tmp[,1])))
    }
    tbar[seq(progres[1], progres[2]),] <- as.matrix(tmp)
    if (i > 1) { par(new=TRUE)}
    par(mar=c(4.35,4,1.5,0.1), lwd=0.5)
    barplot(t(tbar), col=rainbow(ladm)[tmp.rank], xaxt='n', yaxt='n')
  }
  xpos <- barplot(t(res), plot=FALSE)
  mtext(side=1, line=2.3, rownames(res), adj=0, at=xpos, cex=0.4, las=3)
  mtext(side=1, line=3.3, at=max(xpos)-(0.5*max(xpos)), "Genotypes", las=1) 
  axis(side=2, at=seq(0,1,0.1), las=1)
  mtext(side=2, line=3, at=0.5, "Ancestry", las=3) 
  if (length(noclust[,1]) > 1) {
    p1 <- matrix(ncol=2, nrow=2, data=c(tail(xpos,length(noclust[,1]))[1], 
                                        tail(xpos,1), 1.01, 1.01))
    par(xpd=NA)
    lines(p1, lwd=1)
    mtext(side=3, line=0.5, at=p1[1,1] + ((p1[2,1]-p1[1,1])/2),
          'admixed > 0.5', las=1, cex=0.8)
  }
  #return(res)
}
