library(boot)

##
N_LINE <- TRUE
BOX<- FALSE

##
theta <- c(-2,-1,0,1,2)
BETA <- 1

##
par(mfrow=c(1,1), mar=c(5,0.5,3,0.5))
plot(0,0, ylim=c(-1,3), xlim=c(-3,3), type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
if(N_LINE==TRUE) abline(h=0, col=1, lwd=2)
if(BOX==TRUE) box()
lines(c(-1.5,-1.5), c(0,2), lty=2)
lines(c(0,0), c(0,2), lty=2)
lines(c(1.5,1.5), c(0,2), lty=2)
points(theta, c(0,0,0,0,0), pch=21, bg=grey(.9), col=1, cex=7.5)
text(-2,0, expression(paste(theta[1])), cex=2.5)
text(-1,0, expression(paste(theta[2])), cex=2.5)
text(0,0, expression(paste(theta[3])), cex=2.5)
text(1,0, expression(paste(theta[4])), cex=2.5)
text(2,0, expression(paste(theta[5])), cex=2.5)
axis(side=1, at=-5:5, cex.axis=1.5, line=0, tick=F)

points(-1.5, 2, pch=21, bg=grey(.99), col=1, cex=7.5)
points(0, 2, pch=21, bg=grey(.99), col=1, cex=7.5)
points(1.5, 2, pch=21, bg=grey(.99), col=1, cex=7.5)

text(-1.5, 2, expression(paste(alpha[1])), cex=3)
text(0, 2, expression(paste(alpha[2])), cex=3)
text(1.5, 2, expression(paste(alpha[3])), cex=3)
mtext(side=3, "item difficulty parameters", line=1, at=0, cex=2)
mtext(side=1, "subject latent traits", line=3.5, at=0, cex=2)





#########################
#########################
#########################
library(boot)

##
N_LINE <- TRUE
BOX<- FALSE

##
theta <- c(-2,-1,0,1,2)
Y <- c(0,1,1,2,3)
BETA <- 1

##
par(mfrow=c(1,1), mar=c(5,5,1,0.5))
plot(0,0, ylim=c(-.1,3.75), xlim=c(-3,3), type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
if(N_LINE==TRUE) abline(h=0, col=1, lwd=2)
if(BOX==TRUE) box()
polygon(c(-1.5,-1.5,0,0,1.5,1.5,4,4), c(0,1,1,2,2,3,3,0), col=grey(.95))
lines(c(-1.5,-1.5), c(0,3.5), lty=2)
lines(c(0,0), c(0,3.5), lty=2)
lines(c(1.5,1.5), c(0,3.5), lty=2)
lines(c(-5,5), c(3,3))
points(theta, Y, pch=21, bg=grey(.9), col=1, cex=7.5)
text(-2,0, expression(paste(theta[1])), cex=2.5)
text(-1,1, expression(paste(theta[2])), cex=2.5)
text(0,1, expression(paste(theta[3])), cex=2.5)
text(1,2, expression(paste(theta[4])), cex=2.5)
text(2,3, expression(paste(theta[5])), cex=2.5)
axis(side=1, at=-5:5, cex.axis=1.5, line=0, tick=F)
axis(side=2, at=0:3, cex.axis=1.5, line=0, tick=T, las=2)

points(-1.5, 3.5, pch=21, bg=grey(.99), col=1, cex=7.5)
points(0, 3.5, pch=21, bg=grey(.99), col=1, cex=7.5)
points(1.5, 3.5, pch=21, bg=grey(.99), col=1, cex=7.5)

text(-1.5, 3.5, expression(paste(alpha[1])), cex=3)
text(0, 3.5, expression(paste(alpha[2])), cex=3)
text(1.5, 3.5, expression(paste(alpha[3])), cex=3)
mtext(side=2, expression(paste("subject additive scale ", Y[i], "+")), line=3, cex=2)
mtext(side=1, "subject latent traits", line=3.5, at=0, cex=2)


