## toggle to T or F for some graph elements
N_LINE <- TRUE
BOX<- FALSE

## set the value for theta to plot
theta <- c(-2,-1,0,1,2)

##
par(mfrow=c(4,1), mar=c(6,4.5,3,0.5))
plot(0,0, ylim=c(-1,1), xlim=c(-5,5), type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
mtext(side=2, expression(paste(theta)), line=2, at=0, cex=2)
mtext(side=1, "Standard Normal Prior Distribution", line=4, cex=1.5)
if(N_LINE==TRUE) abline(h=0, col=1, lwd=2)
if(BOX==TRUE) box()
points(theta, c(0,0,0,0,0), pch=21, bg=grey(.9), col=1, cex=9)
text(-2,0, expression(paste(theta[1])), cex=3)
text(-1,0, expression(paste(theta[2])), cex=3)
text(0,0, expression(paste(theta[3])), cex=3)
text(1,0, expression(paste(theta[4])), cex=3)
text(2,0, expression(paste(theta[5])), cex=3)
axis(side=1, at=-5:5, cex.axis=2, line=0, tick=F)


plot(0,0, ylim=c(-1,1), xlim=c(-5,5), type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
mtext(side=2, expression(paste(theta + delta)), line=2, at=0, cex=2)
mtext(side=1, "Additive Invariance", line=4, cex=1.5)
abline(h=0, col=1)
if(N_LINE==TRUE) abline(h=0, col=1, lwd=2)
if(BOX==TRUE)box()
points(2+theta, c(0,0,0,0,0), pch=21, bg=grey(.9), col=1, cex=9)
text(0,0, expression(paste(theta[1])), cex=3)
text(1,0, expression(paste(theta[2])), cex=3)
text(2,0, expression(paste(theta[3])), cex=3)
text(3,0, expression(paste(theta[4])), cex=3)
text(4,0, expression(paste(theta[5])), cex=3)
axis(side=1, at=-5:5, cex.axis=2, line=0, tick=F)

plot(0,0, ylim=c(-1,1), xlim=c(-5,5), type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
mtext(side=2, expression(paste(delta %*% theta)), line=2, at=0, cex=2)
mtext(side=1, "Scale Invariance", line=4, at=0, cex=1.5)
abline(h=0, col=1)
if(N_LINE==TRUE) abline(h=0, col=1, lwd=2)
if(BOX==TRUE)box()
points(2*theta, c(0,0,0,0,0), pch=21, bg=grey(.9), col=1, cex=9)
text(-4,0, expression(paste(theta[1])), cex=3)
text(-2,0, expression(paste(theta[2])), cex=3)
text(0,0, expression(paste(theta[3])), cex=3)
text(2,0, expression(paste(theta[4])), cex=3)
text(4,0, expression(paste(theta[5])), cex=3)
axis(side=1, at=-5:5, cex.axis=2, line=0, tick=F)


plot(0,0, ylim=c(-1,1), xlim=c(-5,5), type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
mtext(side=2, expression(paste(-1 %*% theta)), line=2, at=library(boot)

##
N_LINE <- TRUE
BOX<- FALSE

##
theta <- c(-2,-1,0,1,2)
ALPHA <- -1.5
BETA <- 10

##
par(mfrow=c(2,1), mar=c(6,4.5,3,0.5))
plot(0,0, ylim=c(-1,3), xlim=c(-5,5), type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
#mtext(side=2, expression(paste(theta)), line=2, at=0, cex=2)
#mtext(side=1, "Standard Normal Prior Distribution", line=4, cex=1.5)
if(N_LINE==TRUE) abline(h=0, col=1, lwd=2)
if(BOX==TRUE) box()
lines(c(-ALPHA, -ALPHA), c(0,2.5), lty=2)
points(theta, c(0,0,0,0,0), pch=21, bg=grey(.9), col=1, cex=6.5)
text(-2,0, expression(paste(theta[1])), cex=2.5)
text(-1,0, expression(paste(theta[2])), cex=2.5)
text(0,0, expression(paste(theta[3])), cex=2.5)
text(1,0, expression(paste(theta[4])), cex=2.5)
text(2,0, expression(paste(theta[5])), cex=2.5)
axis(side=1, at=-5:5, cex.axis=1.5, line=0, tick=F)

points(-ALPHA, 2, pch=21, bg=grey(.99), col=1, cex=10.5)
text(-ALPHA,2, expression(paste(-alpha[k] / beta[k])), cex=1.5)

#text(-1,1, expression(paste(alpha[2])), cex=3)
#text(0,1, expression(paste(alpha[3])), cex=3)
#text(1,1, expression(paste(alpha[4])), cex=3)
#text(2,1, expression(paste(alpha[5])), cex=3)
mtext(side=1, expression(paste(theta[i])), line=4, at=0, cex=2)


plot(0,0, ylim=c(-0.2,1.2), xlim=(BETA*c(-5,5)), type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")

x <- BETA*seq(-5,5,.01)
lines(x,inv.logit(x), lwd=2)

axis(side=1, at=BETA*(-5:5), cex.axis=1.5, line=0, tick=F)
axis(side=2, at=c(0,.5,1), cex.axis=1.5, las=2, line=1, tick=T)

points(BETA*theta+ALPHA,inv.logit(BETA*theta+ALPHA), pch=21, bg=grey(.9), col=1, cex=6.5)

text(BETA*theta[1]+ALPHA, inv.logit(BETA * theta[1]+ALPHA), expression(paste(theta[1])), cex=2.5)
text(BETA*theta[2]+ALPHA, inv.logit(BETA * theta[2]+ALPHA), expression(paste(theta[2])), cex=2.5)
text(BETA*theta[3]+ALPHA, inv.logit(BETA * theta[3]+ALPHA), expression(paste(theta[3])), cex=2.5)
text(BETA*theta[4]+ALPHA, inv.logit(BETA * theta[4]+ALPHA), expression(paste(theta[4])), cex=2.5)
text(BETA*theta[5]+ALPHA, inv.logit(BETA * theta[5]+ALPHA), expression(paste(theta[5])), cex=2.5)


mtext(side=1, expression(paste(alpha[k] + beta[k] * theta[i])), line=4, at=0, cex=2)

if(BETA==1)text(4.5, .05, expression(paste(beta[k]==1)), cex=1.5, font=2.5)
if(BETA==2)text(9, .05, expression(paste(beta[k]==2)), cex=1.5, font=2.5)
if(BETA==10)text(45, .05, expression(paste(beta[k]==10)), cex=1.5, font=2.5)
if(BETA==1)text(-4.5,.95, expression(paste(Pr(y[ij]))), cex=1.5, font=2.5)
if(BETA==2)text(-9.5,.95, expression(paste(Pr(y[ij]))), cex=1.5, font=2.5)
if(BETA==10)text(-45,.95, expression(paste(Pr(y[ij]))), cex=1.5, font=2.5)
#text(-4,.9, expression(paste(Pr(y[ij]) | theta[i], alpha[k])), cex=1.5)








0, cex=2)
mtext(side=1, "Rotational Invariance", line=4, at=0, cex=1.5)
abline(h=0, col=1)
if(N_LINE==TRUE) abline(h=0, col=1, lwd=2)
if(BOX==TRUE)box()
points(-1*theta, c(0,0,0,0,0), pch=21, bg=grey(.9), col=1, cex=9)
text(-2,0, expression(paste(theta[5])), cex=3)
text(-1,0, expression(paste(theta[4])), cex=3)
text(0,0, expression(paste(theta[3])), cex=3)
text(1,0, expression(paste(theta[2])), cex=3)
text(2,0, expression(paste(theta[1])), cex=3)
axis(side=1, at=-5:5, cex.axis=2, line=0, tick=F)









