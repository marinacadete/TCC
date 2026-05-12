#------------------------------------------------------------------------------------------#
# Comandos:
#       fit.model = ajuste
#       attach(dados)
#       source("envel_nbin.txt")
#       ligacao logaritmica
#------------------------------------------------------------------------------------------#
par(mfrow=c(1,1))
X =   model.matrix(fit.model)
n =   nrow(X)
p =   ncol(X)
fi =   fit.model$theta
w =   fi*fitted(fit.model)/(fi + fitted(fit.model))
W =   diag(w)
H =   solve(t(X)%*%W%*%X)
H =   sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h =   diag(H)
td =   resid(fit.model,type="deviance")/sqrt(1-h)
fi =   fit.model$theta
e =   matrix(0,n,100)
#
for(i in 1:100){
  resp =   rnegbin(n, fitted(fit.model),fi)
  fit =   glm.nb(resp ~ X)
  w =   fit$weights
  W =   diag(w)
  H =   solve(t(X)%*%W%*%X)
  H =   sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h =   diag(H)
  e[,i] =   sort(resid(fit,type="deviance")/sqrt(1-h))}
#
e1 =   numeric(n)
e2 =   numeric(n)
#
for(i in 1:n){
  eo =   sort(e[i,])
  e1[i] =   (eo[2]+eo[3])/2
  e2[i] =   (eo[97]+eo[98])/2}
#
med =   apply(e,1,mean)
faixa =   range(td,e1,e2)
par(pty="s")
qqnorm(td,xlab="Quantil da N(0,1)",
       ylab="Componente do Desvio", ylim=faixa, pch=16, main="",cex=2, cex.axis=1.5, cex.lab=1.5)
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="",lwd=2)
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="",lwd=2)
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="",lwd=2)
#------------------------------------------------------------------------------------------#                      