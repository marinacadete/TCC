#------------------------------------------------------------#
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ts <- resid(fit.model,type="pearson")/sqrt(1-h)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
di <- (h/(1-h))*(ts^2)
par(mfrow=c(2,2))
a <- min(td)
b <- max(td)
plot(fitted(fit.model), h,xlab="Valor Ajustado", ylab="Medida h",
     pch=16)
#identify(fitted(fit.model), h, n=1)
#
plot(di,xlab="Índice", ylab="Distância de Cook",
     pch=16)
#identify(di, n=1)
#
plot(td,xlab="Índice", ylab="Resíduo Componente do Desvio",
     ylim=c(a-1,b+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(td, n=1)
#
w <- fit.model$weights
eta <- predict(fit.model)
z <- eta + resid(fit.model, type="pearson")/sqrt(w)
plot(predict(fit.model),z,xlab="Preditor Linear", 
     ylab="Variavel z", pch=16)
lines(smooth.spline(predict(fit.model), z, df=2))
par(mfrow=c(1,1))
#---------------------------------------------------------------#