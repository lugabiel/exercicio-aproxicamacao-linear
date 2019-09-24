#EXERCICIO 1 - APROXIMACAO POLINOMIAL
library('mltools')
mse()

rm(list=ls())       #realiza limpeza do espaço de variaveis
library('corpcor')  #biblioteca para usar pseudo-inversa
fgx<-function(xin) 0.5*xin^2+3*xin+10 #definicao da funcao

N<-20
X<-runif(n=N, min=-15,max=10) #amostrando x
Y<-fgx(X) + 10*rnorm(length(X)) #funcao amostrada cm erro normal

plot(X,Y, xlim = c(-30,30), ylim = c(-20,100)) #plota pontos no plano XY

#MODELO POLINOMIAL

H1<- cbind(X,1) #camada intermediaria de neuronios
H2<- cbind(X^2,X,1)
H3<- cbind(X^3,X^2,X,1) 
H4<- cbind(X^4,X^3,X^2,X,1) 
H5<- cbind(X^5,X^4,X^3,X^2,X,1) 
H6<- cbind(X^6,X^5,X^4,X^3,X^2,X,1) 
H7<- cbind(X^7,X^6,X^5,X^4,X^3,X^2,X,1) 
H8<- cbind(X^8,X^7,X^6,X^5,X^4,X^3,X^2,X,1) 
H9<- cbind(X^9,X^8,X^7,X^6,X^5,X^4,X^3,X^2,X,1) 
H10<- cbind(X^10,X^9,X^8,X^7,X^6,X^5,X^4,X^3,X^2,X,1) 

polylist <- c(H1,H2,H3,H4,H5,H6,H7,H8,H9,H10)

#calculo matriz pesos atraves da pseudo-inversa
flag<- 1


htest <- cbind(Xtest^2,Xtest, 1)
ytest <- htest %*% w #y a partir dos pesos pnderados                                               
par(new= T)
plot(Xtest, ytest, type='l',xlim = c(-15,10), ylim = c(0,80), col='blue', xlab=' ',ylab=' ')



Xtest <- seq(-15,10,0.5)
functest <- fgx(Xtest)
#par(new = T) #equivalente ao holdon do matlab

#plotando funcao aproximada
#plot(Xtest, functest, type='l',xlim = c(-30,30), ylim = c(-20,100), col='red', xlab=' ',ylab=' ')