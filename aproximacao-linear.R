#AULA 19/08/2019 - PLOTANDO UMA FUNCAO - CAPITULO 2

rm(list=ls())       #realiza limpeza do espa√ßo de variaveis
library('corpcor')  #biblioteca para usar pseudo-inversa
library('MLmetrics') #biblioteca para usar MSE(y_pred = Ytest, y_true = Y7)
fgx<-function(xin) 0.5*xin^2+3*xin+10 #definicao da funcao

N<-20
X<-runif(n=N, min=-15,max=10) #amostrando x
Y<-fgx(X) + 10*rnorm(length(X)) #funcao amostrada cm erro normal


plot(X,Y, xlim = c(-15,10), ylim = c(0,80)) #plota pontos no plano XY


Xtest <- seq(-15,10,0.5)
Ytrue <- fgx(Xtest)
par(new = T) #equivalente ao holdon do matlab

#plotando funcao original fgx() 
plot(Xtest, Ytrue, type='l',xlim = c(-15,10), ylim = c(0,80), col='azure', xlab=' ',ylab=' ')

#MODELO DE 1o GRAU
H1<- cbind(X,1) #camada intermedi?ria de neuronios
w2<-solve(t(H1) %*% H1) %*% t(H1) %*% Y #pesos 

H1 <- cbind(Xtest, 1)
Y1 <- H1 %*% w2 #y a partir dos pesos pnderados
matrix_resultado <- cbind(Y1)

par(new= T)
plot(Xtest, Y1, type='l',xlim = c(-15,10), ylim = c(0,80), col='lightblue', xlab=' ',ylab=' ')


#MODELO DE 2o GRAU
H2<- cbind(X^2,X,1) #camada intermedi?ria de neuronios
w2<-solve(t(H2) %*% H2) %*% t(H2) %*% Y #pesos 

H2 <- cbind(Xtest^2,Xtest, 1)
Y2 <- H2 %*% w2 #y a partir dos pesos pnderados
matrix_resultado <- cbind(matrix_resultado,Y2)

par(new= T)
plot(Xtest, Y2, type='l',xlim = c(-15,10), ylim = c(0,80), col='orange', xlab=' ',ylab=' ')

#MODELO DE 3o GRAU
H3<- cbind(X^3,X^2,X,1) #camada intermedi?ria de neuronios
w3<-solve(t(H3) %*% H3) %*% t(H3) %*% Y #pesos 

H3 <- cbind(Xtest^3,Xtest^2,Xtest, 1)
Y3 <- H3 %*% w3 #y a partir dos pesos ponderados
matrix_resultado <- cbind(matrix_resultado,Y3)

par(new= T)
plot(Xtest, Y3, type='l',xlim = c(-15,10), ylim = c(0,80), col='purple', xlab=' ',ylab=' ')

#MODELO 4 GRAU
H4 <- cbind(X^4,X^3,X^2,X,1)
w4 <- solve(t(H4) %*% H4) %*% t(H4) %*% Y

H2test <- cbind(Xtest^4,Xtest^3,Xtest^2,Xtest,1)
Y4 <- H2test %*% w4
matrix_resultado <- cbind(matrix_resultado,Y4)

par(new= T)
plot(Xtest, Y4, type='l',xlim = c(-15,10), ylim = c(0,80), col='blue', xlab=' ',ylab=' ')

#MODELO 5 GRAU
H4 <- cbind(X^5,X^4,X^3,X^2,X,1)
w4 <- solve(t(H4) %*% H4) %*% t(H4) %*% Y

H2test <- cbind(Xtest^5,Xtest^4,Xtest^3,Xtest^2,Xtest,1)
Y5 <- H2test %*% w4
matrix_resultado <- cbind(matrix_resultado,Y5)

par(new= T)
plot(Xtest, Y5, type='l',xlim = c(-15,10), ylim = c(0,80), col='aquamarine', xlab=' ',ylab=' ')

#MODELO 6 GRAU
H6 <- cbind(X^6,X^5,X^4,X^3,X^2,X,1)
w6 <- solve(t(H6) %*% H6) %*% t(H6) %*% Y

H2test <- cbind(Xtest^6,Xtest^5,Xtest^4,Xtest^3,Xtest^2,Xtest,1)
Y6 <- H2test %*% w6
matrix_resultado <- cbind(matrix_resultado,Y6)

par(new= T)
plot(Xtest, Y6, type='l',xlim = c(-15,10), ylim = c(0,80), col='red', xlab=' ',ylab=' ')


boxplot(matrix_resultado,main='Erro Quadr·tico MÈdio (E.Q.M.) vs Complexidade', col='lightblue',xlab='Ordem de complexidade modelo',ylab='E.Q.M. em relaÁ„o a p(x)')

readline(prompt="Press [enter] to continue e veja do erro de quadratico")
vetError <- 0
i<-0
for (Y in matrix_resultado) {
  vetError[i] <- MSE(y_pred = Y, y_true = Ytrue)
  vetVariancia <- var(vetError[i])
  i <- i +1
}
variancia <- c(var(vetError[0:50]),
               var(vetError[51:101]),
               var(vetError[102:152]),
               var(vetError[153:203]),
               var(vetError[204:254]),
               var(vetError[255:305]))

plot(variancia)

