#1 start

# 1) code start

A<-matrix(c(2,-5,4,1,-2.5,1,1,-4,6),byrow=T,nrow=3,ncol=3)
b<-matrix(c(-3,5,10),nrow=3,ncol=1)
p<-nrow(A)
U.pls<-cbind(A,b)
for(i in 1:p){
	for( j in (i+1):(p+1)) U.pls[i,j]<-U.pls[i,j]/U.pls[i,i]
	U.pls[i,i]<-1
	if (i<p){
 		for(k in (i+1):p) U.pls[k,]<-
 			U.pls[k,]-U.pls[k,i]/U.pls[i,i]*U.pls[i,]
 	}
}
U.pls

# 1) code end

결과:

> U.pls
     [,1] [,2] [,3] [,4]
[1,]    1 -2.5    2 -1.5
[2,]    0  1.0 -Inf  Inf
[3,]    0  0.0    1  NaN

문제 : Inf 와 NaN으로 인해 계산이 진행되지 않는다
해결 : 아주 작은 eps를 정하고 계산을 진행하며 값을 계속 swap해준다.

# 2) code start

A <- matrix(c(2,-5,4,1,-2,1,1,-4,6),byrow=T,nrow=3,ncol=3)
b <- matrix(c(-3,5,10),nrow=3,ncol=1)
p <- nrow(A)
eps <- 0.001

U.pls <- cbind(A,b)

for (i in 1:p){
    if((abs(U.pls[i,i]) < eps) & (i < p)){
            temp.v <- U.pls[i+1,] 
            U.pls[i+1,] <- U.pls[i,]
            U.pls[i,] <- temp.v
    }
    for (j in (i+1):(p+1)) U.pls[i,j] <- U.pls[i,j]/U.pls[i,i]
    U.pls[i,i] <- 1
    if (i < p) {
       for (k in (i+1):p) U.pls[k,] <- U.pls[k,] - U.pls[k,i]/U.pls[i,i]*U.pls[i,]
    }
}
U.pls

x <- rep(0,p)
for (i in p:1){
  if (i < p){
     temp <- 0
     for (j in (i+1):p) temp <- temp + U.pls[i,j]*x[j]
     x[i] <- U.pls[i,p+1] - temp
  }
  else x[i] <- U.pls[i,p+1]
}
x

# 2) code end

결과 :

> U.pls
     [,1] [,2] [,3] [,4]
[1,]    1 -2.5    2 -1.5
[2,]    0  1.0   -2 13.0
[3,]    0  0.0    1 31.0

> x
[1] 124  75  31


#1 end



#2 start

theta <- seq(0, 2*pi, pi/180)
r <- 1 - cos(theta)
x <- r*cos(theta); y<- r*sin(theta)
X <- cbind(x, y)
plot(X, xlim=c(-5,5)/2, ylim=c(-5,5)/2, xlab="", type="l",main="Rotation")
rotate<- function(X,theta){
 	A<-matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),2,2,byrow=T)
 	return (X %*% t(A))
}
colors <-c("red","green","blue","magenta","cyan")
for ( k in 1:5 ){
 	X.prime <- rotate(X,k*pi/3)
 	par(new=T)
 	plot(X.prime, xlim=c(-5,5)/2, ylim=c(-5,5)/2,col=colors[k], type="l")
} 

#2 end


#3 start

# code start

city <- read.table("city_index.txt", header=T)
str(city)

y<-city[,4]
n<- length(y)
intercept <-rep(1,n)
X<- as.matrix(cbind(intercept, city[,1:3]))
A<- t(X) %*% X
c<- t(X) %*% y
b<-solve(A) %*% c
round(b,4)

lm(House ~ Air+ Univ+ Price, data=city)

# code end

결과:

> round(b,4)
             [,1]
intercept 65.4046
Air        0.1032
Univ       0.0971
Price     -0.0802

> lm(House ~ Air+ Univ+ Price, data=city)

Call:
lm(formula = House ~ Air + Univ + Price, data = city)

Coefficients:
(Intercept)          Air         Univ        Price  
   65.40462      0.10318      0.09710     -0.08022  

비교 : 직접 코딩한 결과값의 경우 built-in function을 사용한 것보다 한자리 수 적게 나온다.
	즉, built-in function을 사용한 결과 값을 소숫점 아래 다섯번째 자리에서 반올림 한 값이
	직접 코딩한 결과 값이다.
	그러므로, built-in function이 조금 더 정밀하게 나온다.

#3 end



#4 start

# code start

aerobic <- read.table("aerobic.txt", header=T)
str(aerobic)
X<-as.matrix(aerobic)
n<-nrow(X)
Z<-scale(X)
A<-t(Z) %*% Z/(n)
eigen.A <- eigen(A)
round(eigen.A$values,7)
round(eigen.A$vectors,7)

princomp(Z)

# code end

결과:

> round(eigen.A$values,7)
[1] 2.8090634 1.7684493 0.9160841 0.7395142 0.3745632 0.1273241 0.0391953
> round(eigen.A$vectors,7)
           [,1]       [,2]       [,3]       [,4]       [,5]       [,6]
[1,]  0.1402390  0.5956740  0.0012896  0.4081775  0.6470877 -0.1957801
[2,] -0.2004129 -0.1624211 -0.9153226 -0.1347767  0.2688707 -0.0209937
[3,] -0.4150377  0.4420621 -0.0832464 -0.0716023 -0.4660849 -0.5927991
[4,] -0.3684532  0.0921087  0.3084802 -0.7390681  0.4629184  0.0060118
[5,] -0.4935128 -0.2595393  0.1843607  0.3717000  0.1363055  0.2298514
[6,] -0.4600817 -0.3650315  0.1240466  0.3351857  0.1546115 -0.3631811
[7,]  0.4206227 -0.4629043  0.1035198 -0.1156181  0.1870122 -0.6519588
           [,7]
[1,]  0.0429259
[2,]  0.0688593
[3,]  0.2272047
[4,] -0.0068478
[5,]  0.6674757
[6,] -0.6095364
[7,]  0.3531125
> 
> princomp(Z)
Call:
princomp(x = Z)

Standard deviations:
   Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7 
1.6760261 1.3298306 0.9571228 0.8599501 0.6120157 0.3568250 0.1979781 

비교 : princomp를 이용한 결과의 각 Comp.i를 제곱하면 
	직접 코딩한 결과 값을 마지막 자리수에서 반올림 한 값이 나온다.
	그러므로 직접 코딩하는 것이 상대적으로 세밀하게 나온다.

#4 end
