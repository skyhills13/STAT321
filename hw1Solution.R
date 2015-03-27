#1

sum = 1
n = 20000000
x = rep(0,n)
for( k in 2:n){
	if((1/k - log(k/(k-1))) > .Machine$double.eps){
		print("current k : ", k)	
		print(x)
		break
	}
	x = sum + (1/k - log(k/(k-1)))
	sum = x	
	cat("current k: ", k)
	cat("\n")
	print(x,15)
}
 
#1 end

#2

mySine <-function(x){
	sum <- 0
	temp <- 1
	for( k in 0:100) {
		if( abs(temp) < .Machine$double.eps ){	
			cat("current k :", k)
			cat("\n")
			return(sum)
		}
		if(k == 0){
			temp <- temp * x
			sum <- sum + temp
			next
		}
		temp <- temp*(-1)
		temp <- temp*x
		temp <- temp*x
		sum <- sum + (temp/factorial(2*k+1))	
	}
	return(sum)
}
# print(mySine(pi/4),15)
# print(sin(pi/4),15)

mySine(pi/4) == sin(pi/4)

#2 end

#3

sapply(1:1000, function(x) if(((1/x)*x) != 1) return(x))
sum(sapply(1:1000, function(x) length(which(((1/x)*x) != 1))))

#3 end