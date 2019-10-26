getwd()
setwd("C:/Users/MaJiD/Desktop/Summer-2019/R/R-practice")

#Exexrcise 2.1.
before=c(78,72,78,79,105)
after=c(67,65,79,70,93)

weight_loss=before-after
weight_loss
mean(weight_loss)

#Exexrcise 2.2.
#not important

#Exexrcise 2.3.

i<-seq(1,12,by=0.5)
i
ii<-seq(1:10)^3
ii
iii<-(rep(c(1,-1),100)/seq(1,100))
iii
iv<-seq(1,49)*rep(c(1,0),49)
iv
v<-cumsum(1:10)
v
vi<-rep(1:10,1:10)
vi

#Exexrcise 2.4.

y=((-1)^((1:100)+1))*(((0.5)^(1:100))/(1:100))




#Exexrcise 2.5.
x<-LETTERS
x
i<-x[1:12]
i
ii<-x[seq(1,26,by=2)]
ii
iii<-x[-c(1,5,9,15,21)]
iii

#Exexrcise 2.6.

x<-rnorm(20,mean = 10,sd=20)
x
i<-x[x<1]
i
ii<-x[(x>-0.5)&(x<1)]
ii
iii<-x[abs(x)>1.5]
iii

#Exexrcise 2.7.

b<-matrix(c(1,4,-3,2,2,-1,3,6,-3),3,3)
b

x<-b%*%b%*%b
x
y<--6*diag(3)
y

#Exexrcise 2.8.

a<-rbind(c(1,3,5,7),c(2,4,6,8))
a

b<-matrix(rep(c(1,-1),each=15),15,10)
b


d<-outer(0:9,1:10,"+")
d



#Exexrcise 2.9.

x=matrix(c(a,b,c,d,e),1,5)
x
y=matrix(c(1,2,3,4,5,2,3,4,5,1,3,4,5,1,2,4,5,1,2,3,5,1,2,3,4),5,5)
y
z=matrix(c(-5,2,5,10,1),5,1)
z
solve(y)%*%z


#Exexrcise 3.1.
library(MASS)
hills[1,]
hills[3]
hills%*%c(1,2,4)
mean(hills)

#Exexrcise 3.2.

with(hills, mean((dist*(time/60)>5)&(dist*(time/60)<10)))

#Exexrcise 3.3.

#not imprtabt

#Exexrcise 3.4.

y=sample(c(1,2,3),50,replace=TRUE)
y

z=factor(y, levels=c(1,2,3),labels=c("yes","no","maybe"))
z

#Exexrcise 3.5.

head(birthwt)

race=factor(birthwt$race)

summary(birthwt$race)
summary(race)

plot(birthwt$race)  
plot(race)

mean(birthwt$race)
mean(race)


#Exexrcise 3.6.

dat<-read.table("smoking.dat",nrows =9,skip=20)
head(dat)
dat



#Exexrcise 4.1.

logit<-function(x)
     { 
       log(x/(1-x))
       
       }


#Exexrcise 4.2.

y=function(x,n)
   {
  
sum(((-1)^((1:n)+1))*(((x)^(1:n))/(1:n)))
  
  }


#Exexrcise 4.3.

z<-function(x,y){
  
  xy<-(x-mean(x))*(y-mean(y))
  xx<-(x-mean(x))^2
  
  b<-sum(xy)/sum(xx)
  a<-mean(y)-mean(x)*b
  out<-c(b,a) 
  return(out)
}
  
  
#Exexrcise 4.4.


x=matrix(2:5,2,2)
x
y=matrix(c(2,1),2,1)
y


ab<-function(a,b){
  
  m=nrow(a)
  n=ncol(a)
  p=ncol(b)
  d=matrix(0,m,p)
  
  for( i in 1:m){
    
    for( k in 1:p){
      
      d[i,k]=d[i,k]+sum(a[i,]*b[,p])
      
    }
      
    }
    d 
}
  







#Exexrcise 4.5.

h<-function(x){
  
  if(x==1) return(1)
  else return( 2*h(x-1)+1)
}

h(2)
  

#Exexrcise 4.6.

fib1<-function(n){
  
   fib1<-1
   
   for( i in 0:n){
     
   fib1<-fib1+1
   }
   fib1
}

fib(5)

system.time(fib1(30))

#Exexrcise 4.7.

#nothing happen



  
bx<-1:10
y<-21:30

z(x,y)    
out

lm(y~x)

  


set.seed(1328) # to get the same values as me
x = rnorm(100) # generate data
plot.new()
plot.window(xlim=c(-3,3), ylim=c(-0.1,0.5))
axis(side=1, pos=-0.1)
hist(x, breaks=15, add=TRUE, freq=FALSE, col=2)
plot(dnorm, -3, 3, add=TRUE)
points(x, rep(-0.05,100), pch="|")
title(main="Normal random variables")
















x <- list(temp=19.5, wind=12, wind.dir="SSW", rain=20, summ="showers")
class(x) <- "weather"
attr(x, "class")















row_sums <- function(x, cols){ 
 apply(x[,cols,drop=FALSE], 1, sum)
}

x <- matrix(1:9, 3, 3)

row_sums(x, 2:3)

row_sums(x, 2)





 g = function(y) {
if (y < 0) warning("Some warning")
 return(y)
 
}
  > h = function(z) f
+ stop("Some error message")
+ z
+ g
>
  > f = function(x) f
+











