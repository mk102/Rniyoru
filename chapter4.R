#第4章 母集団と標本

#4.3 点推定
height <- c(165.2,175.9,161.7,174.2,172.1,163.3,170.9,170.6,168.4,171.3)
mean(height)
var(height)

#4.4.3 確率分布
dice6 <- ceiling(runif(n=6,min=0,max=6))
table(dice6)
hist(dice6)
dice6million <- ceiling(runif(n=6000000,min=0,max=6))
table(dice6million)
hist(dice6million)

#4.4.4 母集団分布
barplot(c(2/3,1/3),names.arg=c("男性","女性"))

#4.4.5 正規分布
curve(dnorm(x,mean=0,sd=1),from=-4,to=4)
curve(dnorm(x,mean=1,sd=1),add=TRUE)
curve(dnorm(x,mean=0,sd=2),add=TRUE)

#4.4.7 正規母集団から単純無作為抽出を行う
rnorm(n=5,mean=50,sd=10)
sample <- rnorm(n=5,mean=50,sd=10)
hist(sample)
bigsample <- rnorm(n=10000,mean=50,sd=10)
hist(bigsample)

#4.5.3 正規母集団の母平均の推定
sample <- rnorm(n=10,mean=50,sd=10)
sample
mean(sample)

#4.5.4  標本平均を求める
sampave <- numeric(length=10000)
for(i in 1:10000){
  sample <- rnorm(n=10,mean=50,sd=10)
  sampave[i] <- mean(sample)
}
hist(sampave)
errunder5 <- ifelse(abs(sampave-50)<=5,1,0)
table(errunder5)
hist(sampave,freq=FALSE)
curve(dnorm(x,mean=50,sd=sqrt(10)),add=TRUE)

#4.5.6 標準誤差
sampave <- numeric(length=10000)
for(i in 1:10000){
  sample <- rnorm(n=100,mean=50,sd=10)
  sampave[i] <- mean(sample)
}
var(sampave)
hist(sampave)

#4.6.1 標本分散と不偏分散の標本分布
sampval <- numeric(length=10000)
unval <- numeric(length=10000)
for(i in 1:10000){
  sample <- rnorm(n=100,mean=50,sd=10)
  sampval[i] <- mean((sample-mean(sample))^2)
  unval[i] <- var(sample)
}
mean(sampval)
mean(unval)
sd(sampval)
sd(unval)
hist(sampval,breaks=seq(0,500,10))
hist(unval,breaks=seq(0,500,10))

#練習問題

#(1)
sampave=numeric(length=5000)
for(i in 1:5000){
  sample <- rnorm(n=20,mean=50,sd=10)
  sampave[i] <- mean(sample)
}
hist(sampave,freq=FALSE)
curve(dnorm(x,mean=50,sd=sqrt(100/20)),add=TRUE)

#(2)
curve(dnorm(x,mean=0,sd=sqrt(1/25)))
a <- c(1,4,9,16)
for(i in a){
  curve(dnorm(x,mean=0,sd=sqrt(1/i)),add=TRUE)
}
