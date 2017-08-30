sidouhou <- c("C","B","B","A","B","C","A","D","D","D","A","A","A","C","B","D","C","C","B","D")
sidouhou
table(sidouhou)
sinrigakutest <- c(13,14,7,12,10,6,8,15,4,14,9,6,10,12,5,12,8,8,12,15)
sinrigakutest
hist(sinrigakutest)
10+13+8+15+8
sum(10,13,8,15,8)
testa <- c(10,13,8,15,8)
testa
sum(testa)
sum(testa)/5
sum(testa)/length(testa)
mean(testa)
median(testa)
table(testa)
testamean <- mean(testa)
testamean
heikinkaranohensa <- testa - testamean
heikinkaranohensa
var(testa)
sd(testa)

sinritest <- c(13,14,7,12,10,6,8,15,4,14,9,6,10,12,5,12,8,8,12,15)
sinritest
sinrimean <- mean(sinritest)
sinrimean

#練習問題

#(1)
a <- c(60,100,50,40,50,230,120,240,200,30)
b <- c(50,60,40,50,100,80,30,20,100,120)
hist(a)
hist(b)

#(2)
mean(a)
mean(b)
sd(a)
sd(b)

#(3)
az <- (a-mean(a))/sd(a)
az
bz <- (b-mean(b))/sd(b)
bz