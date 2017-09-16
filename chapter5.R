#第5章 統計的仮説検定

#5.3 標準正規分布を用いた検定
sinri <- c(13,14,7,12,10,6,8,15,4,14,9,6,10,12,5,12,8,8,12,15)
sinri
zbunsi <- mean(sinri) - 12
zbunsi
zbunbo <- sqrt(10/length(sinri))
zbunbo
ztoukei <- zbunsi/zbunbo
ztoukei
qnorm(0.025)
qnorm(0.975)
qnorm(0.025,lower.tail=FALSE)
pnorm(-2.828427)
pnorm(2.828427,lower.tail=FALSE)
2*pnorm(2.828427,lower.tail=FALSE)

#5.4 t分布を用いた検定
tbunsi <- mean(sinri) - 12
tbunsi
tbunbo <- sqrt(var(sinri)/length(sinri))
tbunbo
ttoukei <- tbunsi/tbunbo
ttoukei
qt(0.025,19)
qt(0.975,19)
qt(0.025,19,lower.tail=FALSE)
pt(-2.616648,19)
pt(2.616648,19,lower.tail=FALSE)
2*pt(2.616648,19,lower.tail=FALSE)
t.test(sinri,mu=12)

#5.5 相関係数の検定
statest1 <- c(6,10,6,10,5,3,5,9,3,3,11,6,11,9,7,5,8,7,7,9)
statest1
statest2 <- c(10,13,8,15,8,6,9,10,7,3,18,14,18,11,12,5,7,12,7,7)
statest2
hyocor <- cor(statest1,statest2)
hyocor
samplesize <- length(statest1)
samplesize
tbunsi <- hyocor*sqrt(samplesize-2)
tbunsi
tbunbo <- sqrt(1-hyocor^2)
tbunbo
ttoukei <- tbunsi/tbunbo
ttoukei
qt(0.025,18)
qt(0.975,18)
qt(0.025,18,lower.tail=FALSE)
pt(4.805707,18,lower.tail=FALSE)
2*pt(4.805707,18,lower.tail=FALSE)
cor.test(statest1,statest2)

#5.6 独立性の検定
kitai11 <- 12*14/20
kitai21 <- 12*6/20
kitai12 <- 8*14/20
kitai22 <- 8*6/20
kitaido <- c(kitai11,kitai21,kitai12,kitai22)
kitaido
kansokudo <- c(10,2,4,4)
kansokudo
kai2 <- (kansokudo-kitaido)^2/kitaido
kai2
kai2zyo <- sum(kai2)
kai2zyo
qchisq(0.95,1)
qchisq(0.05,1,lower.tail=FALSE)
pchisq(2.539683,1,lower.tail=FALSE)
1-pchisq(2.539683,1)

#5.7 サンプルサイズの検定結果への影響について
qchisq(0.05,1,lower.tail=FALSE)
risyuA <- matrix(c(16,12,4,8),2,2)
risyuA
rownames(risyuA) <- c("文系","理系")
colnames(risyuA) <- c("履修した","履修しない")
risyuA
chisq.test(risyuA,correct=FALSE)
risyuB <- matrix(c(160,120,40,80),2,2)
rownames(risyuB) <- c("文系","理系")
colnames(risyuB) <- c("履修した","履修しない")
risyuB
chisq.test(risyuB,correct=FALSE)

#練習問題

#(1)
sintyou <- c(165,150,170,168,159,170,167,178,155,159,161,162,166,171,155,160,168,172,155,167)
sintyou
t.test(sintyou,mu=170)

#(2)
studytime <- c(1,3,10,12,6,3,8,4,1,5)
ten <- c(20,40,100,80,50,50,70,50,10,60)
cor.test(studytime,ten)

#(3)
cor.test(studytime,ten,method="spearman")
cor.test(studytime,ten,mathod="kendall")

#(4)
youwa <- c("洋食","和食","和食","洋食","和食","洋食","洋食","和食","洋食","洋食","和食","洋食","和食","洋食","和食","和食","洋食","洋食","和食","和食")
amakara <- c("甘党","辛党","甘党","甘党","辛党","辛党","辛党","辛党","甘党","甘党","甘党","甘党","辛党","辛党","甘党","辛党","辛党","甘党","辛党","辛党")
a <- table(youwa,amakara)
chisq.test(a,correct=FALSE)

#(5)

#(5-1)
kokugo <- c(60,40,30,70,55)
suugaku <- c(80,25,35,70,50)
cor.test(kokugo,suugaku)

#(5-2)
kokugo2 <- rep(kokugo,2)
suugaku2 <- rep(suugaku,2)
cor.test(kokugo2,suugaku2)
