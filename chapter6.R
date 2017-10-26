#第6章 2つの平均値を比較する

#6.2 独立な2群のt検定
toukei1men <- c(6,10,6,10,5,3,5,9,3,3)
toukei1men
toukei1wo <- c(11,6,11,9,7,5,8,7,7,9)
toukei1wo
mean(toukei1men)
mean(toukei1wo)
var(toukei1men)
var(toukei1wo)
poolvar <- sqrt(((length(toukei1men)-1)*var(toukei1men)+(length(toukei1wo)-1)*var(toukei1wo))/(length(toukei1men)+length(toukei1wo)-2))
poolvar
tbunbo <- poolvar*sqrt(1/length(toukei1men)+1/length(toukei1wo))
tbunbo
tbunsi <- mean(toukei1men)-mean(toukei1wo)
tbunsi
ttoukei <- tbunsi/tbunbo
ttoukei
qt(0.025,18)
qt(0.025,18,lower.tail=FALSE)
pt(-1.842885,18)
2*pt(-1.842885,18)
t.test(toukei1men,toukei1wo,var.equal=TRUE)

#6.3 t検定の前提条件

#6.3.1 分散の等質性の検定
classA <- c(54,55,52,48,50,38,41,40,53,52)
classB <- c(67,63,50,60,61,69,43,58,36,29)
var.test(classA,classB)

#6.3.2 Welchの検定
t.test(classA,classB,var.equal=FALSE)

#6.4 対応のあるt検定
