library("Ecdat")
?CPSch3
data(CPSch3)
dimnames(CPSch3)[[2]]

male.earnings = CPSch3[CPSch3[,3] == "male", 2]
sqrt.male.earnings = sqrt(male.earnings)
log.male.earnings = log(male.earnings)

par(mfrow = c(2,2))
qqnorm(male.earnings, datax = T, main = "untransformed")
qqnorm(sqrt.male.earnings, datax = T, main = "sqrt")
qqnorm(log.male.earnings, datax = T, main = "log")

par(mfrow = c(2, 2))
boxplot(male.earnings, main = "untransformed")
boxplot(sqrt.male.earnings, main = "sqrt")
boxplot(log.male.earnings, main = "log")

par(mfrow = c(2, 2))
plot(density(male.earnings), main = "untransformed")
plot(density(sqrt.male.earnings), main = "sqrt")
plot(density(log.male.earnings), main = "log")

library("MASS")
boxcox(male.earning~1, lambda = seq(.3, .45, 1/100))

bc = boxcox(male.earning~1, lambda = seq(.3, .45, 1/100), interp = F)
ind = (bc$y == max(bc$y))
ind2 = (bc$y > max(bc$y) - qchisq(.95, df = 1)/2)
bc$x[ind]
bc$x[ind2]


library("fGarch")
fit = sstdFit(male.earnings, hessian = T)
