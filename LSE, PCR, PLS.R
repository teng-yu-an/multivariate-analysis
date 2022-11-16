
QS_Ranking <- read.table("2020-QS-Ranking.txt",sep = ",",header = T,encoding = "UTF-8")
str(QS_Ranking)
options(scipen = 999)

QS <- apply(QS_Ranking[c(3:8)],2,scale) #把資料標準化
QS <- as.data.frame(QS)

## Least Squares (LS) regression model without the intercept term ##
lsrm <- lm(Academic_Reputation~0+Employer_Reputation+Faculty_Student+Faculty_Citation+International_Faculty+International_Students, data=QS, x=T, y=T)
summary(lsrm)
install.packages("lmvar")
library(lmvar)
set.seed(1)
cv.lm(lsrm,k=10)


## The Principal Component Regression (PCR) without the intercept term ##
install.packages("pls")
library(pls)
set.seed(1)
pcr.fit <- pcr(Academic_Reputation~Employer_Reputation+Faculty_Student+Faculty_Citation+International_Faculty+International_Students, data=QS, scale=T, validation="CV")
summary(pcr.fit)
pcr.fit$coefficients #看主成份為多少時，模型的係數為何
pcr.fit$projection #看每個主成份的組成
validationplot(pcr.fit, val.type = "RMSEP", ylim = c(0.7,1.2)) #選擇CV ERROR最小的主成份(M=?)
selectNcomp(pcr.fit,method = "onesigma",plot = TRUE, ylim = c(0.7,1.2)) #畫一倍標準差



## Partial Least Squares (PLS) regression without the intercept term ##
set.seed(1)
pls.fit <- plsr(Academic_Reputation~Employer_Reputation+Faculty_Student+Faculty_Citation+International_Faculty+International_Students, data=QS, scale=T, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "RMSEP", ylim = c(0.7,1.2))
selectNcomp(pls.fit,method = "onesigma",plot = TRUE, ylim = c(0.7,1.2)) #畫一倍標準差
pls.fit$projection
pls.fit$coefficients
