library(tidyverse)
library(MASS)
library(car)
library(Hotelling)
library(klaR) 

baseball <- read.csv(file = "C:\\Users\\seanc\\Downloads\\baseballHOF.csv")

yes <- baseball %>% filter(HoF == "Yes")
no <- baseball %>% filter(HoF == "No")

equalCovs(yes, no)

summary(baseball)
summary(yes)
summary(no)

hof.lda <- lda(HoF~Yrs+ WAR+ WAR7+ JAWS+ Jpos+ JAWSratio+ G+ AB+ R+ H+ HR+ RBI+ SB+ BB+ BA+ OBP+ SLG+ OPS+ OPSadj, data = baseball)
hof.lda

a <- hof.lda$scaling[,1]
model1 <- lm(cbind(Yrs, WAR, WAR7, JAWS, Jpos, JAWSratio, G, AB, R, H, HR, RBI, SB, BB, BA, OBP, SLG, OPS, OPSadj)~factor(HoF), data = baseball)
fit.manova1 <- Manova(model1)
E <- fit.manova1$SSPE
S.pl <- E/(605)
a.star <- sqrt(diag(S.pl))*a
a.star

test1 <- hotelling.test(cbind(Yrs, WAR, WAR7, JAWS, Jpos, JAWSratio, G, AB, R, H, HR, RBI, SB, BB, BA, OBP, SLG, OPS, OPSadj)~factor(HoF), data = baseball)
test1

step.hof <- greedy.wilks(HoF~Yrs+ WAR+ WAR7+ JAWS+ Jpos+ JAWSratio+ G+ AB+ R+ H+ HR+ RBI+ SB+ BB+ BA+ OBP+ SLG+ OPS+ OPSadj, data = baseball)
step.hof

hof.lda2 <- lda(step.hof$formula, data = baseball)
hof.lda2

zbar.n <- sum(hof.lda$scaling*hof.lda$means[1,])
zbar.y <- sum(hof.lda$scaling*hof.lda$means[2,])
zbar.y
zbar.n
cutoff <- (zbar.y+zbar.n)/2
cutoff
#If  >2.04 hall of fame
resub <- baseball[,-c(1, 2)]
z <- predict(hof.lda, resub)
table(baseball$HoF, z$class)

zbar.y2 <- sum(hof.lda2$scaling*hof.lda2$means[1,])
zbar.n2 <- sum(hof.lda2$scaling*hof.lda2$means[2,])
zbar.y2
zbar.n2
cutoff2 <- (zbar.y2+zbar.n2)/2
cutoff2

resub2 <- baseball[,-c(1, 2)]
z2 <- predict(hof.lda2, resub2)
table(baseball$HoF, z2$class)

hof.qda <- qda(HoF~Yrs+ WAR+ WAR7+ JAWS+ Jpos+ JAWSratio+ G+ AB+ R+ H+ HR+ RBI+ SB+ BB+ BA+ OBP+ SLG+ OPS+ OPSadj, data = baseball)
z3 <- predict(hof.qda, resub)
table(baseball$HoF, z3$class)

hof.lda2 <- lda(HoF~Yrs+ WAR+ WAR7+ JAWS+ Jpos+ JAWSratio+ G+ AB+ R+ H+ HR+ RBI+ SB+ BB+ BA+ OBP+ SLG+ OPS+ OPSadj, CV = T, data = baseball)
table(baseball$HoF, hof.lda2$class)

pred.z1 <- predict(hof.lda)$x[,1]
qplot(pred.z1, fill = HoF, data = baseball) + labs(title = "Linear Discriminant Analysis Visualization", y = "Number of observed values", x = "Calculated Value", fill = "Hall of Fame")
