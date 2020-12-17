library(dplyr)
MISTRAL <- read.csv("C:/Users/Kevin/Desktop/fall 2020/6350/HW/HW3/MISTRAL.csv")
BOOK <- read.csv("C:/Users/Kevin/Desktop/fall 2020/6350/HW/HW3/BOOK.csv")
COMIC <- read.csv("C:/Users/Kevin/Desktop/fall 2020/6350/HW/HW3/COMIC.csv")

nrow(MISTRAL)
nrow(BOOK)
nrow(COMIC)

MISTRAL <- select(MISTRAL, -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
BOOK <- select(BOOK, -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
COMIC <- select(COMIC, -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))

MISTRAL <- na.omit(MISTRAL)
BOOK <- na.omit(BOOK)
COMIC <- na.omit(COMIC)

MISTRAL <- filter(MISTRAL, strength == 0.4, italic == 0)
BOOK <- filter(BOOK, strength == 0.4, italic == 0)
COMIC <- filter(COMIC, strength == 0.4, italic == 0)

n1 <- nrow(MISTRAL)
n2 <- nrow(BOOK)
n3 <- nrow(COMIC)
Ntotal = n1 + n2 + n3
n1;n2;n3;Ntotal

DATA <- bind_rows(MISTRAL, BOOK, COMIC)
DataCol <- ncol(DATA)

SDATA <- scale(DATA[,4:ncol(DATA)])
SDATA <- cbind(DATA[,1:3],SDATA)

corrmatrix <- cor(SDATA[4:ncol(SDATA)])
corrmatrix <- round(corrmatrix,2)
head(corrmatrix)

EV <- eigen(corrmatrix)
Values <- data.frame(EV$values)
W <- data.frame(EV$vectors)
indexes <- data.frame

plot((1:400), Values$EV.values, xlab = "R value (1-400)", ylab = "Eigenvalues for given R", main = "Eigenvalues Plot")

library("devtools")
plot((1:400), Values$EV.values, type = 'l', xlab = 'R', ylab = 'EigenValues')

summary(corrmatrix)
pr.out <- prcomp(SDATA[4:ncol(SDATA)], scale = FALSE, center = TRUE)
?prcomp
summary(pr.out)

variance <- data.frame(summary(pr.out)$importance[2,])

plot((1:400), variance$summary.pr.out..importance.2..., type = 'o', xlab = 'R', ylab = 'PVE of R', main = "PVE Plot")

index = 0
sum = 0
for (i in 1:nrow(variance)) {
  if (sum < .95){
    sum = sum + variance$summary.pr.out..importance.2...[i]    
  }
  else {
    index = i - 1
    break
  }

}
sum
index

write.table(corrmatrix, "C:/Users/Kevin/Desktop/fall 2020/6350/HW/HW3/CORRMATRIX.csv",quote=FALSE, sep = ",",
            row.names=FALSE, col.names=FALSE )
write.table(Values, "C:/Users/Kevin/Desktop/fall 2020/6350/HW/HW3/VALUES.CSV",quote=FALSE, sep = ",",
            row.names=FALSE, col.names=FALSE )
write.table(W, "C:/Users/Kevin/Desktop/fall 2020/6350/HW/HW3/VECTORS.CSV",quote=FALSE, sep = ",",
            row.names=FALSE, col.names=FALSE )

newdata <- pr.out$x[,1:111]
TotalPC <- pr.out$x[,1:400]

write.table(TotalPC, "C:/Users/Kevin/Desktop/fall 2020/6350/HW/HW3/PC.csv",quote=FALSE, sep = ",",
            row.names=FALSE, col.names=FALSE )


NDATA <- cbind(DATA[1], newdata)
NMISTRAL <- data.frame(NDATA %>% filter(NDATA$font == "MISTRAL"))
NBOOK <- data.frame(NDATA %>% filter(NDATA$font == "BOOK"))
NCOMIC <- data.frame(NDATA %>% filter(NDATA$font == "COMIC"))
  
set.seed(1)
NMISTRALIndexes <- sample(1:nrow(NMISTRAL), size = 0.2 * nrow(NMISTRAL))
NMistralTest <- NMISTRAL[NMISTRALIndexes,]
NMistralTrain <- NMISTRAL[-NMISTRALIndexes,]

NBOOKIndexes <- sample(1:nrow(NBOOK), size = 0.2 * nrow(NBOOK))
NBookTest <- NBOOK[NBOOKIndexes,]
NBookTrain <- NBOOK[-NBOOKIndexes,]

NCOMICIndexes <- sample(1:nrow(NCOMIC), size = 0.2 * nrow(NCOMIC))
NComicTest <- NCOMIC[NCOMICIndexes,]
NComicTrain <- NCOMIC[-NCOMICIndexes,]

TRAINSET <- bind_rows(NMistralTrain,NBookTrain,NComicTrain)
TESTSET <- bind_rows(NMistralTest,NBookTest,NComicTest)

library(caret)
library(class)

TRAIN.LABELS <- TRAINSET[,1]
TEST.LABELS <- TESTSET[,1]

TRAINSET <- TRAINSET[-1]
TESTSET <- TESTSET[-1]

set.seed(1)
accuracy <- function(x) {sum(diag(x))/(sum(rowSums(x))) * 100}
KNN5train <- knn(train = TRAINSET, test = TESTSET, cl = TRAIN.LABELS , k = 5)
conf5train <- table(KNN5train, TEST.LABELS)
conf5train
KNN5test <- knn(train = TESTSET, test= TRAINSET, cl = TEST.LABELS , k = 5)
conf5test <- table(KNN5test, TRAIN.LABELS)
train5perf <- accuracy(conf5train)
test5perf <- accuracy(conf5test)
train5perf
test5perf

KNN2train <- knn(train = TRAINSET, test = TESTSET, cl = TRAIN.LABELS , k = 2)
conf2train <- table(KNN2train, TEST.LABELS)
conf2train
KNN2test <- knn(train = TESTSET, test= TRAINSET, cl = TEST.LABELS , k = 2)
conf2test <- table(KNN2test, TRAIN.LABELS)
train2perf <- accuracy(conf2train)
test2perf <- accuracy(conf2test)
train2perf
test2perf

KNN10train <-knn(train = TRAINSET, test = TESTSET, cl = TRAIN.LABELS, k = 10)
conf10train <- table(KNN10train, TEST.LABELS)
KNN10test <- knn(train = TESTSET, test= TRAINSET, cl = TEST.LABELS, k = 10)
conf10test <- table(KNN10test, TRAIN.LABELS)
train10perf <- accuracy(conf10train)
test10perf <- accuracy(conf10test)

KNN15train <-knn(train = TRAINSET, test = TESTSET, cl = TRAIN.LABELS, k = 15)
conf15train <- round(prop.table(table(KNN15train, TEST.LABELS)), 2)
KNN15test <- knn(train = TESTSET, test= TRAINSET, cl = TEST.LABELS, k = 15)
conf15test <- round(prop.table(table(KNN15test, TRAIN.LABELS)), 2)
train15perf <- accuracy(conf15train)
test15perf <- accuracy(conf15test)
conf15train
conf15test

KNN20train <-knn(train = TRAINSET, test = TESTSET, cl = TRAIN.LABELS, k = 20)
conf20train <- table(KNN20train, TEST.LABELS)
KNN20test <- knn(train = TESTSET, test= TRAINSET, cl = TEST.LABELS, k = 20)
conf20test <- table(KNN20test, TRAIN.LABELS)
train20perf <- accuracy(conf20train)
test20perf <- accuracy(conf20test)

KNN35train <-knn(train = TRAINSET, test = TESTSET, cl = TRAIN.LABELS, k = 35)
conf35train <- table(KNN35train, TEST.LABELS)
KNN35test <- knn(train = TESTSET, test= TRAINSET, cl = TEST.LABELS, k = 35)
conf35test <- table(KNN35test, TRAIN.LABELS)
train35perf <- accuracy(conf35train)
test35perf <- accuracy(conf35test)

KNN50train <-knn(train = TRAINSET, test = TESTSET, cl = TRAIN.LABELS, k = 50)
conf50train <- table(KNN50train, TEST.LABELS)
KNN50test <- knn(train = TESTSET, test= TRAINSET, cl = TEST.LABELS, k = 50)
conf50test <- table(KNN50test, TRAIN.LABELS)
train50perf <- accuracy(conf50train)
test50perf <- accuracy(conf50test)

par(mfrow=c(1,1))
TrainPlotPerf <- rbind(data.frame(c(2,5,10,15,20,35,50),(c(train2perf,train5perf,train10perf,train15perf,train20perf,train35perf, train50perf))))
PlotPerf <- rbind(data.frame(c(2,5,10,15,20,35,50),(c(test2perf,test5perf,test10perf,test15perf,test20perf, test35perf,test50perf))))

plot(TrainPlotPerf, type = "o", xlab = "Amount of K nearest neighbors", ylab = "Correctly Predicted Accuracy", col = "red", main = "Visualization of K neighbors and Corresponding Accuracy", ylim = c(60,76))

lines(PlotPerf, type = "o", col= "blue")
legend("topright", legend = c("Training Performance","Testing Performance"), col = c("red","blue"), pch = 1)

set.seed(1)
highesttestpref <- 0
testperf <- 0 
bestK <- 0
for (i in 1:5) {
  KNN <- knn(train = TRAINSET, test = TESTSET, cl = TRAIN.LABELS , k =i)
  conftrain <- prop.table(table(KNN, TEST.LABELS))
  testperf <- accuracy(conftrain)
  if (testperf > highesttestpref ) {
    highesttestpref <- testperf
    bestK <- i
  }
}

bestK

NMISTRAL <- data.frame(NDATA %>% filter(NDATA$font == "MISTRAL"))
NBOOK <- data.frame(NDATA %>% filter(NDATA$font == "BOOK"))
NCOMIC <- data.frame(NDATA %>% filter(NDATA$font == "COMIC"))

library(reshape2)
library(ggplot2)
Y1Y2 <- qplot(x = NDATA$PC1, y = NDATA$PC2, data= NDATA, colour = factor(NDATA$font), xlab = "PC1", ylab = "PC2", xlim = c(-30,30)) + labs(colour = 'Font')
Y1Y2
Y1Y3 <- qplot(x = NDATA$PC1, y = NDATA$PC3, data= NDATA, colour = factor(NDATA$font), xlab = "PC1", ylab = "PC3", xlim = c(-30,30) ) + labs(colour = 'Font')
Y1Y3
Y1Y4 <- qplot(x = NDATA$PC1, y = NDATA$PC4, data= NDATA, colour = factor(NDATA$font), xlab = "PC1", ylab = "PC4", xlim = c(-30,30) ) + labs(colour = 'Font')
Y1Y4
Y2Y3 <- qplot(x = NDATA$PC2, y = NDATA$PC3, data= NDATA, colour = factor(NDATA$font), xlab = "PC2", ylab = "PC3", xlim = c(-30,30) ) + labs(colour = 'Font')
Y2Y3
Y2Y4 <- qplot(x = NDATA$PC2, y = NDATA$PC4, data= NDATA, colour = factor(NDATA$font), xlab = "PC2", ylab = "PC4", xlim = c(-30,30) ) + labs(colour = 'Font')
Y2Y4
Y3Y4 <- qplot(x = NDATA$PC3, y = NDATA$PC4, data= NDATA, colour = factor(NDATA$font), xlab = "PC3", ylab = "PC4", xlim = c(-30,30) ) + labs(colour = 'Font')
Y3Y4

xd <- data.frame(c(2,5,10,15,20,35,50),c(test2perf,test5perf,test10perf,test15perf,test20perf, test35perf,test50perf), c(train2perf,train5perf,train10perf,train15perf,train20perf,train35perf, train50perf))
names(xd) <- c("K value","Test Performance", "Train Performance")