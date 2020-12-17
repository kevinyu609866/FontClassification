library(dplyr)
set.seed(1)
CL1 <- read.table("C:\\Users\\Kevin\\Desktop\\fall 2020\\6350\\HW\\HW2\\fonts\\PMINGLIU-EXTB.csv", sep = ',', header = T)
CL2 <- read.table("C:\\Users\\Kevin\\Desktop\\fall 2020\\6350\\HW\\HW2\\fonts\\VIVALDI.csv", sep = ',', header = T)
CL3 <- read.table("C:\\Users\\Kevin\\Desktop\\fall 2020\\6350\\HW\\HW2\\fonts\\PAPYRUS.csv", sep = ',', header = T)

CL1 <- select(CL1, -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
CL2 <- select(CL2, -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
CL3 <- select(CL3, -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))

CL1 <- na.omit(CL1)
CL2<- na.omit(CL2)
CL3 <- na.omit(CL3)

CL1 <- filter(CL1, strength == 0.4, italic == 0)
CL2 <- filter(CL2, strength == 0.4, italic == 0)
CL3 <- filter(CL3, strength == 0.4, italic == 0)

n1 <- NROW(CL1)
n2 <- NROW(CL2)
n3 <- NROW(CL3)
N = n1 + n2 + n3

DATA <- bind_rows(CL1, CL2, CL3)
DataCol <- ncol(DATA)

index = 1
DataColMeans = 0
DataColStdDev = 0
for (i in 4:DataCol) {
  DataColMeans[index] <- data.frame(colMeans(DATA[i]))
  DataColStdDev[index] <- data.frame(sd(DATA[,i]))
  index = index + 1
}

DataColMeans <- data.frame(DataColMeans)
DataColStdDev <- data.frame(DataColStdDev)
install.packages('matrixStats')

SDATA <- matrix(1:273883,N,401)
names(SDATA)[401] <- "CLASS"
names(SDATA)[400]
SDATA <- data.frame(SDATA)

install.packages('standardize')
library(standardize)

calcstd <- function (value,mean,sd) {
  val <- (value - mean)/sd
  
  return (val)
}

for (j in 4:DataCol) {
  
  for (i in 1:N) {
    SDATA[i,j-3] <- calcstd(DATA[i,j], DataColMeans[j-3], DataColStdDev[j-3])
    SDATA[i,401] <- DATA[i,1]
  }
}

SETROW1 <- SDATA[1:206,]
SETROW2 <- SDATA[207:444,]
SETROW3 <- SDATA[445:683,]

corrmatrix <- cor(SDATA)
round(corrmatrix,2)
options(max.print = 275000)

top <- which(((abs(corrmatrix) >= 0.8933 & abs(corrmatrix) < 1)), arr.ind = TRUE)

top10pairs <- data.frame(corrmatrix[top])
top10pairs



# compare <- function(value, storetop10,i,j) {
#   storetop10 <- data.frame(storetop10)
#   storetop10 <- storetop10[order(storetop10),]
#   
#   if (value > storetop10[1]) {
#     storetop10[1] <- value
#   }
#   return (storetop10)
# }
# 
# for (i in 1:nrow(corrmatrix)) {
#   for (j in 1: ncol(corrmatrix)) {
#     if (corrmatrix[i,j] < 1) {
#       storetop10 <- compare(abs(corrmatrix[i,j]), storetop10,i,j)
#     }
#     else 1
#   }
# }
# storetop10
# 
# which(((abs(corrmatrix) >= 0.8933 & abs(corrmatrix) < 1)), arr.ind = TRUE)

require(caTools)
indexes = sample(1:nrow(SETROW1), size = 0.2*nrow(SETROW1))
testCL1 = SETROW1[indexes,]
trainCL1 = SETROW1[-indexes,]

indexes = sample(1:nrow(SETROW2), size = 0.2*nrow(SETROW2))
testCL2 = SETROW2[indexes,]
trainCL2 = SETROW2[-indexes,]

indexes = sample(1:nrow(SETROW3), size = 0.2*nrow(SETROW3))
testCL3 = SETROW3[indexes,]
trainCL3 = SETROW3[-indexes,]

library(caret)
library(class)


TRAINSET = bind_rows(trainCL1,trainCL2,trainCL3)
train.labels = TRAINSET$X401
TRAINSET$X401 = NULL

TESTSET = bind_rows(testCL1,testCL2,testCL3)
test.labels = TESTSET$X401
TESTSET$X401 = NULL

accuracy <- function(x) {sum(diag(x))/(sum(rowSums(x))) * 100}
KNN.12train <- knn(train = TRAINSET, test = TESTSET, cl = train.labels, k =12)
conf12train <- prop.table(table(KNN.12train, test.labels))
KNN.12test <- knn(train = TESTSET, test = TRAINSET, cl = test.labels, k =12)
conf12test <- prop.table(table(KNN.12test, train.labels))
conf12train
conf12test
train12perf <- accuracy(conf12train)
test12pref <- accuracy(conf12test)
train12perf
test12pref

KNN.5train <- knn(train = TRAINSET, test = TESTSET, cl = train.labels, k =5)
conf5train <- prop.table(table(KNN.5train, test.labels))
KNN.5test <- knn(train = TESTSET, test = TRAINSET, cl = test.labels, k =5)
conf5test <- prop.table(table(KNN.5test, train.labels))
train5pref <- accuracy(conf5train)
test5pref <- accuracy(conf5test)
train5pref
test5pref

KNN.10train <- knn(train = TRAINSET, test = TESTSET, cl = train.labels, k =10)
conf10train <- prop.table(table(KNN.10train, test.labels))
KNN.10test <- knn(train = TESTSET, test = TRAINSET, cl = test.labels, k =10)
conf10test <- prop.table(table(KNN.10test, train.labels))
train10pref <- accuracy(conf10train)
test10pref <- accuracy(conf10test)
train10pref
test10pref

KNN.15train <- knn(train = TRAINSET, test = TESTSET, cl = train.labels, k =15)
conf15train <- prop.table(table(KNN.15train, test.labels))
KNN.15test <- knn(train = TESTSET, test = TRAINSET, cl = test.labels, k =15)
conf15test <- prop.table(table(KNN.15test, train.labels))
train15pref <- accuracy(conf15train)
test15perf <- accuracy(conf15test)
train15pref
test15perf

KNN.20train <- knn(train = TRAINSET, test = TESTSET, cl = train.labels, k =20)
conf20train <- prop.table(table(KNN.20train, test.labels))
KNN.20test <- knn(train = TESTSET, test = TRAINSET, cl = test.labels, k =20)
conf20test <- prop.table(table(KNN.20test, train.labels))
train20perf <- accuracy(conf20train)
test20perf <- accuracy(conf20test)
train20perf
test20perf

KNN.30train <- knn(train = TRAINSET, test = TESTSET, cl = train.labels, k =30)
conf30train <- prop.table(table(KNN.30train, test.labels))
KNN.30test <- knn(train = TESTSET, test = TRAINSET, cl = test.labels, k =30)
conf30test <- prop.table(table(KNN.30test, train.labels))
train30perf <- accuracy(conf30train)
test30perf <- accuracy(conf30test)
train30perf
test30perf

KNN.40train <- knn(train = TRAINSET, test = TESTSET, cl = train.labels, k =40)
conf40train <-prop.table(table(KNN.40train, test.labels))
KNN.40test <- knn(train = TESTSET, test = TRAINSET, cl = test.labels, k =40)
conf40test <- prop.table(table(KNN.40test, train.labels))
train40perf <- accuracy(conf40train)
test40perf <- accuracy(conf40test)
train40perf
test40perf

KNN.50train <- knn(train = TRAINSET, test = TESTSET, cl = train.labels, k =50)
conf50train <- prop.table(table(KNN.50train, test.labels))
KNN.50test <- knn(train = TESTSET, test = TRAINSET, cl = test.labels, k =50)
conf50test <- prop.table(table(KNN.50test, train.labels))
train50perf <- accuracy(conf50train)
test50perf <- accuracy(conf50test)
train50perf
test50perf

KNN.100train <- knn(train = TRAINSET, test = TESTSET, cl = train.labels, k =100)
conf100train <- prop.table(table(KNN.100train, test.labels))
KNN.100test <- knn(train = TESTSET, test = TRAINSET, cl = test.labels, k =100)
conf100test <- prop.table(table(KNN.100test, train.labels))
train100perf <- accuracy(conf100train)
test100perf <- accuracy(conf100test)
train100perf
test100perf

plot(x,y1,type="l",col="red")
lines(x,y2,col="green")

TrainPlotPerf <- rbind(data.frame(c(5,10,12,15,20,30,40,50,100),(c(train5pref,train10pref,train12perf,train15pref,train20perf,train30perf,train40perf,train50perf,train100perf))))
PlotPerf <- rbind(data.frame(c(5,10,12,15,20,30,40,50,100),(c(test5pref,test10pref,test12pref,test15perf,test20perf,test30perf,test40perf,test50perf,test100perf))))
names(PlotPerf) <- c("K", "Performance")
names(TrainPlotPerf) <- c("K", "Performance")
plot(PlotPerf$K, PlotPerf$Performance, xlab = "K", ylab = "Performance in Percentage", asp = .99, main = "KNN Accuracy at K value", type = "o")
lines(TrainPlotPerf$K, TrainPlotPerf$Performance, col = "green", type = "o")


set.seed(1)
highesttestpref <- 0
testperf <- 0 
bestK <- 0
for (i in 1:5) {
  KNN <- knn(train = TRAINSET, test = TESTSET, cl = train.labels , k =i)
  conftrain <- prop.table(table(KNN, test.labels))
  testperf <- accuracy(conftrain)
  if (testperf > highesttestpref ) {
    highesttestpref <- testperf
    bestK <- i
  }
}

bestK

KNN.1train <- knn(train = TRAINSET, test = TESTSET, cl = train.labels, k =3)
trainconf <- prop.table(table(KNN.1train, test.labels))
KNN.1test <- knn(train = TESTSET, test = TRAINSET, cl = test.labels, k =3)
testconf <- prop.table(table(KNN.1test, train.labels))
trainconf
testconf
train3perf <- accuracy(trainconf)
test3perf <- accuracy(testconf)

N1 = nrow(TRAINSET)
N2 = nrow(TESTSET)

diag(testconf.kbest)
CI = function(p, CL, n) {
  p + c(-1, 1) * qnorm((1 + CL)/2) * sqrt(p * (1 - p)/n)
}
# assume the confidence level is 90%
CI(diag(testconf)[1], 0.90, nrow(testCL1))
CI(diag(testconf)[2], 0.90, nrow(testCL2))
CI(diag(testconf)[3], 0.90, nrow(testCL3))

mat = matrix(c(55,34,56,255), ncol=2, byrow=TRUE)
mat  # output omitted
confusionMatrix(as.table(mat), positive="B")

install.packages('plyr')
library(plyr)
install.packages('gtools')
library(gtools)
install.packages('tidyverse')
library(tidyverse)

pack1 <- DATA %>% select(starts_with(c("r0c","r1c","r2c","r3c","r4c","r5c","r6c","r7c","r8c","r9c"))& ends_with(c("c0","c1","c2","c3","c4","c5","c6","c7","c8","c9")))
pack1 <- cbind(pack1, font = DATA$font)

set.seed(1)
PACK1 <- bind_rows(pack1_test,pack1_train)
pack1_test <- TESTSET[c(1:10,21:30,41:50,61:70,81:90,101:110,121:130,141:150,161:170,181:190)] 
pack1_train <- TRAINSET[c(1:10,21:30,41:50,61:70,81:90,101:110,121:130,141:150,161:170,181:190)] 
pack1train.knn <- knn(train = pack1_train, test = pack1_test, cl = train.labels, k =3 )
pack1traintable <- prop.table(table(pack1train.knn, test.labels))
w1 = accuracy(pack1traintable)
w1

set.seed(1)
PACK2 <- bind_rows(pack2_test,pack2_train)
pack2_test <- TESTSET[c(11:20,31:40,51:60,71:80,91:100,111:120,131:140,151:160,171:180,191:200)]
pack2_train <- TRAINSET[c(11:20,31:40,51:60,71:80,91:100,111:120,131:140,151:160,171:180,191:200)]
pack2train.knn <- knn(train = pack2_train, test = pack2_test, cl = train.labels, k =3 )
pack2traintable <- prop.table(table(pack2train.knn, test.labels))
w2 = accuracy(pack2traintable)
w2

set.seed(1)
PACK3 <- bind_rows(PACK3_test,PACK3_train)
PACK3_test <- TESTSET[c(211:220,231:240,251:260,271,280,291:300,311:320,331:340,351:360,371:380,391:400)]
PACK3_train <- TRAINSET[c(211:220,231:240,251:260,271,280,291:300,311:320,331:340,351:360,371:380,391:400)]
pack3train.knn <- knn(train = PACK3_train, test = PACK3_test, cl = train.labels, k =3 )
pack3traintable <- prop.table(table(pack3train.knn, test.labels))
w3 = accuracy(pack4traintable)
w3

set.seed(1)
PACK4 <- bind_rows(pack4_test,pack4_train)
pack4_train <- TRAINSET[c(201:210,221:230,241:250,261:270,281:290,301:310,321:330,341:350,361:370,381:390)]
pack4_test <- TESTSET[c(201:210,221:230,241:250,261:270,281:290,301:310,321:330,341:350,361:370,381:390)]
pack4train.knn <- knn(train = pack4_train, test = pack4_test, cl = train.labels, k =3 )
pack4traintable <- prop.table(table(pack4train.knn, test.labels))
w4 = accuracy(pack4traintable)
w4


sum(PACK1)
PACK1
install.packages('LICORS')
library(LICORS)
weights <- c(w1,w2,w3,w4)
weights <- normalize(weights)
w1 = weights[1]; w2 = weights[2]; w3 = weights[3]; w4 = weights[4]

sum(weights)
PACK1_test_w <- cbind(PACK1_test[0:100]*(w1),PACK1_test[101])
PACK1_train_w <- cbind(PACK1_train[0:100]*(w1),PACK1_train[101])

PACK2_test_w <- cbind(PACK2_test[0:100]*(w2),PACK2_test[101])
PACK2_train_w <- cbind(PACK2_train[0:100]*(w2),PACK2_train[101])

PACK3_test_w <- cbind(PACK3_test[0:100]*(w3),PACK3_test[101])
PACK3_train_w <- cbind(PACK3_train[0:100]*(w3),PACK3_train[101])

PACK4_test_w <- cbind(pack4_test[0:100]*(w4),pack4_test[101])
PACK4_train_w <- cbind(pack4_train[0:100]*(w4),pack4_train[101])
