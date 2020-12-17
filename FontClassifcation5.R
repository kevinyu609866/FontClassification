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

newdata <- data.frame(pr.out$x[,1:111])
PCA_anaylsis <-cbind(DATA['font'],newdata)

COMIC <- data.frame(PCA_anaylsis %>% filter(PCA_anaylsis$font == "COMIC"))
BOOK <- data.frame(PCA_anaylsis %>% filter(PCA_anaylsis$font == "BOOK"))
MISTRAL <- data.frame(PCA_anaylsis %>% filter(PCA_anaylsis$font == "MISTRAL"))

library(caTools)
set.seed(1)
comic = sample.split(COMIC,SplitRatio = 0.80)
TRA_comic <- subset(COMIC, comic == TRUE)
TST_comic <- subset(COMIC, comic == FALSE)


book = sample.split(BOOK,SplitRatio = 0.80)
TRA_book <- subset(BOOK, book == TRUE)
TST_book <- subset(BOOK, book == FALSE)


mistral = sample.split(COMIC,SplitRatio = 0.80)
TRA_mistral <- subset(MISTRAL, mistral == TRUE)
TST_mistral <- subset(MISTRAL, mistral == FALSE)

TRAIN <- bind_rows(TRA_book, TRA_comic, TRA_mistral)
TEST <- bind_rows(TST_book, TST_comic, TST_mistral)

TRAIN <- transform(TRAIN, font=as.factor(font))
TEST <- transform(TEST, font=as.factor(font))

install.packages("randomForest")
install.packages("party")
library(randomForest)
library(party)

set.seed(1)
model1 <- randomForest(font ~ ., data = TRAIN, na.action = na.omit)
plot(model1)
legend(300,.45, legend = c("Out of Bag Samples", "COMIC", "MISTRAL", "BOOK"), col = c("black", "blue", "red", "green"),  lty=1, cex=0.8,
       title="Class lines", text.font=4, bg='white')

fontpredictions <- predict(model1, newdata= TEST)
accuracy <- prop.table(table(TEST[,1],fontpredictions))
accuracy/rowSums(accuracy)

model2 <- randomForest(font ~ . , data = TRAIN, ntrees = 100, ntry = sqrt(111))
fontpredictions <- predict(model2, newdata= TEST)
confusionMatrix(TEST$font,fontpredictions)
acc <- prop.table(table(TEST[,1],fontpredictions))
acc/rowSums(acc)

accuracy(acc)

accuracy <- function(x) {sum(diag(x))/(sum(rowSums(x))) * 100}

set.seed(1)
model10 <- randomForest(font ~ . , data = TRAIN, ntrees = 10, ntry = sqrt(111))
acc10 <- accuracy(model10$confusion) 
round(model10$confusion/rowSums(model10$confusion),2)

model50 <- randomForest(font ~ . , data = TRAIN, ntrees = 50, ntry = sqrt(111))
acc50 <- accuracy(model50$confusion)
round(model50$confusion/rowSums(model50$confusion),2)

model100 <- randomForest(font ~ . , data = TRAIN, ntrees = 100, ntry = sqrt(111))
acc100 <- accuracy(model100$confusion)
round(model100$confusion/rowSums(model100$confusion),2)
model200 <- randomForest(font ~ . , data = TRAIN, ntrees = 200, ntry = sqrt(111))
acc200 <- accuracy(model200$confusion)
round(model200$confusion/rowSums(model200$confusion),2)
model300 <- randomForest(font ~ . , data = TRAIN, ntrees = 300, ntry = sqrt(111))
acc300 <- accuracy(model300$confusion)
round(model300$confusion/rowSums(model300$confusion),2)
model400 <- randomForest(font ~ . , data = TRAIN, ntrees = 400, ntry = sqrt(111))
acc400 <- accuracy(model400$confusion)
round(model400$confusion/rowSums(model400$confusion),2)
ntrees <- c(10,50,100,200,300,400)
naccuracies <- c(acc10,acc50,acc100,acc200,acc300,acc400)

plot(ntrees,naccuracies, xlab = "Number of trees", ylab = "RF performance accuracy", ylim = c(75,80))
lines(ntrees,naccuracies)

C1acc <- c()
C2acc <- c()
C3acc <- c()

model10CONF <- prop.table(model10$confusion)
model10CONF <- model10CONF/rowSums(model10CONF)

model50CONF <- prop.table(model50$confusion)
model50CONF <- model50CONF/rowSums(model50CONF)

model100CONF <- prop.table(model100$confusion)
model100CONF <- model100CONF/rowSums(model100CONF)

model200CONF <- prop.table(model200$confusion)
model200CONF <- model200CONF/rowSums(model200CONF)

model300CONF <- prop.table(model300$confusion)
model300CONF <- model300CONF/rowSums(model300CONF)

model400CONF <- prop.table(model400$confusion)
model400CONF <- model400CONF/rowSums(model400CONF)

C1acc <- c(C1acc,model10CONF[1])
C1acc <- c(C1acc,model50CONF[1])
C1acc <- c(C1acc,model100CONF[1])
C1acc <- c(C1acc,model200CONF[1])
C1acc <- c(C1acc,model300CONF[1])
C1acc <- c(C1acc,model400CONF[1])

C2acc <- c(C2acc,model10CONF[5])
C2acc <- c(C2acc,model50CONF[5])
C2acc <- c(C2acc,model100CONF[5])
C2acc <- c(C2acc,model200CONF[5])
C2acc <- c(C2acc,model300CONF[5])
C2acc <- c(C2acc,model400CONF[5])


C3acc <- c(C3acc,model10CONF[9])
C3acc <- c(C3acc,model50CONF[9])
C3acc <- c(C3acc,model100CONF[9])
C3acc <- c(C3acc,model200CONF[9])
C3acc <- c(C3acc,model300CONF[9])
C3acc <- c(C3acc,model400CONF[9])

plot(ntrees,C1acc, type = "o", col = "blue", pch = "o", xlab = "Number of trees", ylab = "RF performance accuracy", ylim = c(.75,.825))
points(ntrees, C2acc, col = "red", pch = "*")
lines(ntrees, C2acc, col = "red")

points(ntrees, C3acc, col = "green", pch = "+")
lines(ntrees, C3acc, col = "green")

legend(350,.82, legend = c("MISTRAL", "BOOK", "COMIC"), col = c( "blue","green","red"),  lty=1, cex=0.8,
       title="Class lines", text.font=4, bg='white')


bestRF <- model300
plot(bestRF$importance, Values[1:111,], xlab = "Importances", ylab = "Eigenvalue")


set.seed(2)
comic2 = sample.split(COMIC,SplitRatio = 0.80)
TRA_comic2 <- subset(COMIC, comic2 == TRUE)
TST_comic2 <- subset(COMIC, comic2 == FALSE)

book2 = sample.split(BOOK,SplitRatio = 0.80)
TRA_book2 <- subset(BOOK, book2 == TRUE)
TST_book2 <- subset(BOOK, book2 == FALSE)

mistral2 = sample.split(COMIC,SplitRatio = 0.80)
TRA_mistral2 <- subset(MISTRAL, mistral2 == TRUE)
TST_mistral2 <- subset(MISTRAL, mistral2 == FALSE)

TRAIN2 <- bind_rows(TRA_book2, TRA_comic2, TRA_mistral2)
TEST2 <- bind_rows(TST_book2, TST_comic2, TST_mistral2)

TRAIN2 <- transform(TRAIN2, font=as.factor(font))
TEST2 <- transform(TEST2, font=as.factor(font))

model_2 <- randomForest(font ~ . , data = TRAIN2, ntrees = 300, ntry = sqrt(111))
fontpredictions <- predict(model_2, newdata= TEST2)
TESTCONF2 <- confusionMatrix(TEST2$font,fontpredictions)
round(TESTCONF2$table/rowSums(TESTCONF2$table),3)

fontpredictions2 <- predict(model300, newdata =TEST)
TESTCONF1 <- confusionMatrix(TEST$font, fontpredictions2)
round(TESTCONF1$table/rowSums(TESTCONF1$table),2)

library(caret)
confusionMatrix(model_2$confusion)
acc_2 <- accuracy(model_2$confusion)

# modelTrainComic <- randomForest(font ~ ., data = TRA_comic, ntrees = 100, ntry = sqrt(111))]
TRAIN5 <- bind_rows(TRA_book, TRA_comic, TRA_mistral)
TEST5 <- bind_rows(TST_book, TST_comic, TST_mistral)

TRA_c2c3 <- TRAIN5[2:ncol(TRAIN5)]
TRA_c1c3 <- TRAIN5[2:ncol(TRAIN5)]
TRA_c1c2 <- TRAIN5[2:ncol(TRAIN5)]

for (x in 1:nrow(TRAIN5)) {
  if (TRAIN$font[x] == "MISTRAL") {
    TRA_c2c3$font[x] <- "MISTRAL"
  } 
  else {
    TRA_c2c3$font[x] <- "BOOKCOMIC"
  }
}

for (x in 1:nrow(TRAIN5)) {
  if (TRAIN$font[x] == "BOOK") {
    TRA_c1c3$font[x] <- "BOOK"
  } 
  else {
    TRA_c1c3$font[x] <- "MISTRALCOMIC"
  }
}

for (x in 1:nrow(TRAIN5)) {
  if (TRAIN$font[x] == "COMIC") {
    TRA_c1c2$font[x] <- "COMIC"
  } 
  else {
    TRA_c1c2$font[x] <- "BOOKMISTRAL"
  }
}

install.packages("DMwR")
library(DMwR)

SMOTED_TRA_c2c3 <- SMOTE(font ~ ., TRA_c2c3, perc.over = 100)
SMOTED_TRA_c1c3 <- SMOTE(font ~ ., TRA_c1c3, perc.over = 100)
SMOTED_TRA_c1c2 <- SMOTE(font ~ ., TRA_c1c2, perc.over = 100)

TRA_c2c3 <- transform(SMOTED_TRA_c2c3, font = as.factor(font))
TRA_c1c3 <- transform(SMOTED_TRA_c1c3, font = as.factor(font))
TRA_c1c2 <- transform(SMOTED_TRA_c1c2, font = as.factor(font))

modelTrain_c2c3 <- randomForest(font~ ., data= TRA_c2c3, ntrees = 300, ntry = sqrt(111))
acc_c2c3 <- accuracy(modelTrain_c2c3$confusion)

modelTrain_c1c3 <- randomForest(font~ ., data= TRA_c1c3, ntrees = 300, ntry = sqrt(111))
acc_c1c3 <- accuracy(modelTrain_c1c3$confusion)

modelTrain_c1c2 <- randomForest(font~ ., data= TRA_c1c2, ntrees = 300, ntry = sqrt(111))
acc_c1c2 <- accuracy(modelTrain_c1c2$confusion)

?SMOTE

TST_c2c3 <- TEST5[2:ncol(TEST5)]
TST_c1c3 <- TEST5[2:ncol(TEST5)]
TST_c1c2 <- TEST5[2:ncol(TEST5)]

for (x in 1:nrow(TEST5)) {
  if (TEST$font[x] == "MISTRAL") {
    TST_c2c3$font[x] <- "MISTRAL"
  } 
  else {
    TST_c2c3$font[x] <- "BOOKCOMIC"
  }
}

for (x in 1:nrow(TEST5)) {
  if (TEST$font[x] == "BOOK") {
    TST_c1c3$font[x] <- "BOOK"
  } 
  else {
    TST_c1c3$font[x] <- "MISTRALCOMIC"
  }
}

for (x in 1:nrow(TEST5)) {
  if (TEST$font[x] == "COMIC") {
    TST_c1c2$font[x] <- "COMIC"
  } 
  else {
    TST_c1c2$font[x] <- "BOOKMISTRAL"
  }
}


set.seed(1)
TST_c2c3 <- transform(TST_c2c3, font = as.factor(font))
TST_c1c3 <- transform(TST_c1c3, font = as.factor(font))
TST_c1c2 <- transform(TST_c1c2, font = as.factor(font))

C2C3 <- predict(modelTrain_c2c3, newdata= TST_c2c3)
C1C3 <- predict(modelTrain_c1c3, newdata= TST_c1c3)
C1C2 <- predict(modelTrain_c1c2, newdata= TST_c1c2)

accuracyC2C3 <- prop.table(table(TST_c2c3[,112],C2C3))
con1 <- accuracyC2C3/rowSums(accuracyC2C3)

accuracyC1C3 <- prop.table(table(TST_c1c3[,112],C1C3))
con2 <- accuracyC1C3/rowSums(accuracyC1C3)

accuracyC1C2 <- prop.table(table(TST_c1c2[,112],C1C2))
con3 <- accuracyC1C2/rowSums(accuracyC1C2)

accuracy <- function(x) {sum(diag(x))/(sum(rowSums(x))) * 100}
accuracy(con1)
accuracy(con2)
accuracy(con3)






