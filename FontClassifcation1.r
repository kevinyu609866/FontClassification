Auto <- read.table("C:\\Users\\Kevin\\Desktop\\fall 2020\\6350\\HW\\HW1\\cleanDataAuto.csv", sep = ',', header = T)
#1) Compute the mean(mF) and standard deviation(stdF) of each feature F 
# to calcualte the means and standard deviation use the mean and sd function
clean_auto <- Auto %>% select(cylinders, displacement, horsepower, weight,acceleration)
mF <- clean_auto %>% summarise_if(is.numeric, mean)
stdF <- clean_auto %>% summarise_if(is.numeric, sd)
#```

#2) display the histogram histF of each feature F, and the histogram hist(mpg) #to plot a histogram
#```{r}
attach(clean_auto)
par(mfrow = c(1,2))
hist(Auto$cylinders, xlab = "Cylinders")
plot(Auto$cylinders,dnorm(Auto$cylinders,mean(Auto$cylinders),sd(Auto$cylinders)),xlab = "Cylinders", ylab ="Density", main = "Cylinders PDF")
hist(Auto$displacement, xlab = "Displacement")
plot(Auto$displacement,dnorm(Auto$displacement,mean(Auto$displacement),sd(Auto$displacement)),xlab = "Displacement", ylab = "Density", main = "Displacement PDF")
hist(Auto$weight, xlab = "Weight",)
plot(Auto$weight,dnorm(Auto$weight,mean(Auto$weight),sd(Auto$weight)),xlab = "Weight", ylab = "Density", main = "Weight PDF")    
hist(Auto$horsepower, xlab = "Horsepower")
plot(Auto$horsepower,dnorm(Auto$horsepower,mean(Auto$horsepower),sd(Auto$horsepower)),xlab = "Horsepower", ylab = "Density", main = "Horsepower PDF")
hist(Auto$acceleration, xlab = "Accelearation")
plot(Auto$acceleration,dnorm(Auto$acceleration,mean(Auto$acceleration),sd(Auto$acceleration)),xlab = "Accelearion", ylab = "Density", main = "Acceleration PDF")



#```

#Compare visually each histF to the probability density function (pdf) of a normal density function with
#the same mean and standard deviation as F 
#```{r}
#```

#3) display the 5 scatterplots (cyl , mpg) , (dis , mpg) , (hor , mpg) , (wei , mpg) , (acc , mpg) 
#```{r}
# to create a scattered plot 
attach(clean_auto)
par(mfrow=c(2,3))
plot(Auto$cylinders,Auto$mpg)
plot(Auto$displacement,Auto$mpg)
plot(Auto$horsepower,Auto$mpg)
plot(Auto$weight,Auto$mpg)
plot(Auto$acceleration,Auto$mpg)
#```

#4) interpret these scatterplots to guess which features may have stronger capacity to predict mpg 

# The scatterplots could be said to have an inverse relationship, meaning the more of each observation there is example the more horsepower then the less mpg the car has. With that being said the acceleration does not really have a strongly corelated relationship. The best features that have a strong predictive capacity will be displacement acceleration and weight.

#5) compute the correlations cor(cyl , mpg) , cor(dis , mpg) , cor(hor , mpg), cor(wei , mpg), cor(acc , mpg) and interpret these correlations to guess which features may have stronger capacity to predict mpg 
#``` {r}
attach(Auto)
par(mfrow=c(2,3))
cor(cylinders,mpg)
cor(displacement,mpg)
cor(horsepower,mpg)
cor(weight,mpg)
cor(acceleration,mpg)
#```

#6) compute the 5x5 correlation matrix CORR of the 5 features; interpretation 
#``` {r}
obs <- Auto [, c (2,3,4,5,6)]


# To Compute correlation matrix
cmatrix <- cor(obs,obs)
# Use corrplot() function: Draw a correlogram
# Install corrplot
library(corrplot)
# Use corrplot() to create a correlogram:
corrplot(cmatrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
# The function corrplot() takes the correlation matrix as the first argument
# The second argument is to define the method
# The Third argument (type="upper") is used to display only the upper triangular # of the correlation matrix
# The correlation matrix is reordered according to the correlation coefficient using "hclust" method.
#  tl.col (for text label color)
#tl.srt (for text label string rotation)
#```

#7) Compute the quantiles Q1% Q2% ... Q100% of the target variable mpg to create a quantile plot
#```{r}
quantile(mpg)
x <- c(0, .25, .50, .75, 1)
y <- c(9.00, 17.00, 22.75, 29.00, 46.60)
q_mpg <- plot(x,y, type = "l")
#```


#8 ) extract from the data set two disjoint tables of cases, the LOWmpg table gathers all the cases for which mpg <= quantile Q33% the HIGHmpg table will include all the cases for which mpg > quantile Q66%
#  ```{r}
`LOWmpg <- data.frame(auto %>% filter(auto$mpg <= 18.503))
HIGHmpg <- data.frame(auto %>% filter(auto$mpg > 26.6))
#```


#9) Let F be any one of the 5 features Display side by side
#```{r}
#for LOWmpg
i <- which(LOWmpg %in% Auto$mpg)
LF1 <- Auto$cylinders[i]
LF2 <- Auto$displacement[i]
LF3 <- Auto$horsepower[i]
LF4 <-Auto$weight[i]
LF5 <- Auto$acceleration[i]
hist(LF1)
hist(LF2)
hist(LF3)
hist(LF4)
hist(LF5)

#for HIGHmpg
j <- which(HIGHmpg %in% Auto$mpg)
HF1 <- Auto$cylinders[j]
HF2 <- Auto$displacement[j]
HF3 <- Auto$horsepower[j]
HF4 <-Auto$weight[j]
HF5 <- Auto$acceleration[j]
hist(HF1)
hist(HF2)
hist(HF3)
hist(HF4)
hist(HF5)


#```


#10) interpret each one of these 5 pairs of histograms to guess which features may have a good capacity to discriminate between high mpg and low mpg 

# The five pair of histograms do not seem to have a good capacity to discrimante between high and low mpg, it seems to still have the same overall shape for both high and low.

#11) for each feature F, compute the mean mL and standard dev. stdL of the F values associated to all cases with LOWmpg; compute the mean mH and standard dev. stdH of the F values associated to all cases with HIGHmpg 

#``` {r}
LOWmpg_df<- data.frame(LOWmpg, LF1, LF2, LF3, LF4, LF5) 
Lmeans <- LOWmpg_df %>% summarise_if(is.numeric, mean)
Lstdev <- LOWmpg_df %>% summarise_if(is.numeric, sd)

HIGHmpg_df<- data.frame(HIGHmpg, HF1, HF2, HF3, HF4, HF5)
Hmeans <- HIGHmpg_df %>% summarise_if(is.numeric, mean)
Hstdev <- HIGHmpg_df %>% summarise_if(is.numeric, sd)
#```

#12) for each feature F compute s(F) = square root [ (stdL^2 + stdH^2 )/N ] compute then the ratio discr(F) = | mH - mL(F) | / s(F) which is a very rough evaluation for the "discriminating power" of feature F to distinguish between low mpg and high mpg ;compare the discriminating powers of the five feature 
#```{r}
sF1 <- sqrt( (1.8385^2) + (1.819975^2)/130)
sF2 <- sqrt( (122.4006^2) + (120.879^2)/130)
sF3 <- sqrt( (47.08524^2) + (46.66992^2)/130)
sF4 <- sqrt( (964.2385^2) + (953.8044^2)/130)
sF5 <- sqrt( (3.011865^2) + (3.001907^2)/130)

discrF1 <- abs(6.104478-6.123077)/sF1
discrF2 <- abs(6.104478-6.123077)/sF2
discrF3 <- abs(6.104478-6.123077)/sF3
discrF4 <- abs(6.104478-6.123077)/sF4
discrF5 <- abs(6.104478-6.123077)/sF5
#```

#13) For each feature F , we compute a threshold thrF = (mL stdH+ mH stdL)/( stdH +stdL) which we use to compute for each case #n a scoreF(n) based on the value F(n),
#```{r}
trF1 <- (6.123077	* 1.819975 + 6.104478 * 1.8385) /  (1.819975 + 1.8385)
trF2 <- (240.5962	* 120.879 + 239.9291 * 122.4006) /  (120.879 + 122.4006)
trF3 <- (125.1385	* 46.66992 + 124.3134 * 47.08524) /  (46.66992 + 47.08524)
trF4 <- (3251	* 953.8044 + 3255.209 * 964.238) /  (953.8044 + 964.238)
trF5 <- (14.48077	* 3.001907 + 14.5597 * 3.011865) /  (3.001907 + 3.011865)

#Score assigning functions for each feature 
comparecyl <- function(fnvalue, threshold) {
  if (Hmeans$HF1 > Lmeans$LF1) {
     if (fnvalue > threshold) {
       scorefn <- 1
     }  
    else {
       scorefn <- -1
     } 
  } else { 
    if (fnvalue < threshold) {
      scorefn <- 1
    } else {
      scorefn <- -1
    }
  }
  return(scorefn)
}

comparedis <- function(fnvalue, threshold) {
  if (Hmeans$HF2 > Lmeans$LF2) {
    if (fnvalue > threshold) {
      scorefn <- 1
    }  else {
      scorefn <- -1
    } 
  } else { 
    if (fnvalue < threshold) {
      scorefn <- 1
    } else {
      scorefn <- -1
    }
  }
  return(scorefn)
}

comparehor <- function(fnvalue, threshold) {
  if (Hmeans$HF3 > Lmeans$LF3) {
    if (fnvalue > threshold) {
      scorefn <- 1
    }  else {
      scorefn <- -1
    } 
  } else { 
    if (fnvalue < threshold) {
      scorefn <- 1
    } else {
      scorefn <- -1
    }
  }
  return(scorefn)
}

comparewei <- function(fnvalue, threshold) {
  if (Hmeans$HF4 > Lmeans$LF4) {
    if (fnvalue > threshold) {
      scorefn <- 1
    }  else {
      scorefn <- -1
    } 
  } else { 
    if (fnvalue < threshold) {
      scorefn <- 1
    } else {
      scorefn <- -1
    }
  }
  return(scorefn)
}

compareacc <- function(fnvalue, threshold) {
  if (Hmeans$HF5 > Lmeans$LF5) {
    if (fnvalue > threshold) {
      scorefn <- 1
    }  else {
      scorefn <- -1
    } 
  } else { 
    if (fnvalue < threshold) {
      scorefn <- 1
    } else {
      scorefn <- -1
    }
  }
  return(scorefn)
}

fullscore <- 0
A <- 1

quantile(Auto$mpg, .50)

for (i in 1:nrowsdf) {
  if (Auto$mpg[i] < 22.75) {
    Auto$trueclass[i] <- "LOW MPG" 
  } else if (Auto$mpg[i] > 22.75){
    Auto$trueclass[i] <- "HIGH MPG"
  }
}

#Assign prediction class for training set
train <- union(LOWmpg, HIGHmpg)

trainrows <- nrow(train)
for(i in 1:trainrows) { 
  fullscore <- 0
  cylscorefn <- comparecyl(train$cylinders[i], trF1)
  disscorefn <- comparedis(train$displacement[i], trF2)
  horscorefn <- comparehor(train$horsepower[i], trF3)
  weiscorefn <- comparewei(train$weight[i], trF4)
  accscorefn <- compareacc(train$acceleration[i], trF5)
  
  fullscore <- cylscorefn + disscorefn + horscorefn + weiscorefn + accscorefn
  if (fullscore >= A) {
    
    train$fullscore[i] <- "HIGH MPG"
  }
  else if (fullscore <= (A* -1))  {
    train$fullscore[i] <- "LOW MPG"
  }
  
}

#Establish true class for each training observation
for (i in 1:trainrows) {
  if (train$mpg[i] < 22.75) {
    train$trueclass[i] <- "LOW MPG" 
  } else {
    train$trueclass[i] <- "HIGH MPG"
  }
}

#confusion matrix for training set
prop.table(table(train$trueclass, train$fullscore))


#Assign prediction class for test set
test <- data.frame(auto %>% filter(auto$mpg > 18.503, auto$mpg < 26.6))

testrows <- nrow(test)
for(i in 1:testrows) { 
  fullscore <- 0
  cylscorefn <- comparecyl(test$cylinders[i], trF1)
  disscorefn <- comparedis(test$displacement[i], trF2)
  horscorefn <- comparehor(test$horsepower[i], trF3)
  weiscorefn <- comparewei(test$weight[i], trF4)
  accscorefn <- compareacc(test$acceleration[i], trF5)
  
  fullscore <- cylscorefn + disscorefn + horscorefn + weiscorefn + accscorefn
  if (fullscore >= A) {
    
    test$fullscore[i] <- "HIGH MPG"
  }
  else if (fullscore <= (A* -1))  {
    test$fullscore[i] <- "LOW MPG"
  }
  
}

#Assign true class for test set
for (i in 1:testrows) {
  if (test$mpg[i] < 22.75) {
    test$trueclass[i] <- "LOW MPG" 
  } else {
    test$trueclass[i] <- "HIGH MPG"
  }
}

#Generate Confusion Matrix
prop.table(table(test$trueclass, test$fullscore))



