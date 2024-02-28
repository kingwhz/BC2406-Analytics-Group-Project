# SEM 4 GROUP 2 ================================================================
# GOAL: Find out what contributes to heart disease (target == 1) ================
# Edit as of 14/12/22: Added this to github

# Importing Libraries ----------------------------------------------------------
library(car)
library(data.table)
library(ggplot2)
library(ggcorrplot)
library(caTools)
library(rpart)
library(rpart.plot)  
library(ggrepel)
library(moments)
library(dplyr)
library(tibble)
library(tidyverse)
library(ROSE)


# Importing Dataset ------------------------------------------------------------
heart.dt <- fread("heart.csv", stringsAsFactors = T, na.strings = c("NA", "missing", "N/A", "", "m", "M", "na", "."))   
View(heart.dt)

# DATA PREPARATION & CLEANING ==================================================
## Checking datatype ----
str(heart.dt)
ncol(heart.dt)

## Setting the appropriate datatype for each variable ----
heart.dt$sex <- as.factor(heart.dt$sex)
heart.dt$cp <- as.factor(heart.dt$cp)
heart.dt$fbs <- as.factor(heart.dt$fbs)
heart.dt$restecg <- as.factor(heart.dt$restecg)
heart.dt$exang <- as.factor(heart.dt$exang)
heart.dt$thal <- as.factor(heart.dt$thal)
heart.dt$ca <- as.factor(heart.dt$ca)
heart.dt$slope <- as.factor(heart.dt$slope)
heart.dt$target <- as.factor(heart.dt$target)

## Formatting the dataset ----
heart.dt[heart.dt$sex == 0,]$sex <- "Female"
heart.dt[heart.dt$sex == 1,]$sex <- "Male"

heart.dt[heart.dt$fbs == 0,]$fbs <- "False"
heart.dt[heart.dt$fbs == 1,]$fbs <- "True"

heart.dt[heart.dt$restecg == 0,]$restecg <- "Normal"
heart.dt[heart.dt$restecg == 1,]$restecg <- "ST-T Abnormality"
heart.dt[heart.dt$restecg == 2,]$restecg <- "Hypertrophy"

heart.dt[heart.dt$cp == 0,]$cp <- "Typical Anginal"
heart.dt[heart.dt$cp == 1,]$cp <- "Atypical Anginal"
heart.dt[heart.dt$cp == 2,]$cp <- "Non-anginal Anginal"
heart.dt[heart.dt$cp == 3,]$cp <- "Asymptomatic"

heart.dt[heart.dt$slope == 0,]$slope <- "Upsloping"
heart.dt[heart.dt$slope == 1,]$slope <- "Flat"
heart.dt[heart.dt$slope == 2,]$slope <- "Downsloping"

heart.dt[heart.dt$exang == 0,]$exang <- "No"
heart.dt[heart.dt$exang == 1,]$exang <- "Yes"

heart.dt[heart.dt$thal == 1,]$thal <- "Normal"
heart.dt[heart.dt$thal == 2,]$thal <- "Fixed Defect"
heart.dt[heart.dt$thal == 3,]$thal <- "Reversible Defect"

## Boxplots to confirm the existence of outliers ----
boxplot(heart.dt$age)
boxplot(heart.dt$trestbps)
boxplot(heart.dt$chol)
boxplot(heart.dt$thalach)
boxplot(heart.dt$oldpeak)
boxplot(heart.dt$slope)
# Note: Do not remove yet. Outliers may carry important information, could be true natural outliers. 

# DATA VISUALISATION & EXPLORATION =============================================

## Use stacked barchart to show relationship between X and Y variables ----
# https://rkabacoff.github.io/datavis/Bivariate.html
# Y is filler. 
ggplot(heart.dt, aes(sex)) + geom_bar(aes(fill=target)) + ggtitle("Heart disease across Gender") 
ggplot(heart.dt, aes(cp)) + geom_bar(aes(fill=target)) + ggtitle("Heart disease across Chest pain type")
ggplot(heart.dt, aes(fbs)) + geom_bar(aes(fill=target)) + ggtitle("Heart disease across patients with FBS>120mg/dl")
ggplot(heart.dt, aes(restecg)) + geom_bar(aes(fill=target)) + ggtitle("Heart disease across ECG results")
ggplot(heart.dt, aes(exang)) + geom_bar(aes(fill=target)) + ggtitle("Heart disease across Exercise Induced Angina")
ggplot(heart.dt, aes(ca)) + geom_bar(aes(fill=target)) + ggtitle("Heart disease across number of major vessels")
ggplot(heart.dt, aes(thal)) + geom_bar(aes(fill=target)) + ggtitle("Heart disease across defect types")
ggplot(heart.dt, aes(slope)) + geom_bar(aes(fill=target)) + ggtitle("Heart disease across slope of peak exercise")

## Use correlation map ----
continuous.dt <- heart.dt[,c(1,4,5,8,10)]
corr <- round(cor(continuous.dt),2)
ggcorrplot(corr, hc.order = TRUE, type = "lower", outline.col = "black",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE)

# Discovered that thal has a category '0'. Removing it because 0 does not represent defect type.
heart.dt[thal=="0",.N]
nrow(heart.dt)
heart.dt<-heart.dt[!thal=="0",]
nrow(heart.dt)
ggplot(heart.dt, aes(thal)) + geom_bar(aes(fill=target)) + ggtitle("Heart disease across defect types")
# Successfully removed 7 rows of thal = "0" 

## Density plot for 1 categorical, 1 numeric ----
# Blue line represents heart disease, red line represents no heart disease.
# Run across all continuous X
ggplot(heart.dt, aes(x=age, col=target)) + geom_density()
ggplot(heart.dt, aes(x=trestbps, col=target)) + geom_density()
ggplot(heart.dt, aes(x=chol, col=target)) + geom_density()
ggplot(heart.dt, aes(x=thalach, col=target)) + geom_density()
ggplot(heart.dt, aes(x=oldpeak, col=target)) + geom_density() #Looks weird

## Data cleaning for logistic regression ----
sum(is.na(heart.dt)) # 0 NA values
skewness(heart.dt$age)
skewness(heart.dt$chol)
skewness(heart.dt$trestbps)
skewness(heart.dt$oldpeak)
# Continuous variables are not very skewed, no need to log. 

# MODEL BUILDING (LOGISTIC REGRESSION) WITH OUTLIERS ===========================

## Building model for prediction ----
set.seed(2004)
split <- sample.split(Y = heart.dt$target, SplitRatio = 0.7)
trainset <- subset(heart.dt, split == T) 
testset <- subset(heart.dt, split == F)

heart.log1 <- glm(target~.,data=trainset,family="binomial")
summary(heart.log1) # 467.86
step(heart.log1) ## Backward elimination ----
heart.log2<-glm(formula = target ~ age + sex + cp + trestbps + chol + thalach + 
                  exang + oldpeak + slope + ca + thal, family = "binomial", 
                data = trainset)
summary(heart.log2) # 464.4

## This shows us that there is no concrete evidence of multi-collinearity using the VIF function.
vif(heart.log2)

heart.dt = heart.dt[,c("ca","slope"):=NULL]
heart.dt

split <- sample.split(Y = heart.dt$target, SplitRatio = 0.7)
trainset <- subset(heart.dt, split == T) 
testset <- subset(heart.dt, split == F)

heart.log1 <- glm(target~.,data=trainset,family="binomial")
summary(heart.log1) # 467.86
step(heart.log1) ## Backward elimination ----
heart.log2<-glm(formula = target ~ age + sex + cp + trestbps + chol + thalach + exang + 
                  oldpeak + thal, family = "binomial", 
                data = trainset)
summary(heart.log2) # 464.4

## This shows us that there is no concrete evidence of multi-collinearity using the VIF function.
vif(heart.log2)


## Set OR through coefficient level ----
OR <- exp(coef(heart.log2))
OR
## Set OR.CI through confidence interval level ----
OR.CI <- exp(confint(heart.log2))
OR.CI

## MODEL EVALUATION (LOGISTIC REGRESSION) WITH OUTLIERS =========================
## Confusion matrix on TESTSET ----
log.prob <- predict(heart.log2, newdata= testset, type = 'response')
threshold <- 0.5 
log.predict.test <- ifelse(log.prob > threshold, "1" , "0" )
log.cm <- table(test.Actual = testset$target, log.predict.test, deparse.level = 2)
log.cm
log.cm.alt <- confusionMatrix(as.factor(log.predict.test), reference = testset$target, positive = "1")
log.cm.alt

## Overall accuracy, sensitivity, specificity on TESTSET ----
## Log1, log1_type1, log1_type2 will be used in a datatable to showcase all accuracies of all models later on
log1 <- round(mean(log.predict.test == testset$target)*100,2) #87.9% 
log1_type1 <- (24 / (125 +24 +13 + 144)) * 100
log1_type2 <- (13 / (125 +24 +13 + 144)) * 100

sensitivity <- 91.72
specificity <- 83.89

log.cm.matrix <- cbind.data.frame(log1, sensitivity, specificity)
log.cm.matrix

## Confusion matrix on TRAINSET ----
log.prob2 <- predict(heart.log2, type = 'response')
threshold <- 0.5 
log.predict.train <- ifelse(log.prob2 > threshold, "1" , "0" )
log.cm2 <- table(train.Actual = trainset$target, log.predict.train, deparse.level = 2)
log.cm2
log.cm2.alt <- confusionMatrix(as.factor(log.predict.train), reference = trainset$target, positive = "1")
log.cm2.alt

## Overall accuracy, sensitivity, specificity on TRAINSET ----
accuracy <- round(mean(log.predict.train == trainset$target)*100,2) #88.9%
accuracy
sensitivity <- 91.26
specificity <- 86.42

log.cm2.matrix <- cbind.data.frame(accuracy, sensitivity, specificity)
log.cm2.matrix


# Model built on trainset has a 87.9% accuracy on testset data, but has a 88.9% accuracy on trainset data.

# MODEL BUILDING (CART) WITH OUTLIERS ==========================================

set.seed(2004)
m1.cart <- rpart(target~., data = trainset , method = 'class', control = rpart.control(minsplit = 15, cp = 0))
print(m1.cart)
printcp(m1.cart)
plotcp(m1.cart)
cverrorcap = m1.cart$cptable[which.min(m1.cart$cptable[,"xerror"]),"xerror"] + m1.cart$cptable[which.min(m1.cart$cptable[,"xerror"]),"xstd"]
i = 1; j=4
while (m1.cart$cptable[i,j] > cverrorcap) {
  i = i+1
}
optimal_cp1 = ifelse(i > 1, sqrt(m1.cart$cptable[i,1] * m1.cart$cptable[i-1,1]),1)
m2.cart <- prune(m1.cart, cp = optimal_cp1)
plotcp(m2.cart)
rpart.plot(m2.cart, nn= T, main = "Pruned Tree with new cp")

# MODEL EVALUATION (CART) WITH OUTLIERS ========================================

## Confusion matrix on testset ----
cart.predict.test <- predict(m2.cart, newdata= testset, type = 'class')
result.test <- table(Actual = testset$target, cart.predict.test, deparse.level = 2)
result.test.alt <- confusionMatrix(as.factor(cart.predict.test), reference = testset$target, positive = "1")
result.test.alt


## cart1, cart1_type1,cart1_type2 will be used in a table for analysis of all models
## Overall accuracy, sensitivity, specificity on testset ----
cart1 <- round(mean(cart.predict.test == testset$target)*100,2) #89.22% 
cart1 <- mean(testset$target == cart.predict.test)*100
cart1_type1 <- (23 / (126+10+23+147)) * 100
cart1_type2 <- (10 / (126+10+23+147)) * 100


sensitivity <- 93.63
specificity <- 84.56

result.test.matrix <- cbind.data.frame(accuracy, sensitivity, specificity)
result.test.matrix

## Confusion matrix on trainset ----
cart.predict.train <- predict(m2.cart, type = 'class')
result.train <- table(Actual = trainset$target, cart.predict.train, deparse.level = 2)
result.train.alt <- confusionMatrix(as.factor(cart.predict.train), reference = trainset$target, positive = "1")
result.train.alt

## Overall accuracy, sensitivity, specificity on trainset ----
accuracy <- round(mean(cart.predict.train == trainset$target)*100,2) #91.71% 
sensitivity <- 93.44
specificity <- 89.88

result.train.matrix <- cbind.data.frame(accuracy, sensitivity, specificity)
result.train.matrix

## Variable importance ----
m2.cart$variable.importance
# cp is the most important variable, followed by ca, thalach and thal.

## Show variable importance in chart ----
vi1<-
  m2.cart$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Model 1 VI")
vi1
## General overview of present outliers ----
par(mar=c(5,10,2,2), mgp = c(8, 1, 0),las = 1)
data <- heart.dt[,c(1,4:5,8,10)]
boxplot(data,
        horizontal = TRUE,names = c(colnames(data)),main = "Boxplot of All Variables (Excluding unwanted X variables and Y variable)",
        ylab = "Independent Variables") 

## Density plot of each variable to see if data is skewed ----
plot(density(heart.dt$chol)) #Right skewed 
plot(density(heart.dt$thalach)) #Left skewed
plot(density(heart.dt$trestbps)) #Right skewed
plot(density(heart.dt$oldpeak)) #Right skewed

# CREATE A SUBSET WITH NO OUTLIERS =============================================

## Removing outliers (chol) ----
boxplot(heart.dt$chol, data = heart.dt) # Check for outliers
Q1_chol <- quantile(heart.dt$chol, .25)
Q3_chol <- quantile(heart.dt$chol, .75)
IQR <- IQR(heart.dt$chol)
heart.dt2 <- subset(heart.dt, heart.dt$chol > (Q1_chol -1.5*IQR) & heart.dt$chol < (Q3_chol + 1.5*IQR))
dim(heart.dt2)
# Checking if outliers are removed (chol)
boxplot(heart.dt2$chol, data = heart.dt2)

## Removing outliers (oldpeak) ----
boxplot(heart.dt$oldpeak, data = heart.dt) # Check for outliers
Q1_oldpeak <- quantile(heart.dt$oldpeak, .25)
Q3_oldpeak <- quantile(heart.dt$oldpeak, .75)
IQR_oldpeak <- IQR(heart.dt$oldpeak)
heart.dt2 <- subset(heart.dt, heart.dt$oldpeak > (Q1_oldpeak -1.5*IQR_oldpeak) & heart.dt$oldpeak < (Q3_oldpeak + 1.5*IQR_oldpeak))
dim(heart.dt2)
# Checking if outliers are removed (oldpeak)
boxplot(heart.dt2$oldpeak, data = heart.dt2)

## Removing outliers (thalach) ----
boxplot(heart.dt$thalach, data = heart.dt) # Check for outliers
Q1_thalach <- quantile(heart.dt$thalach, .25)
Q3_thalach <- quantile(heart.dt$thalach, .75)
IQR_thalach <- IQR(heart.dt$thalach)
heart.dt2 <- subset(heart.dt, heart.dt$thalach > (Q1_thalach -1.5*IQR_thalach) & heart.dt$thalach < (Q3_thalach + 1.5*IQR_thalach))
dim(heart.dt2)
# Checking if outliers are removed (thalach)
boxplot(heart.dt2$thalach, data = heart.dt2)

## Removing outliers (trestbps) ----
boxplot(heart.dt$trestbps, data = heart.dt) # Checking for outliers
Q1_trestbps <- quantile(heart.dt$trestbps, .25)
Q3_trestbps <- quantile(heart.dt$trestbps, .75)
IQR_trestbps <- IQR(heart.dt$trestbps)
heart.dt2 <- subset(heart.dt, heart.dt$trestbps > (Q1_trestbps -1.5*IQR_trestbps) & heart.dt$trestbps < (Q3_trestbps + 1.5*IQR_trestbps))
dim(heart.dt2)
# Checking if outliers are removed (trestbps)
boxplot(heart.dt2$trestbps, data = heart.dt2)

## Checking distribution of variables ----
plot(density(heart.dt2$chol)) # Right skewed 
plot(density(heart.dt2$thalach)) # Left skewed
plot(density(heart.dt2$trestbps)) # Right skewed
plot(density(heart.dt2$oldpeak)) # Right skewed
# There are not many outliers in chol thalach and oldpeak that warrant a change in density 


# MODEL BUILDING (LOGISTIC REGRESSION) WITHOUT OUTLIERS ========================

set.seed(2004)
split2 <- sample.split(Y = heart.dt2$target, SplitRatio = 0.7) 
trainset2 <- subset(heart.dt2, split2 == T) 
testset2 <- subset(heart.dt2, split2 == F)

heart.log1_2 <- glm(target~.,data=trainset2,family="binomial")
summary(heart.log1_2) # 431.69
step(heart.log1_2) ## Backward elimination, no variables removed ----
heart.log2_2<-glm(formula = target ~ sex + cp + chol + thalach + oldpeak + 
                    thal, family = "binomial", data = trainset2)
summary(heart.log2_2) # 428.1

## Set OR through coefficient level ----
OR <- exp(coef(heart.log2_2))
OR
## Set OR.CI through confidence interval level ----
OR.CI <- exp(confint(heart.log2_2))
OR.CI

# MODEL EVALUATION (LOGISTIC REGRESSION) WITHOUT OUTLIERS ======================

## Confusion matrix on testset ----
log.prob2_2 <- predict(heart.log2_2, newdata= testset2, type = 'response')
threshold <- 0.5 
log.predict.test2 <- ifelse(log.prob2_2 > threshold, "1" , "0" )
log.cm_2 <- table(Actual = testset2$target, log.predict.test2, deparse.level = 2)
log.cm_2.alt <- confusionMatrix(as.factor(log.predict.test2), reference = testset2$target, positive = "1")
log.cm_2.alt

## Overall accuracy, sensitivity, specificity on testset ----
accuracy <- round(mean(log.predict.test2 == testset2$target )*100,2) #85.27%, accuracy decreased from 87.9 % , 2.5 % decrease in accuracy.

## Log2, log2_type1, log2_type2 will be used in a table analysis later on
log2 <-(mean(log.predict.test2 == testset2$target ) * 100)
log2_type1 <- (21 / (118 +22+21+131)) * 100
log2_type2 <- (22 / (118 +22+21+131)) * 100

sensitivity <- 85.62
specificity <- 84.89

log.cm_2.matrix <- cbind.data.frame(accuracy, sensitivity, specificity)
log.cm_2.matrix

## Confusion matrix on trainset ----
log.prob2_2 <- predict(heart.log2_2, type = 'response')
threshold <- 0.5 
log.predict.train2 <- ifelse(log.prob2_2 > threshold, "1" , "0" )
log.cm2_2 <- table(Actual = trainset2$target, log.predict.train2, deparse.level = 2)
log.cm2_2.alt <- confusionMatrix(as.factor(log.predict.train2), reference = trainset2$target, positive = "1")
log.cm2_2.alt

## Overall accuracy, sensitivity, specificity on trainset ----
accuracy <- round(mean(log.predict.train2 == trainset2$target)*100,2) #88.69%, 3% higher than testset
sensitivity <- 89.64
specificity <- 87.65

log.cm2_2.matrix <- cbind.data.frame(accuracy, sensitivity, specificity)
log.cm2_2.matrix


# MODEL BUILDING (CART) WITHOUT OUTLIERS ======================================= 

set.seed(2004)
m1.cart2 <- rpart(target~., data = trainset2 , method = 'class', control = rpart.control(minsplit = 15, cp = 0))
print(m1.cart2)
printcp(m1.cart2)
plotcp(m1.cart2)
cverrorcap = m1.cart2$cptable[which.min(m1.cart2$cptable[,"xerror"]),"xerror"] + m1.cart2$cptable[which.min(m1.cart2$cptable[,"xerror"]),"xstd"]
i = 1; j=4
while (m1.cart2$cptable[i,j] > cverrorcap) {
  i = i+1
}
optimal_cp2 = ifelse(i > 1, sqrt(m1.cart2$cptable[i,1] * m1.cart2$cptable[i-1,1]),1)
m2.cart2 <- prune(m1.cart2, cp = optimal_cp2)
plotcp(m2.cart2)
rpart.plot(m2.cart2, nn= T, main = "Pruned Tree with new cp")

# MODEL EVALUATION (CART) WITHOUT OUTLIERS =====================================

## Overall accuracy, sensitivity, specificity on trainset ----
cart.predict.train2 <- predict(m2.cart2, type = 'class')
result.train2 <- table(Actual = trainset2$target, cart.predict.train2, deparse.level = 2)
mean(trainset2$target == cart.predict.train2) #88.8% , increased in accuracy as compared to model with outliers. 
# Accuracy increased but there are still differences between in the accuracies of testset and trainset. 

## Overall accuracy, sensitivity, specificity on testset ----
cart.predict.test2 <- predict(m2.cart2, newdata= testset2, type = 'class')
result.test2 <- table(Actual = testset2$target, cart.predict.test2, deparse.level = 2)
mean(testset2$target == cart.predict.test2) #85.61%

## cart2, cart2_type1, cart2_type2 will be used in a data table for analysis later
cart2 = mean(testset2$target == cart.predict.test2)  * 100
cart2_type1 = (20 / (119+20+22+131)) * 100
cart2_type2 = (22 / (119+20+22+131)) * 100


## Variable importance in cart2 model ----
m2.cart2$variable.importance

## Show variable importance in chart ----
vi2<-
  m2.cart2$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Model 2 VI")
vi2



# MODEL BUILDING (LOGISTIC REGRESSION) AFTER BALANCING =========================

set.seed(2004)
summary(trainset2$target)
balancetrain_data = ovun.sample(target~., data=trainset2, method = "over", N = (2*357))$data
table(balancetrain_data$target)
summary(balancetrain_data)

heart.log1_3 <- glm(target~.,data=balancetrain_data,family="binomial")
summary(heart.log1_3)# 455.45
step(heart.log1_3) # Backward elimination, no variables removed ----
heart.log2_3<- glm(formula = target ~ age + sex + cp + trestbps + chol + restecg + 
                     thalach + oldpeak + thal, family = "binomial", 
                   data = balancetrain_data)
summary(heart.log2_3) # 452.7

## Set OR through coefficient level ----
OR <- exp(coef(heart.log2_3))
OR
## Set OR.CI through confidence interval level ----
OR.CI <- exp(confint(heart.log2_3))
OR.CI

# MODEL EVALUATION (LOGISTIC REGRESSION) AFTER BALANCING =======================

## Confusion matrix on testset ----
log.prob2_3 <- predict(heart.log2_3, newdata= testset2, type = 'response')
threshold <- 0.5 
log.predict.test3 <- ifelse(log.prob2_3 > threshold, "0" , "1" )
log.cm_3 <- table(Actual = testset2$target, log.predict.test3, deparse.level = 2)
log.cm_3

## Overall accuracy on testset ----
mean(log.predict.test3 == testset2$target ) # 85.95%  

## Log3, log3_type1, log3_type2 will be used in analysis later
log3 = mean(log.predict.test3 == testset2$target )*100
log3_type1 = (21 / (118 +21 +20 +133)) * 100
log3_type2 = (20 / (118 +21 +20 +133)) * 100

## Confusion matrix on trainset ----
log.prob2_3 <- predict(heart.log2_3, type = 'response')
threshold <- 0.5 
log.predict.train3 <- ifelse(log.prob2_3> threshold, "0" , "1" )
log.cm2_3 <- table(Actual = balancetrain_data$target, log.predict.train3, deparse.level = 2)
log.cm2_3
## Overall accuracy on trainset ----
mean(log.predict.train3 == balancetrain_data$target) #89.5%, very close to testset accuracy. 


# MODEL BUILDING (CART) AFTER BALANCING ========================================

set.seed(2004)
m1.cart3 <- rpart(target~., data = balancetrain_data , method = 'class', control = rpart.control(minsplit = 15, cp = 0))
print(m1.cart3)
printcp(m1.cart3)
plotcp(m1.cart3)
cverrorcap = m1.cart3$cptable[which.min(m1.cart3$cptable[,"xerror"]),"xerror"] + m1.cart3$cptable[which.min(m1.cart3$cptable[,"xerror"]),"xstd"]
i = 1; j=4
while (m1.cart3$cptable[i,j] > cverrorcap) {
  i = i+1
}
optimal_cp3 = ifelse(i > 1, sqrt(m1.cart3$cptable[i,1] * m1.cart3$cptable[i-1,1]),1)
m2.cart3 <- prune(m1.cart3, cp = optimal_cp3)
plotcp(m2.cart3)
rpart.plot(m2.cart3, nn= T, main = "Pruned Tree with new cp")

# MODEL EVALUATION (CART) AFTER BALANCING ======================================

## Accuracy on test set ----
cart.predict.test3 <- predict(m2.cart3, newdata= testset2, type = 'class')
result.test3 <- table(Actual = testset2$target, cart.predict.test3, deparse.level = 2)
mean(testset2$target == cart.predict.test3) # 90%, accuracy increases

## Cart3, Cart3_type1, Cart3_type2 will be used later on for analysis in a data table
cart3 = mean(testset2$target == cart.predict.test3) * 100
cart3_type1 = (16 / (16+123+140+13)) *100
cart3_type2 = (13 / (16+123+140+13)) *100


## Accuracy on trainset ----
cart.predict.train3 <- predict(m2.cart3, type = 'class')
result.train3 <- table(Actual = balancetrain_data$target, cart.predict.train3, deparse.level = 2)
mean(balancetrain_data$target == cart.predict.train3) # 94.81% 

## Variable importance in cart2 model ----
m2.cart3$variable.importance

## Show variable importance in chart ----
vi3<-
  m2.cart3$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Model 3 VI")
vi3

#Putting all the cart trees together 
par(mfrow=c(3,1))
rpart.plot(m2.cart, nn= T, main = "Pruned Tree cart model 1")
rpart.plot(m2.cart2, nn= T, main = "Pruned Tree cart model 2")
rpart.plot(m2.cart3, nn= T, main = "Pruned Tree cart model 3")
par(mfrow=c(1,1))

#Put variable importance plot for all three models together. 
ggarrange(vi1, vi2, vi3 + rremove("x.text"),
          ncol = 3, nrow = 1)


## Collating all accuracies, type 1 errors, type 2 errors together in a single data table

accuracy_table = data.frame("Accuracy of Different Models" = 6:1)
rownames(accuracy_table) = c("Log1", "Log2", "Log3", "CART 1" , "CART 2", "CART 3")

accuracy_table[1,1] = log1
accuracy_table[1,2] = log1_type1
accuracy_table[1,3] = log1_type2
accuracy_table[2,1] = log2
accuracy_table[2,2] = log2_type1
accuracy_table[2,3] = log2_type2
accuracy_table[3,1] = log3
accuracy_table[3,2] = log3_type1
accuracy_table[3,3] = log3_type2
accuracy_table[4,1] = cart1
accuracy_table[4,2] = cart1_type1
accuracy_table[4,3] = cart1_type2
accuracy_table[5,1] = cart2
accuracy_table[5,2] = cart2_type1
accuracy_table[5,3] = cart2_type2
accuracy_table[6,1] = cart3
accuracy_table[6,2] = cart3_type1
accuracy_table[6,3] = cart3_type2

colnames(accuracy_table) = c("Model Accuracy In Percentage", "TYPE 1 ERROR RATE", "TYPE 2 ERROR RATE")
accuracy_table


