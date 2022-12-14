## Testing this shit out on VS Code
## Install packages
library(data.table)
library(rpart)
library(rpart.plot) 
library(stringr)
library(ggplot2)
library(caret)
library(skimr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(class)
library(RANN)
library(VIM)
library(ggpubr)
library(MLmetrics)
library(purrr)
library(ggpubr)
library(mice)
library(caTools)
library(ROSE)
set.seed(2004)
setwd("/Users/junlongng/Desktop/NTU/Year 2/Semester 1/BC2406 Analytics 1/Group Project/Data Sets/Dataset")
newdf = fread('/Users/junlongng/Desktop/NTU/Year 2/Semester 1/BC2406 Analytics 1/Group Project/Data Sets/Dataset/secondreadmissioncleaned_data.csv', na.strings = c("NA", "missing", "N/A", "", "m", "M", "na", "."))

## Data exploration
summary(newdf)
colnames(newdf)
ncol(newdf)
sapply(newdf, class)
old_df = newdf
## Need to convert "logical" to "factor" for further analysis


## EDA ##

plot(density(newdf$time_in_hospital)) 
newdf$time_in_hospital = log(newdf$time_in_hospital)
plot(density(newdf$num_lab_procedures)) 
newdf$num_lab_procedures = log(newdf$num_lab_procedures)
newerdf = newdf %>% mutate_if(is.integer, as.factor)
sapply(newerdf, class)

## Checking for duplicated values in columns that are supposed to be unique
newerdf$number_outpatient
newerdf$number_diagnoses
newerdf$`payer_code_?`
newerdf$glimepiride_No


## Converting class type integer to factor
# <- sapply(newerdf, class) == "integer"
#lcols <- names(lclass)[which(lclass==TRUE)]
#newerdf[, (lcols):= lapply(.SD, as.numeric), .SDcols = lcols]
#newerdf[, (lcols):= lapply(.SD, factor), .SDcols = lcols]


## Changing True values to be 1, False values to be 0 for all columns that is needed 
## Cheated by changing the values in excel 
testdf = newerdf
colnames(testdf)[colnames(testdf) %in% c("age_[40-50)","age_[50-60)","age_[60-70)","age_[70-80)","age_[80-90)")]= c("Age40_50","Age50_60","Age60_70","Age70_80","Age80_90")
colnames(testdf)

## Taking a closer look into V1
testdf$V1 
## Seems to be ID of person. Dropping the column
cleaning_df = testdf[,c(-1)]
ncol(cleaning_df)
sapply(cleaning_df, class)


## Extracting useful features
cleaning_df = cleaning_df[,c(1:16,64:65)]
sapply(cleaning_df, class)


## Conducting EDA
sum(is.na(cleaning_df))
## There are no NA values

str(cleaning_df)

## Plotting graphs 
## Plotting for cateforical graphs
factor = names(keep(cleaning_df,is.factor))
gglist1 = list()
factor
for (graph in 1:(length(factor)-1)) {
  gglist1[graph]=list(ggplot(data=cleaning_df,aes_string(factor[graph],fill='readmitted'))+ geom_bar(position = 'dodge')+ ylab("Count of people") + theme(legend.position = c(.9,.75),legend.background=element_rect(fill = alpha("white", 0.5)))+ scale_fill_brewer(palette = 'Paired'))
}

ggarrange(plotlist=gglist1, nrow = 5, ncol = 4)

summary(cleaning_df$number_emergency)
summary(cleaning_df$num_lab_procedures)
summary(cleaning_df$num_procedures)


plot(density(old_df$time_in_hospital))


## There are some fields with several categories. How should I deal with this?

cleaning_df2 = cleaning_df

## Further extracting more categorical variables 

cleaning_df2 = cleaning_df[,c(9:18)]
cleaning_df2
sapply(cleaning_df2, class)


## Replotting the graphs
factor2 = names(keep(cleaning_df2,is.factor))
gglist2 = list()

for (graph in 1:(length(factor2)-1)) {
  gglist2[graph]=list(ggplot(data=cleaning_df,aes_string(factor[graph],fill='readmitted'))+ geom_bar(position = 'dodge')+ ylab("Count of people") + theme(legend.position = c(.9,.75),legend.background=element_rect(fill = alpha("white", 0.5)))+ scale_fill_brewer(palette = 'Paired'))
}
ggarrange(plotlist=gglist2, nrow = 5, ncol = 4)

summary(cleaning_df)

## Building traintest split
train_data = sample.split(Y = cleaning_df$readmitted, SplitRatio = 0.7)
trainset = subset(cleaning_df, train_data == T)
testset = subset(cleaning_df, train_data == F)


## Looking at the sample distribution for balancing
summary(trainset$readmitted)
## There is at least 1,000 more 1s then 0s
balancetrain_data = ovun.sample(readmitted~., data=trainset, seed = 2004, method = "over", N = (2*9513))$data
summary(balancetrain_data)
table(balancetrain_data$readmitted)
## Equal sampling of 0s and 1s for readmitted dataset



## Performing Logistic Regression
model1 = glm(readmitted ~., data=balancetrain_data, family = "binomial")
summary(model1)

## Performing Backwards elimination
model2 = glm(readmitted ~ race_Caucasian+race_AfricanAmerican+gender_Female+Age40_50+Age50_60+ Age70_80+diabetesMed_Yes, data=balancetrain_data, family = "binomial")
summary(model2)

model3 = glm(readmitted ~ race_Caucasian+race_AfricanAmerican+Age40_50+Age50_60+ Age70_80+diabetesMed_Yes, data=balancetrain_data, family = "binomial")
summary(model3)

model4 = glm(readmitted ~ race_Caucasian+race_AfricanAmerican+Age70_80+diabetesMed_Yes, data=balancetrain_data, family = "binomial")
summary(model4)

model_list = c(AIC(model1), AIC(model2), AIC(model3), AIC(model4))
min(model_list)


## Model 3 is the most accurate
levels(balancetrain_data$readmitted)


prob = predict(model3, type = 'response')
classifier = ifelse(prob>0.5, "0", "1")
classifier=as.factor(classifier)
confusionMatrix(classifier, balancetrain_data$readmitted)
## Logistic Regression has a 46.7% accuracy on balanced data
## How to increase the accuracy


## Testing out on the testset with log model trained on balanced data
test_prob = predict(model3, newdata=testset, type="response")
test_classifier = ifelse(test_prob>0.5,"0","1")
test_classifier = as.factor(test_classifier)
confusionMatrix(test_classifier,testset$readmitted)
log_model_acc = mean(test_classifier == testset$readmitted) * 100
log_model_acc
## Logistic Regression has an accuracy of 47.97%


## Performing CART analysis
bcart = rpart(readmitted~. , data=balancetrain_data, method="class", control = rpart.control(cp=0))
printcp(bcart)
plotcp(bcart)
rpart.plot(bcart, nn = T, main="Maximum CART Tree")

cverrorcap = bcart$cptable[which.min(bcart$cptable[,"xerror"]),"xerror"] + bcart$cptable[which.min(bcart$cptable[,"xerror"]),"xstd"]

i = 1; j=4
while (bcart$cptable[i,j] > cverrorcap) {
  i = i+1
}

optimal_cp = ifelse(i > 1, sqrt(bcart$cptable[i,1] * bcart$cptable[i-1,1]),1)
optimal_cp


bcart2 = prune(bcart, cp = optimal_cp, minsplit = 1)
rpart.plot(bcart2,nn=T,main="Newer Pruned Tree")

## Testing CART on trainset data
predictcart1 = predict(bcart2, newdata = balancetrain_data, type = "class")
result1 = data.frame(balancetrain_data$readmitted, predictcart1)
cart1_accuracy = mean(predictcart1 == balancetrain_data$readmitted)
table(actual = balancetrain_data$readmitted, predictcart1)
cart1_accuracy = cart1_accuracy*100
cart1_accuracy

## Cart has a higher accuracy of 54.55%

## Using it on the testset
predictcart2 = predict(bcart2, newdata = testset, type="class")
result2 = data.frame(testset$readmitted, predictcart2)
cart2accuracy = mean(predictcart2 == testset$readmitted)
table(actual = testset$readmitted, predict=predictcart2)
cart2accuracy = cart2accuracy * 100
cart2accuracy 

## On testdata Cart has a lower accuracy of 52.46%

accuracy_table = data.frame("Model_Table" = 2:1)
rownames(accuracy_table) = c("CART on Balanced Data", "Logistic Regression on Balanced Data")
colnames(accuracy_table) = "Model Accuracy In Percentage"
accuracy_table[1,1] = cart2accuracy
accuracy_table[2,1] = log_model_acc
accuracy_table

## Where to continue
## What category should be conntinuous, 
## Should you group outliers together, for example num medication, should you add up all the small numbers together and lump them under one category: 60+>?

## Taking a closer look at the density of certain categorical variables













































##Setting seed 
set.seed(2004)
testdf
dt1 = rpart(readmitted ~., data = testdf, method ="class",control =rpart.control(minsplit=20,cp=0))
dt1
printcp(dt1)





plotcp(dt1,main="unpruned tree")

## Don't run this unless you want to wait damn long for the full tree 
# rpart.plot(dt1, nn= T, main = "unpruned tree")

## Automate the pruning process
dt2 = data.table(dt1$cptable)

## Numbering the sequence of the trees
dt2[,index:=1:nrow(dt2)]
dt2
min_cp_index = min(dt2[(xerror == min(xerror)),index])
errorcap = dt2[min_cp_index, xerror + xstd]
optimal_cp_index = min(dt2[(xerror < errorcap), index])
cp.optimal = sqrt(dt2[index == optimal_cp_index, CP]*dt2[index == optimal_cp_index -1, CP])
cp.optimal


## Creating the pruned cart
pruned_cart = prune(dt1, cp = cp.optimal)
printcp(pruned_cart, digits = 3)
rpart.plot(pruned_cart, nn = T, main ="Optimal Tree")
pruned_cart$variable.importance

## Saving the csv
## write.csv(testdf, "/Users/junlongng/Desktop/NTU/Year 2/Semester 1/BC2406 Analytics 1/Group Project/Data Sets/hospitalized-patients-with-heart-failure-integrating-electronic-healthcare-records-and-external-outcome-data-1.3/secondreadmissioncleaned_data.csv")






