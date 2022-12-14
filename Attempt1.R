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
library(moments)
set.seed(2004)
# Set your own working directory
setwd("/Users/junlongng/Desktop/NTU/Year 2/Semester 1/BC2406 Analytics 1/Group Project")
### DATA CLEANING FOR LOGISTIC REGRESSION: PREDICTING READMISSION ----------------------------------------------------------------------------------------------------------------------------------------------
los_dt = fread('/Users/junlongng/Desktop/NTU/Year 2/Semester 1/BC2406 Analytics 1/Group Project/Data Sets/Dataset/dat5.csv', na.strings = c("NA", "missing", "N/A", "", "m", "M", "na", "."))
summary(los_dt) 
skim(los_dt)

sum(is.na(los_dt)) #Showed that there are NA values in every row
colSums(is.na(los_dt))

## Checking for percentage of data loss if we drop empty rows
sum(is.na(los_dt)) / ncol(los_dt)

## We will be dropping only 1.586%
## Going ahead and dropping this data

los_dt = na.omit(los_dt)
sum(is.na(los_dt))


## All NA values have been dropped

## We will drop people who have died and the id column
los_dt = los_dt[,c(3:31)]
str(los_dt)
skim(los_dt)

summary(los_dt$ethnicgroup)


## EDA 
## Finding data types

factor_list= c("gender","cancer","cabg","crt","defib","dementia","diabetes","hypertension","ihd", "mental_health", "arrhythmias","copd","obesity","pvd","renal_disease","valvular_disease","metastatic_cancer","pacemaker","pneumonia","pci","stroke","senile")
los_dt[, (factor_list):= lapply(.SD, factor), .SDcols = factor_list]


## Ordered Categorical values
los_dt$quintile = factor(los_dt$quintile, ordered = T, levels = c("1","2","3","4","5"))
los_dt$ethnicgroup = factor(los_dt$ethnicgroup, ordered = T, levels = c("1","2","3","8","9"))

sapply(los_dt, class)
summary(los_dt$ethnicgroup)

## Attempting to make categorical LOS
los_dt$losnew[(los_dt$los <= 7)] = "within 1 week"
los_dt$losnew[(los_dt$los > 7) & (los_dt$los <= 14)] = "within 2 weeks"
los_dt$losnew[(los_dt$los > 14) & (los_dt$los <= 30)] = "within 1 month"
los_dt$losnew[(los_dt$los > 30)] = "more than 1 month"
los_dt$losnew= factor(los_dt$losnew)
los_dt$losnew = factor(los_dt$losnew, ordered = T, levels = c("within 1 week","within 2 weeks","within 1 month","more than 1 month"))


sapply(los_dt, class)


factor = names(keep(los_dt,is.factor))
factor = factor[3:25]
gglist = list()
for(graph in 1:length(factor)){
  gglist[graph]=list(ggplot(data=los_dt,aes_string(factor[graph],fill='los'))+ geom_bar(position = 'dodge')+ ylab("Count of people") + theme(legend.position = c(.9,.75),legend.background=element_rect(fill = alpha("white", 0.5)))+ scale_fill_brewer(palette = 'Paired'))
}
ggarrange(plotlist=gglist)

## To work on this
continuous_list = names(keep(los_dt,is.integer))
# continuous_list
# cont_plot1 = list()
# for (i in 1:(length(continuous_list))) {
  ## Creating Box Plots using a for loop
#  cont_plot1[i] = list(ggplot(data = los_dt, aes_string(x ="Loan_Status", y = continuous_list[i], color = "Loan_Status")) + geom_boxplot() + theme(legend.position = c(1,2), legend.background = element_rect(fill = alpha("white", 0.5))) + scale_color_brewer(palette = "Dark2"))
#}
#ggarrange(plotlist = cont_plot1)


plot(density(los_dt$age))
skewness(los_dt$age)
plot(density(los_dt$prior_appts_attended))
skewness(los_dt$prior_appts_attended)
## is there a way to normalize this without affecting 0s
plot(density(los_dt$prior_dnas))
plot(density(los_dt$fu_time))

summary(los_dt)
skim(los_dt)
## Getting a finalised dataset 
## This final dataset we keep in prior_dnas and priorapptsattended for now, will run again without these two fields
final_dt = los_dt[,c("ethnicgroup", "fu_time", "los"):= NULL] 
final_dt = na.omit(final_dt)
skim(final_dt)

train_data = sample.split(Y=final_dt$losnew,SplitRatio = 0.7)
trainset = subset(final_dt,train_data==T)
testset = subset(final_dt, train_data==F)


summary(trainset$losnew)

trainset$losnew = relevel(trainset$losnew, ref ="within 2 weeks")
sapply(trainset, class)
model1=multinom(formula = losnew~.,data=trainset)
summary(model1)

?glm




