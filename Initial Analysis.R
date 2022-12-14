## Installing all the relevant packages
install.packages("ROSE")
library(data.table)
library(rpart)
library(rpart.plot)  
library(ROSE)


## Setting the working directory
setwd("/Users/junlongng/Desktop/NTU/Year 2/Semester 1/BC2406 Analytics 1/Group Project")

## Importing the data
cleaned_data = fread(file.choose())
cleaned_data[,"re.admission.within.28.days"]

## Setting seed as 2
set.seed(2)


## Since we're doing CART for logistric regression, we have to use GLM later on
## Factorizing the data for readmission 1 = yes, 0 = no has already been done by Yan Yu
## Let's try readmission within 3 months first
## This gives me all the other dependent variables for dt1
dt1 = cleaned_data[, c(9:31,33)]


## Factorise readmission 
## dt1$re.admission.within.28.days = factor(dt1$re.admission.within.28.days)

dt1 = rpart(re.admission.within.28.days ~ ., data = dt1, method = 'class', control = rpart.control(minsplit = 10, cp = 0))

dt1

printcp(dt1)
## sampled_dt = ovun.sample(re.admission.within.28.days ~., data=dt1, method = "both")



plotcp(dt1, main="subtrees in 28 days cart")
## Need to investigate why all the breakpoints are above the CP error line
print(dt1)


## This is the fully fleshed out tree that hasn't been pruned yet
rpart.plot(dt1, nn= T, main = "Maximal Tree for dt1")


## Automating the pruning
dt2 = data.table(dt1$cptable)
## Numbering the sequence of the trees
dt2[,index:=1:nrow(dt2)]
dt2
min_cp = min(dt2[(xerror == min(xerror)),index])
errorcap = dt2[min_cp,xerror + xstd]
optimal_cp_index = min(dt2[(xerror<errorcap), index])
cp.optimal = sqrt(dt2[index == optimal_cp_index, CP]*dt2[index == optimal_cp_index -1, CP])
cp.optimal

pruned_cart = prune(dt1, cp = cp.opt)
printcp(pruned_cart, digits = 3)
rpart.plot(pruned_cart, nn = T, main ="Optimal Tree")







