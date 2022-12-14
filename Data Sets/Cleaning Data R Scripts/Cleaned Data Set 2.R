library(data.table)
setwd("C:/Users/Rachel Goh/OneDrive - Nanyang Technological University/NTU documents/Y2S1/BC2406_Biz Analytics 1/Project")

heart_upload.dt <- fread("heart_cleveland_upload (Data Set 2).csv", na.strings = c("NA", "missing", "N/A", -99, "", "m", "M", "na", "."))
View(heart_upload.dt)
summary(heart_upload.dt)

sapply(heart_upload.dt, class)

# factor required categories
cols1 <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal", "condition")
heart_upload.dt[, (cols1):= lapply(.SD, factor), .SDcols = cols1]
sapply(heart_upload.dt, class) #check for class after factorising variables

#check for na values
sum(is.na(heart_upload.dt))
summary(heart_upload.dt)

#remove outliers (for chol)
boxplot(heart_upload.dt$chol, data = heart_upload.dt)
Q1_chol <- quantile(heart_upload.dt$chol, .25)
Q3_chol <- quantile(heart_upload.dt$chol, .75)
IQR <- IQR(heart_upload.dt$chol)

heart_upload.dt <- subset(heart_upload.dt, heart_upload.dt$chol > (Q1_chol -1.5*IQR) & heart_upload.dt$chol < (Q3_chol + 1.5*IQR))
dim(heart_upload.dt)

boxplot(heart_upload.dt$chol, data = heart_upload.dt) #check if outliers are removed

#remove outliers (for oldpeak)
boxplot(heart_upload.dt$oldpeak, data = heart_upload.dt)
Q1_oldpeak <- quantile(heart_upload.dt$oldpeak, .25)
Q3_oldpeak <- quantile(heart_upload.dt$oldpeak, .75)
IQR_oldpeak <- IQR(heart_upload.dt$oldpeak)
heart_upload.dt <- subset(heart_upload.dt, heart_upload.dt$oldpeak > (Q1_oldpeak -1.5*IQR_oldpeak) & heart_upload.dt$oldpeak < (Q3_oldpeak + 1.5*IQR_oldpeak))
dim(heart_upload.dt)
boxplot(heart_upload.dt$oldpeak, data = heart_upload.dt) #check if outliers are removed

#replacing 0 and 1 with female and male respectively
heart_upload.dt[sex==0,sex := 'Female'][sex==1,sex := 'Male']
View(heart_upload.dt)

heart_upload.dt[fbs==0,fbs := 'False'][fbs==1,fbs := 'True']
View(heart_upload.dt)

heart_upload.dt[cp==0,cp := 'typical'][cp==1,cp := 'atypical'][cp==2,cp := 'non-anginal'][cp==3,cp := 'asymptomatic']

heart_upload.dt[slope==0,slope := 'upsloping'][slope==1,slope := 'flat'][slope==2,slope := 'downsloping']

heart_upload.dt[exang==0,exang := 'Yes'][exang==1,exang := 'No']

heart_upload.dt[thal==0,thal := 'normal'][thal==1, thal := 'fixed defect'][thal==2,thal := 'reversable defect']

heart_upload.dt[condition==0,condition := 'no disease'][condition==1,condition := 'disease']

View(heart_upload.dt)
