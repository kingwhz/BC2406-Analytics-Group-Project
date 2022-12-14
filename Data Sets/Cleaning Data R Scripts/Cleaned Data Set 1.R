library(data.table)
setwd("C:/Users/Rachel Goh/OneDrive - Nanyang Technological University/NTU documents/Y2S1/BC2406_Biz Analytics 1/Project")

heart2020.dt <- fread("heart_2020_cleaned (Data Set 1).csv", na.strings = c("NA", "missing", "N/A", -99, "", "m", "M", "na", "."))
View(heart2020.dt)
summary(heart2020.dt)
sapply(heart2020.dt, class) #check for class

cols <- c("HeartDisease", "Smoking", "AlcoholDrinking", "Stroke", "DiffWalking", "Sex", "AgeCategory", "Race", "Diabetic", "PhysicalActivity", "GenHealth", "Asthma", "KidneyDisease", "SkinCancer")
heart2020.dt[, (cols):= lapply(.SD, factor), .SDcols = cols] #factor the columns that need to become categorical

sapply(heart2020.dt, class)

sum(is.na(heart2020.dt)) #check number of na values
summary(heart2020.dt)
summary(heart2020.dt$AgeCategory) #check for what is included in "others"
summary(heart2020.dt$BMI) #check for any outliers

boxplot(heart2020.dt$BMI, data=heart2020.dt) #form boxplot to remove outliers

out = boxplot(heart2020.dt$BMI)$out #identify values that are outliers
out_index = which(heart2020.dt$BMI %in% c(out)) #identify the rows that contain outliers

out.dt <- heart2020.dt[out_index,] #create another table to see the outliers values
View(out.dt)
summary(out.dt)

#removing of outliers
Q1 <- quantile(heart2020.dt$BMI, .25)
Q3 <- quantile(heart2020.dt$BMI, .75)
IQR <- IQR(heart2020.dt$BMI)
heart2020.dt <- subset(heart2020.dt, heart2020.dt$BMI > (Q1 -1.5*IQR) & heart2020.dt$BMI < (Q3 + 1.5*IQR))
dim(heart2020.dt)




